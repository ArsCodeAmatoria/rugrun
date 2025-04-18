{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module RugRun.Contract
  ( rugWalletValidator
  , decoyWalletValidator
  , rugWalletScript
  , decoyWalletScript
  , rugWalletHash
  , decoyWalletHash
  , extractRugSecret
  , validateClaimPayment
  ) where

import qualified Prelude                     as Haskell
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Address    (Address)
import           Plutus.V1.Ledger.Value      (Value, assetClassValueOf)
import           Plutus.V1.Ledger.Contexts   (ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo, findOwnInput, 
                                             findDatum, txSignedBy, txInfoValidRange, spendsOutput)
import           Plutus.V1.Ledger.Api        (ValidatorHash, Validator, PubKeyHash, TokenName, CurrencySymbol, AssetClass(..))
import           Plutus.V1.Ledger.Interval   (contains, from)
import           Plutus.V1.Ledger.Time       (POSIXTime)
import qualified PlutusTx
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import           Plutus.Script.Utils.V2.Scripts (validatorHash)

import           RugRun.Types
import           RugRun.Utils                (verifySecret, emitEvent, sha256)

-- Token policy ID and name for the game tokens
{-# INLINABLE rugTokenId #-}
rugTokenId :: CurrencySymbol
rugTokenId = "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2" -- Replace with actual token policy ID

{-# INLINABLE rugTokenName #-}
rugTokenName :: TokenName
rugTokenName = "RugCoin" -- Replace with actual token name

{-# INLINABLE gameAssetClass #-}
gameAssetClass :: AssetClass
gameAssetClass = AssetClass (rugTokenId, rugTokenName)

-- | Extract and validate the secret phrase provided in a claim attempt
{-# INLINABLE extractRugSecret #-}
extractRugSecret :: RugRedeemer -> BuiltinByteString
extractRugSecret = secretPhrase

-- | Validate that the proper payment is made when claiming a wallet
{-# INLINABLE validateClaimPayment #-}
validateClaimPayment :: PubKeyHash -> Value -> ScriptContext -> Bool
validateClaimPayment creatorPkh outputValue ctx = 
  let
    info = scriptContextTxInfo ctx
    creatorGetsValue = valuePaidTo info creatorPkh
    -- Ensure that the creator receives at least 80% of the value when claimed
    minimumPayment = scale 80 100 outputValue
  in
    assetClassValueOf creatorGetsValue gameAssetClass >= assetClassValueOf minimumPayment gameAssetClass

-- | Main validator for the rug wallet
{-# INLINABLE mkRugWalletValidator #-}
mkRugWalletValidator :: RugDatum -> RugRedeemer -> ScriptContext -> Bool
mkRugWalletValidator datum redeemer ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Extract the provided secret phrase
    providedSecret = extractRugSecret redeemer
    
    -- The expected hash from the datum
    expectedHash = rugSecretHash datum
    
    -- The current status of the rug wallet
    currentStatus = rugStatus datum
    
    -- The wallet creator's public key hash
    creatorPkh = rugCreatorPkh datum
    
    -- Time constraints
    validTimeRange = txInfoValidRange info
    expiration = rugExpiration datum
    notExpired = not (from expiration `contains` validTimeRange)
    
    -- Verify wallet conditions
    notYetClaimed = currentStatus == Unclaimed
    secretMatches = verifySecret expectedHash providedSecret
    
    -- Handle output value and fees
    ownInput = findOwnInput ctx
    outputValue = case ownInput of
                    Just inp -> valueSpent inp
                    Nothing  -> traceError "Input not found"
    
    -- Validate that the creator receives their payment
    validPayment = validateClaimPayment creatorPkh outputValue ctx
    
    -- Emit event for successful claim
    _ = emitEvent "rug_pulled" providedSecret
    
  in
    traceIfFalse "Wallet already claimed" notYetClaimed &&
    traceIfFalse "Wallet has expired" notExpired &&
    traceIfFalse "Invalid secret phrase" secretMatches &&
    traceIfFalse "Creator payment insufficient" validPayment

-- | Validator for decoy wallets
{-# INLINABLE mkDecoyWalletValidator #-}
mkDecoyWalletValidator :: DecoyDatum -> DecoyRedeemer -> ScriptContext -> Bool
mkDecoyWalletValidator datum redeemer ctx =
  let
    info = scriptContextTxInfo ctx
    
    -- Time constraints
    validTimeRange = txInfoValidRange info
    expiration = decoyExpiration datum
    
    -- Check if wallet expired (if expired, tokens can be reclaimed)
    isExpired = from expiration `contains` validTimeRange
    
    -- If this is a reclaim operation (only allowed after expiration)
    isReclaimAttempt = decoyReclaimOp redeemer == ReclaimTokens
    
    -- Verify it's signed by the creator if it's a reclaim
    creatorPkh = decoyCreatorPkh datum
    signedByCreator = txSignedBy info creatorPkh
    
    -- If attempting to interact normally (which always fails unless expired)
    burnTokens = decoyBurnOnUse datum && not isReclaimAttempt
    
    -- Define custom error messages
    burnError = traceError "Tokens burned due to interaction with decoy wallet"
    invalidError = traceError "Invalid attempt to access decoy wallet"
    
    -- Handle reclaim case (only valid if expired and signed by creator)
    validReclaim = isExpired && isReclaimAttempt && signedByCreator
    
  in
    if validReclaim 
    then True  -- Allow reclaiming tokens after expiration
    else if burnTokens 
         then burnError  -- Burn tokens on regular interaction
         else invalidError  -- Regular interaction always fails

-- Types for typed validators
data RugWallet
data DecoyWallet

-- Create validator instances
rugWalletInstance :: Scripts.TypedValidator RugWallet
rugWalletInstance = Scripts.mkTypedValidator @RugWallet
    $$(PlutusTx.compile [|| mkRugWalletValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RugDatum @RugRedeemer

decoyWalletInstance :: Scripts.TypedValidator DecoyWallet
decoyWalletInstance = Scripts.mkTypedValidator @DecoyWallet
    $$(PlutusTx.compile [|| mkDecoyWalletValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DecoyDatum @DecoyRedeemer

-- Script and hash exports
rugWalletValidator :: Validator
rugWalletValidator = Scripts.validatorScript rugWalletInstance

decoyWalletValidator :: Validator
decoyWalletValidator = Scripts.validatorScript decoyWalletInstance

rugWalletScript :: Haskell.String
rugWalletScript = Haskell.show $ Scripts.validatorScript rugWalletInstance

decoyWalletScript :: Haskell.String
decoyWalletScript = Haskell.show $ Scripts.validatorScript decoyWalletInstance

rugWalletHash :: ValidatorHash
rugWalletHash = validatorHash rugWalletValidator

decoyWalletHash :: ValidatorHash
decoyWalletHash = validatorHash decoyWalletValidator

-- | Helper function to get value spent in an input (placeholder implementation)
{-# INLINABLE valueSpent #-}
valueSpent :: PlutusTx.Prelude.BuiltinData -> Value
valueSpent _ = PlutusTx.Prelude.error () -- Replace with actual implementation when available 