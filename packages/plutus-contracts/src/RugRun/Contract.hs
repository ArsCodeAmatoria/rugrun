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
  ) where

import qualified Prelude                     as Haskell
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Address    (Address)
import           Plutus.V1.Ledger.Value      (Value)
import           Plutus.V1.Ledger.Contexts   (ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo)
import           Plutus.V1.Ledger.Api        (ValidatorHash, Validator, PubKeyHash)
import qualified PlutusTx
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import           Plutus.Script.Utils.V2.Scripts (validatorHash)

import           RugRun.Types
import           RugRun.Utils                (verifySecret, emitEvent)

{-# INLINABLE mkRugWalletValidator #-}
mkRugWalletValidator :: RugDatum -> RugRedeemer -> ScriptContext -> Bool
mkRugWalletValidator datum redeemer ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The secret phrase provided in the redeemer
    providedSecret = secretPhrase redeemer
    
    -- The expected hash from the datum
    expectedHash = rugSecretHash datum
    
    -- The current status of the rug wallet
    currentStatus = rugStatus datum
    
    -- Verify if the wallet is already claimed
    notYetClaimed = currentStatus == Unclaimed
    
    -- Verify if the hash matches
    secretMatches = verifySecret expectedHash providedSecret
    
    -- Emit an event when the rug is pulled (successful claim)
    _ = emitEvent "rug_pulled" providedSecret
    
  in
    traceIfFalse "Wallet already claimed" notYetClaimed &&
    traceIfFalse "Invalid secret phrase" secretMatches

{-# INLINABLE mkDecoyWalletValidator #-}
mkDecoyWalletValidator :: DecoyDatum -> DecoyRedeemer -> ScriptContext -> Bool
mkDecoyWalletValidator datum redeemer _ =
  -- This validator always fails, ensuring funds are locked or burned
  let
    -- If burnOnUse is set, tokens will be sent to a burning address in the transaction
    _ = if decoyBurnOnUse datum
        then traceError "Tokens burned due to interaction"
        else traceError "Invalid attempt to access decoy wallet"
        
    -- The decoy clue is just for show, no actual validation logic
    fakeClue = decoyClue datum
    
    -- The input is ignored, will always fail
    _ = decoyInput redeemer
    
  in
    -- This validator always returns False, ensuring the transaction fails
    False

rugWalletValidator :: Validator
rugWalletValidator = Scripts.validatorScript rugWalletInstance

decoyWalletValidator :: Validator
decoyWalletValidator = Scripts.validatorScript decoyWalletInstance

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

-- Types for typed validators
data RugWallet
data DecoyWallet

-- Script and hash exports
rugWalletScript :: Haskell.String
rugWalletScript = Haskell.show $ Scripts.validatorScript rugWalletInstance

decoyWalletScript :: Haskell.String
decoyWalletScript = Haskell.show $ Scripts.validatorScript decoyWalletInstance

rugWalletHash :: ValidatorHash
rugWalletHash = validatorHash rugWalletValidator

decoyWalletHash :: ValidatorHash
decoyWalletHash = validatorHash decoyWalletValidator 