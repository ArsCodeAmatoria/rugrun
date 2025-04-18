{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module RugRun.Validators
    ( rugWalletValidator
    , decoyWalletValidator
    , writeRugWalletValidator
    , writeDecoyWalletValidator
    , rugWalletValidatorScript
    , decoyWalletValidatorScript
    , rugWalletAddress
    , decoyWalletAddress
    , validateSecret
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV2)
import qualified Cardano.Api.Shelley      as Shelley
import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString.Lazy     as LBS
import qualified Plutus.V2.Ledger.Api     as V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Value   as Value
import qualified Plutus.V2.Ledger.Contexts as Contexts
import qualified Plutus.V2.Ledger.Address as Address
import           Prelude                  (FilePath, IO, Show (..), String)
import qualified Prelude                  as Haskell
import qualified Cardano.Crypto.Hash      as Crypto
import qualified Data.ByteString          as BS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           GHC.Generics             (Generic)
import           Codec.Serialise          (serialise)
import           RugRun.Types

-- | Hash a bytestring using SHA-256
hashSecretPhrase :: BuiltinByteString -> BuiltinByteString
hashSecretPhrase bs = sha2_256 bs

-- | Validate if the provided secret matches the expected hash
{-# INLINABLE validateSecret #-}
validateSecret :: BuiltinByteString -> BuiltinByteString -> Bool
validateSecret expectedHash providedSecret = 
  sha2_256 providedSecret == expectedHash

{-# INLINABLE mkRugWalletValidator #-}
mkRugWalletValidator :: RugDatum -> RugRedeemer -> V2.ScriptContext -> Bool
mkRugWalletValidator datum redeemer ctx = 
    traceIfFalse "Wallet already claimed" notYetClaimed &&
    traceIfFalse "Secret phrase is incorrect" correctSecret
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx
    
    -- Check if the wallet is unclaimed
    notYetClaimed :: Bool
    notYetClaimed = rugStatus datum == Unclaimed
    
    -- Check if provided secret hash matches expected hash
    correctSecret :: Bool
    correctSecret = validateSecret (rugSecretHash datum) (secretPhrase redeemer)

{-# INLINABLE mkDecoyWalletValidator #-}
mkDecoyWalletValidator :: DecoyDatum -> DecoyRedeemer -> V2.ScriptContext -> Bool
mkDecoyWalletValidator datum redeemer ctx
    | decoyReclaimOp redeemer == ReclaimTokens = 
        traceIfFalse "Only creator can reclaim tokens" isCreator &&
        traceIfFalse "Tokens cannot be reclaimed before expiration" isExpired
    | otherwise = 
        trace "This is a decoy wallet. Nice try!" False
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx
    
    -- Check if caller is the creator
    isCreator :: Bool
    isCreator = V2.txSignedBy info (decoyCreatorPkh datum)
    
    -- Check if wallet has expired
    isExpired :: Bool
    isExpired = V2.txInfoValidRange info `V2.contains` decoyExpiration datum

-- | Compile the Rug Wallet validator
rugWalletValidator :: Scripts.TypedValidator RugWallet
rugWalletValidator = Scripts.mkTypedValidator @RugWallet
    $$(PlutusTx.compile [|| mkRugWalletValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RugDatum @RugRedeemer

data RugWallet
data DecoyWallet

-- | Compile the Decoy Wallet validator
decoyWalletValidator :: Scripts.TypedValidator DecoyWallet
decoyWalletValidator = Scripts.mkTypedValidator @DecoyWallet
    $$(PlutusTx.compile [|| mkDecoyWalletValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DecoyDatum @DecoyRedeemer

-- | Script for Rug Wallet
rugWalletValidatorScript :: V2.Validator
rugWalletValidatorScript = Scripts.validatorScript rugWalletValidator

-- | Script for Decoy Wallet
decoyWalletValidatorScript :: V2.Validator
decoyWalletValidatorScript = Scripts.validatorScript decoyWalletValidator

-- | Hash of the rug wallet validator
rugWalletValidatorHash :: V2.ValidatorHash
rugWalletValidatorHash = Scripts.validatorHash rugWalletValidator

-- | Hash of the decoy wallet validator
decoyWalletValidatorHash :: V2.ValidatorHash
decoyWalletValidatorHash = Scripts.validatorHash decoyWalletValidator

-- | Address of the rug wallet script
rugWalletAddress :: V2.Address
rugWalletAddress = Address.scriptHashAddress rugWalletValidatorHash

-- | Address of the decoy wallet script
decoyWalletAddress :: V2.Address
decoyWalletAddress = Address.scriptHashAddress decoyWalletValidatorHash

-- | Serialize the rug wallet validator as CBOR
serialisedRugScript :: SBS.ShortByteString
serialisedRugScript = SBS.toShort . LBS.toStrict . serialise $ rugWalletValidatorScript

-- | Serialize the decoy wallet validator as CBOR
serialisedDecoyScript :: SBS.ShortByteString
serialisedDecoyScript = SBS.toShort . LBS.toStrict . serialise $ decoyWalletValidatorScript

-- | Create Plutus script from the serialized rug validator
apiRugWalletValidator :: PlutusScript PlutusScriptV2
apiRugWalletValidator = PlutusScriptSerialised serialisedRugScript

-- | Create Plutus script from the serialized decoy validator
apiDecoyWalletValidator :: PlutusScript PlutusScriptV2
apiDecoyWalletValidator = PlutusScriptSerialised serialisedDecoyScript

-- | Write the rug wallet validator to a file
writeRugWalletValidator :: FilePath -> IO ()
writeRugWalletValidator file = do
    result <- Shelley.writeFileTextEnvelope file Nothing apiRugWalletValidator
    case result of
        Left err -> Haskell.error $ Haskell.show err
        Right () -> Haskell.putStrLn $ "Wrote rug wallet validator to: " ++ file

-- | Write the decoy wallet validator to a file
writeDecoyWalletValidator :: FilePath -> IO ()
writeDecoyWalletValidator file = do
    result <- Shelley.writeFileTextEnvelope file Nothing apiDecoyWalletValidator
    case result of
        Left err -> Haskell.error $ Haskell.show err
        Right () -> Haskell.putStrLn $ "Wrote decoy wallet validator to: " ++ file 