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

module Validators.WalletValidator
    ( SecretPhrase
    , UnlockAction (..)
    , walletValidator
    , walletValidatorScript
    , walletValidatorHash
    , writeWalletValidator
    , secretPhraseFromString
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
import           Prelude                  (FilePath, IO, Show (..), String)
import qualified Prelude                  as Haskell
import qualified Cardano.Crypto.Hash      as Crypto
import qualified Data.ByteString          as BS
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           GHC.Generics             (Generic)
import           Codec.Serialise          (serialise)

-- | The data type for our secret phrase
newtype SecretPhrase = SecretPhrase { unSecretPhrase :: BS.ByteString }
    deriving (Show, Generic, Haskell.Eq)

-- | Action to unlock the wallet
data UnlockAction = UnlockWallet
    deriving (Show, Generic, Haskell.Eq)

PlutusTx.makeIsDataIndexed ''SecretPhrase [('SecretPhrase, 0)]
PlutusTx.makeIsDataIndexed ''UnlockAction [('UnlockWallet, 0)]

-- | Convert a string to a SecretPhrase
secretPhraseFromString :: String -> SecretPhrase
secretPhraseFromString = SecretPhrase . TE.encodeUtf8 . T.pack

-- | Hash a secret phrase using SHA-256
hashSecretPhrase :: SecretPhrase -> BS.ByteString
hashSecretPhrase (SecretPhrase bs) = Crypto.hashToBytes (Crypto.hashWith Crypto.SHA256 bs)

{-# INLINABLE mkWalletValidator #-}
mkWalletValidator :: SecretPhrase -> UnlockAction -> V2.ScriptContext -> Bool
mkWalletValidator expectedSecret UnlockWallet ctx = 
    traceIfFalse "Secret phrase is incorrect!" correctSecret
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx
    
    -- Get the redeemer data, which should contain the secret phrase to check
    redeemer :: V2.Redeemer
    redeemer = V2.findOwnInput ctx >>= \input -> 
                case V2.txInRedeemer input of
                    Just r -> r
                    Nothing -> traceError "Redeemer not found"
    
    -- Convert the redeemer to a SecretPhrase
    providedSecret :: SecretPhrase
    providedSecret = case PlutusTx.fromBuiltinData (V2.getRedeemer redeemer) of
        Just s -> s
        Nothing -> traceError "Could not decode redeemer as SecretPhrase"
    
    -- Check if the hash of the provided secret matches the hash of the expected secret
    correctSecret :: Bool
    correctSecret = hashSecretPhrase providedSecret == hashSecretPhrase expectedSecret

-- | Create the wallet validator script
walletValidator :: SecretPhrase -> Scripts.TypedValidator UnlockAction
walletValidator secretPhrase = Scripts.mkTypedValidator @UnlockAction
    ($$(PlutusTx.compile [|| mkWalletValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode secretPhrase)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SecretPhrase @UnlockAction

-- | Get the validator script
walletValidatorScript :: SecretPhrase -> V2.Validator
walletValidatorScript = Scripts.validatorScript . walletValidator

-- | Get the validator hash
walletValidatorHash :: SecretPhrase -> V2.ValidatorHash
walletValidatorHash = Scripts.validatorHash . walletValidator

-- | Serialize the validator as CBOR
serialisedScript :: SecretPhrase -> SBS.ShortByteString
serialisedScript = SBS.toShort . LBS.toStrict . serialise . walletValidatorScript

-- | Create Plutus script from the serialized validator
apiWalletValidator :: SecretPhrase -> PlutusScript PlutusScriptV2
apiWalletValidator = PlutusScriptSerialised . serialisedScript

-- | Write the validator to a file
writeWalletValidator :: FilePath -> SecretPhrase -> IO ()
writeWalletValidator file secretPhrase = do
    result <- Shelley.writeFileTextEnvelope file Nothing $ apiWalletValidator secretPhrase
    case result of
        Left err -> Haskell.error $ Haskell.show err
        Right () -> Haskell.putStrLn $ "Wrote validator to: " ++ file 