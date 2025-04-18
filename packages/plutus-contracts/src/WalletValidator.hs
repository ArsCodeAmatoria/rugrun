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

module WalletValidator
  ( walletScript
  , walletValidatorHash
  , serialisedScript
  , WalletDatum(..)
  , WalletRedeemer(..)
  , walletValidator
  , validateSecret
  ) where

import           Cardano.Api                          as API
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Aeson                           (FromJSON, ToJSON)
import           GHC.Generics                         (Generic)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.V2.Ledger.Api                 as Plutus
import qualified Plutus.V2.Ledger.Contexts            as Contexts
import qualified PlutusTx
import           PlutusTx.Prelude                     hiding (Semigroup (..),
                                                       unless)
import qualified Prelude                              as P

-- | Datum holds the SHA-256 hash of the secret phrase
newtype WalletDatum = WalletDatum
  { secretHash :: BuiltinByteString
  } deriving (P.Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''WalletDatum

-- | Redeemer holds the secret phrase for unlocking
newtype WalletRedeemer = WalletRedeemer
  { secret :: BuiltinByteString
  } deriving (P.Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''WalletRedeemer

-- | Validates if the provided secret matches the expected hash
{-# INLINABLE validateSecret #-}
validateSecret :: BuiltinByteString -> BuiltinByteString -> Bool
validateSecret expectedHash providedSecret = 
  sha2_256 providedSecret == expectedHash

-- | Validator function for a wallet
{-# INLINABLE walletValidator #-}
walletValidator :: WalletDatum -> WalletRedeemer -> Contexts.ScriptContext -> Bool
walletValidator datum redeemer _ctx = 
  validateSecret (secretHash datum) (secret redeemer)

-- | Compiled Plutus script
typedValidator :: Scripts.TypedValidator WalletValidator
typedValidator = Scripts.mkTypedValidator @WalletValidator
  $$(PlutusTx.compile [|| walletValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @WalletDatum @WalletRedeemer

-- | Script instance
data WalletValidator

walletScript :: Plutus.Script
walletScript = Plutus.unValidatorScript $ Scripts.validatorScript typedValidator

walletValidatorHash :: Plutus.ValidatorHash
walletValidatorHash = Scripts.validatorHash typedValidator

-- | Serialised script
serialisedScript :: SBS.ShortByteString
serialisedScript = SBS.toShort . LBS.toStrict . serialise $ walletScript 