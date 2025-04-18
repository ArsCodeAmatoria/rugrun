{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module RugRun.Types
  ( RugDatum (..)
  , RugRedeemer (..)
  , RugStatus (..)
  , DecoyDatum (..)
  , DecoyRedeemer (..)
  , WalletType (..)
  ) where

import           PlutusTx.Prelude
import qualified Prelude                     as Haskell
import           GHC.Generics                (Generic)
import qualified Data.Aeson                  as JSON
import qualified PlutusTx
import           Plutus.V1.Ledger.Api        (BuiltinByteString, PubKeyHash)

-- | Status of the Rug wallet
data RugStatus = Unclaimed | Claimed
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugStatus [('Unclaimed, 0), ('Claimed, 1)]

-- | Datum for the Rug Wallet
data RugDatum = RugDatum
  { rugSecretHash     :: BuiltinByteString -- ^ SHA-256 hash of the secret phrase
  , rugStatus         :: RugStatus         -- ^ Current status (unclaimed/claimed)
  , rugCreatorPkh     :: PubKeyHash        -- ^ Public key hash of the creator
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugDatum [('RugDatum, 0)]

-- | Redeemer for the Rug Wallet
data RugRedeemer = RugRedeemer
  { secretPhrase :: BuiltinByteString  -- ^ Secret phrase that should hash to the stored hash
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugRedeemer [('RugRedeemer, 0)]

-- | Datum for the Decoy Wallet
data DecoyDatum = DecoyDatum
  { decoyClue     :: BuiltinByteString  -- ^ Fake clue to mislead users
  , decoyBurnOnUse :: Bool              -- ^ Whether to burn tokens on interaction
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''DecoyDatum [('DecoyDatum, 0)]

-- | Redeemer for the Decoy Wallet
data DecoyRedeemer = DecoyRedeemer
  { decoyInput :: BuiltinByteString   -- ^ Input value (will always fail)
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''DecoyRedeemer [('DecoyRedeemer, 0)]

-- | Type of wallet
data WalletType = RealWallet | DecoyWallet
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''WalletType [('RealWallet, 0), ('DecoyWallet, 1)] 