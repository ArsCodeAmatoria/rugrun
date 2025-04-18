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
  , DecoyOperation (..)
  ) where

import           PlutusTx.Prelude
import qualified Prelude                     as Haskell
import           GHC.Generics                (Generic)
import qualified Data.Aeson                  as JSON
import qualified PlutusTx
import           Plutus.V1.Ledger.Api        (BuiltinByteString, PubKeyHash)
import           Plutus.V1.Ledger.Time       (POSIXTime)

-- | Status of the Rug wallet
data RugStatus = Unclaimed | Claimed
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugStatus [('Unclaimed, 0), ('Claimed, 1)]

-- | Operation types for decoy wallets
data DecoyOperation = NormalOperation | ReclaimTokens
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''DecoyOperation [('NormalOperation, 0), ('ReclaimTokens, 1)]

-- | Datum for the Rug Wallet
data RugDatum = RugDatum
  { rugSecretHash     :: BuiltinByteString -- ^ SHA-256 hash of the secret phrase
  , rugStatus         :: RugStatus         -- ^ Current status (unclaimed/claimed)
  , rugCreatorPkh     :: PubKeyHash        -- ^ Public key hash of the creator
  , rugExpiration     :: POSIXTime         -- ^ Expiration time after which the wallet cannot be claimed
  , rugDifficulty     :: Integer           -- ^ Difficulty level (1=easy, 2=medium, 3=hard)
  , rugGameId         :: BuiltinByteString -- ^ Unique ID for the game instance
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugDatum [('RugDatum, 0)]

-- | Redeemer for the Rug Wallet
data RugRedeemer = RugRedeemer
  { secretPhrase :: BuiltinByteString  -- ^ Secret phrase that should hash to the stored hash
  , claimFee     :: Integer            -- ^ Fee paid by claimer (percentage of 100)
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''RugRedeemer [('RugRedeemer, 0)]

-- | Datum for the Decoy Wallet
data DecoyDatum = DecoyDatum
  { decoyClue       :: BuiltinByteString  -- ^ Fake clue to mislead users
  , decoyBurnOnUse  :: Bool               -- ^ Whether to burn tokens on interaction
  , decoyExpiration :: POSIXTime          -- ^ Expiration time after which the tokens can be reclaimed
  , decoyCreatorPkh :: PubKeyHash         -- ^ Public key hash of the creator (for reclaiming)
  , decoyGameId     :: BuiltinByteString  -- ^ Unique ID for the game instance
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''DecoyDatum [('DecoyDatum, 0)]

-- | Redeemer for the Decoy Wallet
data DecoyRedeemer = DecoyRedeemer
  { decoyInput    :: BuiltinByteString   -- ^ Input value (will always fail for normal operation)
  , decoyReclaimOp :: DecoyOperation      -- ^ Operation type (normal or reclaim)
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''DecoyRedeemer [('DecoyRedeemer, 0)]

-- | Type of wallet
data WalletType = RealWallet | DecoyWallet
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''WalletType [('RealWallet, 0), ('DecoyWallet, 1)] 