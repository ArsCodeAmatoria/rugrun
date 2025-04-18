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
  , ClueType (..)
  , Difficulty (..)
  , AttemptRecord (..)
  , GameConfig (..)
  , WalletMetadata (..)
  , LeaderboardEntry (..)
  , GameState (..)
  , createGameConfig
  , defaultGameConfig
  , emptyGameState
  , attemptSuccessful
  , attemptToLeaderboardEntry
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

-- | Types of clues
data ClueType = DirectClue | IndirectClue | MisdirectionClue | EncodedClue
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''ClueType [('DirectClue, 0), ('IndirectClue, 1), ('MisdirectionClue, 2), ('EncodedClue, 3)]

-- | Game difficulty levels
data Difficulty = Easy | Medium | Hard | Expert
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''Difficulty [('Easy, 0), ('Medium, 1), ('Hard, 2), ('Expert, 3)]

-- | Wallet categories
data WalletType = RealWallet | DecoyWallet
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''WalletType [('RealWallet, 0), ('DecoyWallet, 1)]

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

-- | Additional wallet metadata for frontend display
data WalletMetadata = WalletMetadata
  { metaWalletType  :: WalletType          -- Type of wallet (real or decoy)
  , metaClueType    :: ClueType            -- Type of clue provided
  , metaBalance     :: Integer             -- Balance in lovelace
  , metaTokenAmount :: Integer             -- Amount of game tokens
  , metaCreatedAt   :: POSIXTime           -- Time when wallet was created
  , metaDescription :: BuiltinByteString   -- Human-readable description
  , metaTags        :: [BuiltinByteString] -- Descriptive tags for the wallet
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''WalletMetadata [('WalletMetadata, 0)]

-- | Record of an unlock attempt
data AttemptRecord = AttemptRecord
  { attemptWalletAddr :: BuiltinByteString -- Address of the wallet
  , attemptPhrase     :: BuiltinByteString -- Phrase that was tried
  , attemptTimestamp  :: POSIXTime         -- When the attempt was made
  , attemptSuccess    :: Bool              -- Whether it was successful
  , attemptUserPkh    :: PubKeyHash        -- Who made the attempt
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''AttemptRecord [('AttemptRecord, 0)]

-- | Game configuration parameters
data GameConfig = GameConfig
  { gameTokenPolicyId :: BuiltinByteString -- Policy ID for game tokens
  , gameTokenName     :: BuiltinByteString -- Token name
  , gameTotalWallets  :: Integer           -- Number of wallets in the game
  , gameDecoyRatio    :: Integer           -- Percentage of wallets that are decoys
  , gameDifficulty    :: Difficulty        -- Game difficulty
  , gameRewardAmount  :: Integer           -- Base reward for successful unlock
  , gameMaxAttempts   :: Integer           -- Maximum number of attempts allowed
  , gameExpiration    :: POSIXTime         -- When the game expires
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''GameConfig [('GameConfig, 0)]

-- | Leaderboard entry
data LeaderboardEntry = LeaderboardEntry
  { leaderUserPkh      :: PubKeyHash        -- User public key hash
  , leaderUserName     :: BuiltinByteString -- Display name (if provided)
  , leaderSuccessCount :: Integer           -- Number of successful unlocks
  , leaderAttemptCount :: Integer           -- Total number of attempts
  , leaderLastSuccess  :: POSIXTime         -- Timestamp of last success
  , leaderScore        :: Integer           -- Score (based on success rate and speed)
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''LeaderboardEntry [('LeaderboardEntry, 0)]

-- | Current game state
data GameState = GameState
  { stateConfig          :: GameConfig          -- Game configuration
  , stateRealWalletFound :: Bool                -- Whether the real wallet has been found
  , stateAttempts        :: [AttemptRecord]     -- History of attempts
  , stateLeaderboard     :: [LeaderboardEntry]  -- Current leaderboard
  , stateStartTime       :: POSIXTime           -- When the game started
  , stateEndTime         :: POSIXTime           -- When the game ended (if ended)
  }
  deriving (Haskell.Show, Generic, JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''GameState [('GameState, 0)]

-- | Create a game configuration with default values
createGameConfig :: BuiltinByteString -> BuiltinByteString -> Difficulty -> POSIXTime -> GameConfig
createGameConfig policyId tokenName difficulty expiration = GameConfig
  { gameTokenPolicyId = policyId
  , gameTokenName     = tokenName
  , gameTotalWallets  = difficultyToWalletCount difficulty
  , gameDecoyRatio    = 90 -- 90% of wallets are decoys
  , gameDifficulty    = difficulty
  , gameRewardAmount  = difficultyToReward difficulty
  , gameMaxAttempts   = difficultyToMaxAttempts difficulty
  , gameExpiration    = expiration
  }

-- | Default game configuration
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { gameTokenPolicyId = emptyByteString
  , gameTokenName     = emptyByteString
  , gameTotalWallets  = 5
  , gameDecoyRatio    = 80
  , gameDifficulty    = Medium
  , gameRewardAmount  = 100000000 -- 100 ADA
  , gameMaxAttempts   = 10
  , gameExpiration    = 1924991999000 -- Far in the future
  }

-- | Create an empty game state
emptyGameState :: GameConfig -> POSIXTime -> GameState
emptyGameState config startTime = GameState
  { stateConfig          = config
  , stateRealWalletFound = False
  , stateAttempts        = []
  , stateLeaderboard     = []
  , stateStartTime       = startTime
  , stateEndTime         = 0 -- Not ended yet
  }

-- | Check if an attempt was successful
attemptSuccessful :: AttemptRecord -> Bool
attemptSuccessful = attemptSuccess

-- | Convert an attempt record to a leaderboard entry
attemptToLeaderboardEntry :: AttemptRecord -> LeaderboardEntry
attemptToLeaderboardEntry attempt = LeaderboardEntry
  { leaderUserPkh      = attemptUserPkh attempt
  , leaderUserName     = emptyByteString -- Will be filled in by off-chain code
  , leaderSuccessCount = if attemptSuccess attempt then 1 else 0
  , leaderAttemptCount = 1
  , leaderLastSuccess  = if attemptSuccess attempt then attemptTimestamp attempt else 0
  , leaderScore        = if attemptSuccess attempt then 100 else 0
  }

-- | Helper function to determine wallet count based on difficulty
difficultyToWalletCount :: Difficulty -> Integer
difficultyToWalletCount Easy   = 3
difficultyToWalletCount Medium = 5
difficultyToWalletCount Hard   = 8
difficultyToWalletCount Expert = 12

-- | Helper function to determine reward based on difficulty
difficultyToReward :: Difficulty -> Integer
difficultyToReward Easy   = 50000000  -- 50 ADA
difficultyToReward Medium = 100000000 -- 100 ADA
difficultyToReward Hard   = 250000000 -- 250 ADA
difficultyToReward Expert = 500000000 -- 500 ADA

-- | Helper function to determine max attempts based on difficulty
difficultyToMaxAttempts :: Difficulty -> Integer
difficultyToMaxAttempts Easy   = 20
difficultyToMaxAttempts Medium = 15
difficultyToMaxAttempts Hard   = 10
difficultyToMaxAttempts Expert = 5 