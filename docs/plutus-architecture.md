# RugRun Plutus Smart Contract Architecture

This document provides a detailed overview of the Plutus smart contract architecture used in the RugRun game.

## Overview

RugRun uses Plutus, Cardano's smart contract platform, to implement a game where players must identify the real wallet among decoys and unlock it with the correct secret phrase. The smart contract system consists of two primary validators:

1. **RugWallet Validator** - The "real" wallet containing tokens
2. **DecoyWallet Validator** - Multiple decoy wallets designed to mislead players

## Contract Types and Data Model

### Core Data Types

The core data types are defined in `RugRun/Types.hs`:

```haskell
-- Status of the Rug wallet
data RugStatus = Unclaimed | Claimed

-- Operation types for decoy wallets
data DecoyOperation = NormalOperation | ReclaimTokens

-- Wallet types
data WalletType = RealWallet | DecoyWallet

-- Datum for the Rug Wallet
data RugDatum = RugDatum
  { rugSecretHash  :: BuiltinByteString  -- SHA-256 hash of the secret phrase
  , rugStatus      :: RugStatus          -- Current status (unclaimed/claimed)
  , rugCreatorPkh  :: PubKeyHash         -- Public key hash of the creator
  }

-- Redeemer for the Rug Wallet
data RugRedeemer = RugRedeemer
  { secretPhrase :: BuiltinByteString    -- Secret phrase that should hash to the stored hash
  }

-- Datum for the Decoy Wallet
data DecoyDatum = DecoyDatum
  { decoyClue        :: BuiltinByteString  -- Fake clue to mislead users
  , decoyBurnOnUse   :: Bool               -- Whether to burn tokens on interaction
  , decoyExpiration  :: POSIXTime          -- Time after which tokens can be reclaimed
  , decoyCreatorPkh  :: PubKeyHash         -- Public key hash of the creator
  }

-- Redeemer for the Decoy Wallet
data DecoyRedeemer = DecoyRedeemer
  { decoyInput    :: BuiltinByteString     -- Input value (will always fail)
  , decoyReclaimOp :: DecoyOperation       -- Operation (normal or reclaim)
  }
```

## Validator Logic

### RugWallet Validator

The RugWallet validator (`RugRun/Contract.hs`) implements the core game logic:

1. **Secret Verification**: Checks if the provided secret phrase matches the hashed value stored in the datum
2. **Status Checking**: Ensures the wallet has not already been claimed
3. **Tokenization**: Manages the tokens that represent the game prize

```haskell
-- Main validator logic for RugWallet
mkRugWalletValidator :: RugDatum -> RugRedeemer -> ScriptContext -> Bool
mkRugWalletValidator datum redeemer ctx =
  let 
    info = scriptContextTxInfo ctx
    
    -- Check if wallet is already claimed
    isUnclaimed = rugStatus datum == Unclaimed
    
    -- Extract the secret phrase from the redeemer
    providedSecret = secretPhrase redeemer
    
    -- Check if the hash matches
    hashMatches = verifySecret (rugSecretHash datum) providedSecret
    
    -- Only the correct hash unlocks the tokens
    validation = isUnclaimed && hashMatches
    
    -- For logging/events
    _ = if validation
        then emitEvent "rug_claimed" providedSecret
        else emitEvent "rug_attempt_failed" providedSecret
  in
    traceIfFalse "RugWallet: Already claimed or invalid secret" validation
```

### DecoyWallet Validator

The DecoyWallet validator is designed to always fail for normal operation, but allows the creator to reclaim tokens after an expiration date:

```haskell
-- Main validator logic for DecoyWallet
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
    
    -- Handle reclaim case (only valid if expired and signed by creator)
    validReclaim = isExpired && isReclaimAttempt && signedByCreator
  in
    if validReclaim 
    then True  -- Allow reclaiming tokens after expiration
    else if burnTokens 
         then traceError "Tokens burned due to interaction with decoy wallet"
         else traceError "Invalid attempt to access decoy wallet"
```

## Utility Functions

The utility functions (`RugRun/Utils.hs`) provide essential cryptographic and validation capabilities:

```haskell
-- Compute SHA-256 hash of input
sha256 :: BuiltinByteString -> BuiltinByteString
sha256 = Builtins.sha2_256

-- Verify if a provided secret phrase matches the expected hash
verifySecret :: BuiltinByteString -> BuiltinByteString -> Bool
verifySecret secretHash providedSecret = sha256 providedSecret == secretHash

-- Generate a unique game ID based on seed and timestamp
generateGameId :: BuiltinByteString -> POSIXTime -> BuiltinByteString
generateGameId seed timestamp = 
  let 
    timeBytes = Builtins.toBuiltin $ Builtins.indexByteString (Builtins.consByteString timestamp emptyByteString) 0
    combined = appendByteString seed timeBytes
  in 
    sha256 combined

-- Check if wallet has expired based on current time
validateExpiration :: POSIXTime -> POSIXTime -> Bool
validateExpiration currentTime expirationTime = 
  from expirationTime `contains` currentTime
```

## On-Chain / Off-Chain Integration

The contracts are designed to work with both on-chain validation and off-chain simulation:

### On-Chain (Real Blockchain)

When deployed to the Cardano blockchain:

1. The RugWallet validator is compiled to Plutus Core and deployed as a script
2. Multiple DecoyWallet validators are deployed with different datums (clues)
3. Tokens are locked in these script addresses
4. Players interact by submitting transactions with the appropriate redeemers

### Off-Chain (Simulation)

For development and testing without blockchain interaction:

1. The TypeScript implementation in `apps/web/utils/haskellSimulation.ts` mimics the Haskell contract logic
2. The game state is stored in browser localStorage
3. The same validation rules apply, but without requiring blockchain transactions

## Tokenomics

The RugRun game utilizes native tokens on Cardano:

1. **RugCoin ($RGRN)** - The main token used in the game
2. **Winner NFT** - A special token minted for the player who successfully unlocks the real wallet

Token minting is handled through a separate minting policy:

```haskell
-- Token policy ID and name for the game tokens
rugTokenId :: CurrencySymbol
rugTokenId = "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"

rugTokenName :: TokenName
rugTokenName = "RugCoin"

gameAssetClass :: AssetClass
gameAssetClass = AssetClass (rugTokenId, rugTokenName)
```

## Deployment Process

The contracts are deployed to the Cardano blockchain using the following process:

1. Compile the Plutus contracts using `cabal build`
2. Export the compiled contracts with `cabal run rugrun -- export`
3. Deploy the contracts to testnet/mainnet using `./scripts/deploy.sh`
4. Update the frontend configuration with the deployed contract addresses

## Security Considerations

The RugRun contracts incorporate several security measures:

1. **Secret Hashing**: Only the hash of the secret phrase is stored on-chain
2. **Creator Reclaim**: Allows creators to reclaim tokens from decoy wallets after expiration
3. **Validation Checks**: Multiple validation criteria before allowing token transfers
4. **Trace Logging**: Comprehensive error messages and event logs for troubleshooting

## Testing 

The contracts include a comprehensive test suite:

1. **Unit Tests**: Tests for individual utility functions
2. **Validator Tests**: Tests for the RugWallet and DecoyWallet validators
3. **Integration Tests**: Tests for the entire contract ecosystem

Run the tests with `cabal test`.

## Future Enhancements

Potential future enhancements to the contract system:

1. **Multi-phase Games**: Multiple stages with progressive difficulty
2. **Time-locked Clues**: Additional clues revealed over time
3. **Team Play**: Collaborative wallet unlocking mechanics
4. **Staking Mechanics**: Allow players to stake tokens for additional clues 