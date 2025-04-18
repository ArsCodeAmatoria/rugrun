# ⚡ RugRun ($RGRN)

A decentralized Capture The Flag (CTF) game on the blockchain. Find the real wallet, unlock the funds, and win the pool.

## Overview

RugRun is a gamified smart contract experience where players must find the real wallet among many decoys. The game simulates a "rug pull" scenario, but inverts it by giving players a chance to "pull the rug" themselves by finding and unlocking the wallet containing all the tokens.

### Key Features

- One wallet holds 100% of all $RGRN tokens
- Multiple decoy wallets with misleading clues
- On-chain validation using SHA-256 hash verification
- Interactive hunting interface with real-time attempt tracking
- Leaderboard for most active participants
- Winner recognition with NFT badge

## Project Structure

- `apps/web`: Next.js frontend application
- `packages/plutus-contracts`: Plutus smart contracts for wallet locking/unlocking logic
- `packages/common`: Shared utilities and types for cross-package use

## Smart Contract Logic

The core of RugRun consists of two types of smart contracts:

1. **RugWallet**: Contains 100% of the token supply, locked by a SHA-256 hash of a secret phrase.
   - Uses Plutus Datum + Redeemer pattern
   - Verifies the provided secret phrase against the stored hash
   - Emits an on-chain event when successfully unlocked

2. **DecoyWallets**: Multiple wallets with misleading clues.
   - Always fail validation attempts
   - Some have auto-burn functionality when interacted with
   - Contain metadata clues that may or may not help find the real wallet

## Development

```bash
# Install dependencies
npm install

# Start Next.js frontend
npm run dev

# Build Plutus contracts (requires Cardano development environment)
cd packages/plutus-contracts
cabal build
cabal run rugrun export
```

## Frontend Pages

- **Landing Page** (`/`): Introduction to the game with feature highlights
- **Hunt Page** (`/hunt`): Main game interface with wallets and unlock attempts
- **Leaderboard** (`/leaderboard`): Rankings of most active hunters
- **Winner Page** (`/rugslayer`): Celebration page shown once the real wallet is unlocked

## The Game

One wallet holds all the $RGRN tokens. Many decoy wallets exist with false clues. Only by finding the real wallet and providing the correct unlock condition can you claim the entire pool.

Players must analyze clues, wallet behaviors, and on-chain data to determine which wallet is real. Once identified, they must figure out the secret phrase to unlock it.

## Technology Stack

- **Smart Contracts**: Plutus (Haskell)
- **Frontend**: Next.js (App Router), Tailwind CSS, shadcn/ui, Three.js
- **UI Components**: Lucide icons, Framer Motion
- **Visual Style**: Futuristic sci-fi theme with dark mode and pink accents
- **3D Effects**: Three.js starfield for immersive background

## Local Development

To run the project locally, you'll need:
- Node.js 16+ and npm
- For contract development: Cardano development environment (GHC, Cabal, Plutus dependencies)

## License

MIT 