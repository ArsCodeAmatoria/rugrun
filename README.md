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

## Smart Contract Architecture

The RugRun project includes Plutus smart contracts that power the wallet validation logic in the game. These contracts are located in the `packages/plutus-contracts` directory.

### Contract Structure

The Plutus contracts consist of two main validators:

1. **RugWallet Validator** - Represents the "real" wallet that contains tokens and can be unlocked with the correct secret phrase. This validator:
   - Verifies that the provided secret phrase matches the stored hash
   - Validates that the wallet hasn't already been claimed
   - Allows token withdrawal when validation passes

2. **DecoyWallet Validator** - Represents fake wallets that are designed to mislead users. This validator:
   - Always fails when users attempt to unlock it
   - May optionally burn tokens on interaction (simulation only)
   - Contains misleading clues

### Key Contract Files

- `RugRun/Contract.hs` - Main validator logic
- `RugRun/Types.hs` - Data types for the contract
- `RugRun/Utils.hs` - Helper functions for hashing and validation

### Simulation vs Real Deployment

The web application can operate in two modes:

1. **Simulation Mode** (Default) - Uses TypeScript implementations of the Plutus contract logic to simulate blockchain behavior without real blockchain interactions. This is in `apps/web/utils/haskellSimulation.ts`.

2. **Blockchain Mode** - Interacts with real deployed Plutus contracts on the Cardano blockchain. This requires additional configuration described in the `docs/plutus-integration.md` guide.

### Building and Deploying Contracts

To build the Plutus contracts:

```bash
cd packages/plutus-contracts
./scripts/build.sh
```

This will compile the contracts and generate the necessary outputs for deployment.

For detailed instructions on deploying to the Cardano testnet or mainnet, refer to the [Plutus Integration Guide](docs/plutus-integration.md).

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

## Cardano Smart Contract Development Setup

### Prerequisites

To develop and compile the Plutus smart contracts for RugRun, you'll need the following tools:

1. **GHC (Glasgow Haskell Compiler)** - We recommend using GHCup to manage Haskell tooling.
2. **Cabal** - Haskell build system
3. **Cardano Node and CLI** - For interacting with the Cardano blockchain
4. **Plutus development environment**

### Installation Steps

#### 1. Install GHCup, GHC, and Cabal

GHCup is the recommended way to install Haskell tools:

```bash
# macOS/Linux
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Windows
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

During installation, select:
- GHC version 8.10.7 (recommended for Plutus development)
- Cabal version 3.6.2.0 or newer
- HLS (Haskell Language Server) for IDE support

#### 2. Install Cardano Node and CLI

The easiest way is to use Docker:

```bash
# Pull the official Cardano node image
docker pull inputoutput/cardano-node:latest

# Create a configuration directory
mkdir -p $HOME/cardano-node/config

# Start a Cardano node connected to the testnet
docker run -d \
  --name cardano-node \
  -v $HOME/cardano-node:/data \
  -p 3001:3001 \
  inputoutput/cardano-node:latest run \
  --config /data/config/testnet-config.json \
  --topology /data/config/testnet-topology.json \
  --database-path /data/db
  
# Interact with the node using cardano-cli
docker exec cardano-node cardano-cli version
```

#### 3. Set Up Plutus Development Environment

We recommend using the Plutus Application Framework's Nix-based environment for development.

```bash
# Clone the plutus-apps repository
git clone https://github.com/input-output-hk/plutus-apps.git
cd plutus-apps

# Checkout a compatible tag (adjust based on current compatibility)
git checkout v2023-05-01

# Start the Nix shell (requires Nix to be installed)
nix-shell
```

Alternatively, you can use the Plutus Playground online or set up your own instance.

### Building the RugRun Plutus Contracts

Navigate to the `packages/plutus-contracts` directory and build the project using Cabal:

```bash
cd packages/plutus-contracts
cabal build
```

To export the compiled Plutus script:

```bash
cabal run rugrun -- export
```

This will generate the compiled Plutus script in the `output/scripts` directory.

### Development Tips

1. **Setting up VSCode for Haskell/Plutus development**:
   - Install the Haskell extension
   - Configure it to use the HLS provided by GHCup
   - Consider adding the Haskell Syntax Highlighting extension

2. **Testing on the Cardano testnet**:
   - Create a testnet wallet using cardano-cli
   - Get test ADA from the testnet faucet
   - Use cardano-cli to submit transactions with your scripts

3. **Debugging Plutus contracts**:
   - Use the `testSecretVerification` and `hashSecret` functions for testing
   - Inspect the script execution trace for errors
   - Validate script behavior in the Plutus Playground before deploying

### Useful Resources

- [Plutus Documentation](https://plutus.readthedocs.io/en/latest/)
- [Cardano Developer Portal](https://developers.cardano.org/)
- [Plutus Pioneer Program Materials](https://github.com/input-output-hk/plutus-pioneer-program)
- [Cardano Stack Exchange](https://cardano.stackexchange.com/)

## License

MIT 