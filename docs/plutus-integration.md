# Plutus Smart Contract Integration Guide

This document explains how to integrate the compiled Plutus smart contracts with the RugRun web application.

## Overview

The RugRun application simulates Cardano smart contracts in the frontend but can also be configured to interact with real on-chain contracts. This guide covers both approaches.

## Contract Compilation

Before integration, you need to compile the Plutus smart contracts:

1. Follow the setup instructions in the README.md
2. Navigate to the contracts directory: `cd packages/plutus-contracts`
3. Run the build script: `./scripts/build.sh`

This will generate the following key files in the `output` directory:
- `contracts.json` - Contains all contract information including validator scripts and hashes
- `scripts/rugWallet.plutus` - The compiled Rug Wallet validator script
- `scripts/decoyWallet.plutus` - The compiled Decoy Wallet validator script

## Integration with Web Application

### 1. Using the Simulation Mode (Default)

By default, the web application uses a TypeScript simulation of the Plutus contracts:

- `apps/web/utils/haskellSimulation.ts` - Contains TypeScript equivalents of the Haskell functions
- `apps/web/services/GameService.ts` - Uses these simulated functions for the game logic

This approach allows users to play the game without blockchain interaction.

### 2. Connecting to Real Cardano Contracts

To use real Cardano contracts, follow these steps:

#### A. Copy the compiled contracts

```bash
mkdir -p apps/web/public/contracts
cp packages/plutus-contracts/output/contracts.json apps/web/public/contracts/
```

#### B. Update the CardanoBridge configuration

Edit `apps/web/utils/cardanoBridge.ts` to use the actual contract data:

```typescript
// Near the top of the file
import contractData from '../public/contracts/contracts.json';

// Update the contract address and validator hash constants
export const RUG_WALLET_ADDRESS = "addr_test1..."; // Your deployed contract address
export const RUG_WALLET_VALIDATOR_HASH = contractData.rugWalletHash;
```

#### C. Modify the GameService to use Cardano integration

Update `apps/web/services/GameService.ts` to switch between simulation and real blockchain:

```typescript
import { isCardanoEnabled, submitTransaction } from '../utils/cardanoBridge';

// Inside the attemptUnlock method
async attemptUnlock(walletAddress: string, secret: string): Promise<UnlockResult> {
  if (isCardanoEnabled()) {
    // Real blockchain logic
    return await this.submitCardanoTransaction(walletAddress, secret);
  } else {
    // Simulation logic (existing code)
    const wallet = this.wallets.find(w => w.address === walletAddress);
    if (!wallet) return { success: false, message: "Wallet not found" };
    // ...rest of existing code
  }
}

// Add new method for real blockchain interaction
private async submitCardanoTransaction(walletAddress: string, secret: string): Promise<UnlockResult> {
  try {
    const tx = await buildUnlockTransaction(walletAddress, secret);
    const txHash = await submitTransaction(tx);
    
    return { 
      success: true, 
      message: `Transaction submitted: ${txHash}`,
      transactionId: txHash
    };
  } catch (error) {
    console.error("Transaction failed:", error);
    return {
      success: false,
      message: error instanceof Error ? error.message : "Unknown error"
    };
  }
}
```

#### D. Add Cardano wallet connection

Implement wallet connection in `ConnectWalletButton.tsx`:

```typescript
import { connectWallet, getWallets } from '../utils/cardanoBridge';

// Add connect handler
const handleConnectWallet = async () => {
  try {
    const result = await connectWallet();
    if (result.success) {
      // Update app state with connected wallet
      onConnect(result.walletAddress);
    } else {
      setError(result.message);
    }
  } catch (error) {
    setError("Failed to connect wallet");
    console.error(error);
  }
};
```

### 3. Testing the Integration

1. Start by testing in simulation mode (default)
2. To test with real contracts:
   - Deploy the contracts to Cardano testnet using the deployment script
   - Configure environment variables: `NEXT_PUBLIC_USE_CARDANO=true`
   - Connect a supported Cardano wallet (Nami, Eternl, etc.)
   - The app will use real blockchain transactions

## Deployment Considerations

When deploying contracts to mainnet:

1. **Security audits**: Have the contracts professionally audited
2. **Gas optimization**: Ensure contracts are optimized for minimum fees
3. **Testing**: Thoroughly test on testnet before mainnet deployment
4. **Front-end safeguards**: Implement confirmation dialogs and fee estimations

## Troubleshooting

Common issues and solutions:

- **Contract compilation errors**: Ensure you're using the correct GHC and Plutus versions
- **CborDecodeError**: Check that contract scripts are correctly serialized
- **Transaction failures**: Verify wallet has sufficient ADA for fees
- **Wallet connection issues**: Make sure wallet extension is installed and unlocked

## Additional Resources

- [Cardano Developer Portal](https://developers.cardano.org/)
- [Plutus Documentation](https://plutus.readthedocs.io/)
- [Plutus Playground](https://playground.plutus.iohkdev.io/)
- [Cardano Wallet API Documentation](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) 