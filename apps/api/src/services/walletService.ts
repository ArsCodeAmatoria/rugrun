import { wallets, Wallet } from '../models/walletModel';
import { ApiError } from '../utils/errorHandler';
import { v4 as uuidv4 } from 'uuid';
import { logger } from '../utils/logger';

export class WalletService {
  // Get all wallets (without sensitive info)
  getWallets(): Omit<Wallet, 'secretPhrase' | 'isReal'>[] {
    return wallets.map(wallet => {
      const { secretPhrase, isReal, ...safeWallet } = wallet;
      return safeWallet;
    });
  }

  // Get a wallet by address (without sensitive info)
  getWalletByAddress(address: string): Omit<Wallet, 'secretPhrase' | 'isReal'> | null {
    const wallet = wallets.find(w => w.address === address);
    
    if (!wallet) {
      return null;
    }
    
    const { secretPhrase, isReal, ...safeWallet } = wallet;
    return safeWallet;
  }

  // Attempt to unlock a wallet with a secret phrase
  unlockWallet(address: string, secretPhrase: string): { success: boolean; message: string } {
    const wallet = wallets.find(w => w.address === address);
    
    if (!wallet) {
      throw new ApiError(404, 'Wallet not found');
    }
    
    // If it's not a real wallet, pretend to check but always fail
    if (!wallet.isReal) {
      logger.info(`Failed unlock attempt on decoy wallet ${address}`);
      return {
        success: false,
        message: 'Incorrect phrase. This wallet cannot be unlocked.',
      };
    }
    
    // For the real wallet, check the secret phrase
    if (wallet.secretPhrase === secretPhrase) {
      logger.info(`Successful unlock on real wallet ${address}`);
      return {
        success: true,
        message: 'Wallet unlocked successfully! You found the real token wallet.',
      };
    } else {
      logger.info(`Failed unlock attempt on real wallet ${address}`);
      return {
        success: false,
        message: 'Incorrect phrase. Try again.',
      };
    }
  }

  // Add a new wallet (admin only)
  addWallet(walletData: Omit<Wallet, 'id' | 'createdAt' | 'updatedAt'>): Wallet {
    const newWallet: Wallet = {
      id: uuidv4(),
      ...walletData,
      createdAt: new Date(),
      updatedAt: new Date()
    };
    
    wallets.push(newWallet);
    logger.info(`New wallet added with address ${newWallet.address}`);
    
    const { secretPhrase, isReal, ...safeWallet } = newWallet;
    return newWallet;
  }
} 