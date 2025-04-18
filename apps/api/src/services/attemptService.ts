import { attempts, Attempt } from '../models/attemptModel';
import { wallets } from '../models/walletModel';
import { leaderboard } from '../models/leaderboardModel';
import { ApiError } from '../utils/errorHandler';
import { WalletService } from './walletService';
import { v4 as uuidv4 } from 'uuid';
import { logger } from '../utils/logger';

export class AttemptService {
  private walletService: WalletService;
  
  constructor() {
    this.walletService = new WalletService();
  }

  // Get all attempts
  getAttempts(): Attempt[] {
    return attempts;
  }
  
  // Get attempts for a specific wallet
  getAttemptsByWallet(walletAddress: string): Attempt[] {
    return attempts.filter(a => a.walletAddress === walletAddress);
  }
  
  // Get attempts by a specific user
  getAttemptsByUser(userAddress: string): Attempt[] {
    return attempts.filter(a => a.userAddress === userAddress);
  }
  
  // Create a new unlock attempt
  createAttempt(attemptData: {
    walletAddress: string;
    userAddress: string;
    attempt: string;
  }): Attempt {
    // First check if the wallet exists
    const wallet = wallets.find(w => w.address === attemptData.walletAddress);
    
    if (!wallet) {
      throw new ApiError(404, 'Wallet not found');
    }
    
    // Attempt to unlock the wallet
    const unlockResult = this.walletService.unlockWallet(
      attemptData.walletAddress,
      attemptData.attempt
    );
    
    // Create the attempt record
    const newAttempt: Attempt = {
      id: uuidv4(),
      walletAddress: attemptData.walletAddress,
      userAddress: attemptData.userAddress,
      timestamp: Date.now(),
      attempt: attemptData.attempt,
      status: unlockResult.success ? 'success' : 'failed'
    };
    
    attempts.push(newAttempt);
    
    // Update leaderboard entry for this user
    this.updateLeaderboard(attemptData.userAddress, unlockResult.success);
    
    logger.info(`New unlock attempt by ${attemptData.userAddress} on wallet ${attemptData.walletAddress} - Status: ${newAttempt.status}`);
    
    return newAttempt;
  }
  
  // Update the leaderboard after an attempt
  private updateLeaderboard(userAddress: string, isSuccess: boolean): void {
    // Find or create leaderboard entry for user
    let entry = leaderboard.find(entry => entry.address === userAddress);
    
    if (entry) {
      // Update existing entry
      entry.attempts += 1;
      entry.lastAttempt = Date.now();
      if (isSuccess) {
        entry.successfulAttempts += 1;
      }
    } else {
      // Create new entry
      const newEntry = {
        id: uuidv4(),
        address: userAddress,
        attempts: 1,
        lastAttempt: Date.now(),
        successfulAttempts: isSuccess ? 1 : 0
      };
      
      leaderboard.push(newEntry);
    }
  }
} 