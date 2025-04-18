import { Request, Response, NextFunction } from 'express';
import { WalletService } from '../services/walletService';
import { ApiError } from '../utils/errorHandler';

const walletService = new WalletService();

// For now, we'll simulate getting points by summing up wallet balances
export const getUserPoints = async (req: Request, res: Response, next: NextFunction) => {
  try {
    // In a real application, you would authenticate the user and get their user ID
    // For now, we'll simulate this by using a dummy user ID or returning a random balance
    
    // Get all wallets (this includes the balance field)
    const wallets = walletService.getWallets();
    
    // Sum up the balances
    const points = wallets.reduce((total, wallet) => {
      return total + (wallet.balance || 0);
    }, 0);
    
    // Return the points
    res.status(200).json({ points });
  } catch (error) {
    next(error);
  }
}; 