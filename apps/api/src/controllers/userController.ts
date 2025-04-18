import { Request, Response, NextFunction } from 'express';
import { UserService } from '../services/userService';
import { WalletService } from '../services/walletService';
import { ApiError } from '../utils/errorHandler';
import { logger } from '../utils/logger';

const userService = new UserService();
const walletService = new WalletService();

// Get user points from the user model
export const getUserPoints = async (req: Request, res: Response, next: NextFunction) => {
  try {
    // In a real application, you would get the user from authentication
    // For now, we'll check if an address is provided in the query or use a default
    const { address } = req.query;
    
    if (address) {
      // Get points for the specific user
      const points = userService.getUserPoints(address as string);
      res.status(200).json({ points });
    } else {
      // No address provided, return sum of all wallet balances as before
      const wallets = walletService.getWallets();
      const points = wallets.reduce((total, wallet) => {
        return total + (wallet.balance || 0);
      }, 0);
      
      res.status(200).json({ points });
    }
  } catch (error) {
    next(error);
  }
};

// Get user profile
export const getUserProfile = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { address } = req.params;
    
    const user = userService.getUserByAddress(address);
    
    if (!user) {
      throw new ApiError(404, 'User not found');
    }
    
    res.status(200).json(user);
  } catch (error) {
    next(error);
  }
};

// Create or update user
export const createOrUpdateUser = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const userData = req.body;
    
    // Ensure address is provided
    if (!userData.address) {
      throw new ApiError(400, 'Wallet address is required');
    }
    
    const user = userService.createOrUpdateUser(userData);
    
    // Return only safe user data
    const { email, ...safeUser } = user;
    res.status(200).json(safeUser);
  } catch (error) {
    next(error);
  }
};

// Connect wallet to user
export const connectWallet = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { userAddress, walletAddress } = req.body;
    
    if (!userAddress || !walletAddress) {
      throw new ApiError(400, 'Both user address and wallet address are required');
    }
    
    // Check if wallet exists
    const wallet = walletService.getWalletByAddress(walletAddress);
    
    if (!wallet) {
      throw new ApiError(404, 'Wallet not found');
    }
    
    // Add wallet to user
    const updatedUser = userService.addWalletToUser(userAddress, walletAddress);
    
    if (!updatedUser) {
      throw new ApiError(404, 'User not found');
    }
    
    // Return only safe user data
    const { email, ...safeUser } = updatedUser;
    res.status(200).json({
      message: 'Wallet connected successfully',
      user: safeUser
    });
  } catch (error) {
    next(error);
  }
};

// Add points to user
export const addPoints = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { address } = req.params;
    const { points } = req.body;
    
    if (!points || isNaN(points) || points <= 0) {
      throw new ApiError(400, 'Valid points amount is required');
    }
    
    const updatedUser = userService.addPointsToUser(address, Number(points));
    
    if (!updatedUser) {
      throw new ApiError(404, 'User not found');
    }
    
    // Return only safe user data
    const { email, ...safeUser } = updatedUser;
    res.status(200).json({
      message: `${points} points added successfully`,
      user: safeUser
    });
  } catch (error) {
    next(error);
  }
}; 