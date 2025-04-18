import { Request, Response, NextFunction } from 'express';
import { AttemptService } from '../services/attemptService';
import { ApiError } from '../utils/errorHandler';

const attemptService = new AttemptService();

export const getAttempts = async (req: Request, res: Response, next: NextFunction) => {
  try {
    // Check for query parameters
    const { wallet, user } = req.query;
    
    let attempts;
    if (wallet) {
      attempts = attemptService.getAttemptsByWallet(wallet as string);
    } else if (user) {
      attempts = attemptService.getAttemptsByUser(user as string);
    } else {
      attempts = attemptService.getAttempts();
    }
    
    res.status(200).json(attempts);
  } catch (error) {
    next(error);
  }
};

export const createAttempt = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { walletAddress, userAddress, attempt } = req.body;
    
    if (!walletAddress || !userAddress || !attempt) {
      throw new ApiError(400, 'Wallet address, user address, and attempt are required');
    }
    
    const newAttempt = attemptService.createAttempt({
      walletAddress,
      userAddress,
      attempt
    });
    
    res.status(201).json(newAttempt);
  } catch (error) {
    next(error);
  }
}; 