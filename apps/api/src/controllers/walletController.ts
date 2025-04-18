import { Request, Response, NextFunction } from 'express';
import { WalletService } from '../services/walletService';
import { ApiError } from '../utils/errorHandler';

const walletService = new WalletService();

export const getWallets = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const wallets = walletService.getWallets();
    res.status(200).json(wallets);
  } catch (error) {
    next(error);
  }
};

export const getWalletByAddress = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { address } = req.params;
    
    const wallet = walletService.getWalletByAddress(address);
    
    if (!wallet) {
      throw new ApiError(404, 'Wallet not found');
    }
    
    res.status(200).json(wallet);
  } catch (error) {
    next(error);
  }
};

export const unlockWallet = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { address } = req.params;
    const { secretPhrase } = req.body;
    
    if (!secretPhrase) {
      throw new ApiError(400, 'Secret phrase is required');
    }
    
    const result = walletService.unlockWallet(address, secretPhrase);
    
    res.status(200).json(result);
  } catch (error) {
    next(error);
  }
};

// Admin only - This would normally have authentication middleware
export const addWallet = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const walletData = req.body;
    
    if (!walletData.address || !walletData.clue) {
      throw new ApiError(400, 'Address and clue are required');
    }
    
    const newWallet = walletService.addWallet(walletData);
    
    res.status(201).json(newWallet);
  } catch (error) {
    next(error);
  }
}; 