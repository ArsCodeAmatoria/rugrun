import express from 'express';
import { getWallets, getWalletByAddress, unlockWallet, addWallet } from '../controllers/walletController';

const router = express.Router();

// GET all wallets
router.get('/', getWallets);

// GET a specific wallet by address
router.get('/:address', getWalletByAddress);

// POST attempt to unlock a wallet
router.post('/:address/unlock', unlockWallet);

// POST create a new wallet (admin only)
router.post('/', addWallet);

export default router; 