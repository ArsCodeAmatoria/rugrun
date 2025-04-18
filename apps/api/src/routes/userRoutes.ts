import express from 'express';
import { 
  getUserPoints, 
  getUserProfile, 
  createOrUpdateUser, 
  connectWallet, 
  addPoints 
} from '../controllers/userController';

const router = express.Router();

// GET user points
router.get('/points', getUserPoints);

// GET user profile
router.get('/:address', getUserProfile);

// POST create or update user
router.post('/', createOrUpdateUser);

// POST connect wallet to user
router.post('/connect-wallet', connectWallet);

// POST add points to user
router.post('/:address/points', addPoints);

export default router; 