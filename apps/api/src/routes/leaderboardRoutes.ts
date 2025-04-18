import express from 'express';
import { getLeaderboard, getUserRank, getLeaderboardStats } from '../controllers/leaderboardController';

const router = express.Router();

// GET leaderboard (with optional sorting and limiting)
router.get('/', getLeaderboard);

// GET leaderboard stats
router.get('/stats', getLeaderboardStats);

// GET a user's rank
router.get('/user/:address', getUserRank);

export default router; 