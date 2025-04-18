import express from 'express';
import { getUserPoints } from '../controllers/userController';

const router = express.Router();

// GET user points
router.get('/points', getUserPoints);

export default router; 