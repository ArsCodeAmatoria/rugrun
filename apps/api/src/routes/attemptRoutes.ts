import express from 'express';
import { getAttempts, createAttempt } from '../controllers/attemptController';

const router = express.Router();

// GET all attempts (with optional filtering)
router.get('/', getAttempts);

// POST create a new attempt
router.post('/', createAttempt);

export default router; 