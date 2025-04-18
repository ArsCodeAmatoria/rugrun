import { Request, Response, NextFunction } from 'express';
import { LeaderboardService } from '../services/leaderboardService';

const leaderboardService = new LeaderboardService();

export const getLeaderboard = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const sortBy = req.query.sortBy as 'attempts' | 'lastAttempt' || 'attempts';
    const limit = parseInt(req.query.limit as string) || undefined;
    
    let leaderboard;
    if (limit) {
      leaderboard = leaderboardService.getTopHunters(limit);
    } else {
      leaderboard = leaderboardService.getLeaderboard(sortBy);
    }
    
    res.status(200).json(leaderboard);
  } catch (error) {
    next(error);
  }
};

export const getUserRank = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const { address } = req.params;
    
    const rankInfo = leaderboardService.getUserRank(address);
    
    res.status(200).json(rankInfo);
  } catch (error) {
    next(error);
  }
};

export const getLeaderboardStats = async (req: Request, res: Response, next: NextFunction) => {
  try {
    const stats = leaderboardService.getStats();
    
    res.status(200).json(stats);
  } catch (error) {
    next(error);
  }
}; 