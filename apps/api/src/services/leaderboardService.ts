import { leaderboard, LeaderboardEntry } from '../models/leaderboardModel';
import { logger } from '../utils/logger';

export class LeaderboardService {
  // Get leaderboard sorted by attempts (descending)
  getLeaderboard(sortBy: 'attempts' | 'lastAttempt' = 'attempts'): LeaderboardEntry[] {
    return [...leaderboard].sort((a, b) => {
      if (sortBy === 'attempts') {
        return b.attempts - a.attempts;
      } else {
        return b.lastAttempt - a.lastAttempt;
      }
    });
  }
  
  // Get top hunters (limited by count)
  getTopHunters(count: number = 10): LeaderboardEntry[] {
    return this.getLeaderboard().slice(0, count);
  }
  
  // Get a user's position on the leaderboard
  getUserRank(userAddress: string): { rank: number; entry: LeaderboardEntry | null } {
    const sortedLeaderboard = this.getLeaderboard();
    const index = sortedLeaderboard.findIndex(entry => entry.address === userAddress);
    
    if (index === -1) {
      return { rank: -1, entry: null };
    }
    
    return { 
      rank: index + 1, 
      entry: sortedLeaderboard[index] 
    };
  }
  
  // Get leaderboard statistics
  getStats(): {
    totalAttempts: number;
    uniqueHunters: number;
    successRate: number;
  } {
    const totalAttempts = leaderboard.reduce((sum, entry) => sum + entry.attempts, 0);
    const totalSuccesses = leaderboard.reduce((sum, entry) => sum + entry.successfulAttempts, 0);
    
    return {
      totalAttempts,
      uniqueHunters: leaderboard.length,
      successRate: totalAttempts > 0 ? (totalSuccesses / totalAttempts) * 100 : 0
    };
  }
} 