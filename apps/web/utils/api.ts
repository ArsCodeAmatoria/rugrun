// API base URL
const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001/api';

// Types
export interface Wallet {
  id: string;
  address: string;
  clue: string;
  balance?: number;
  createdAt: string;
  updatedAt: string;
}

export interface Attempt {
  id: string;
  walletAddress: string;
  userAddress: string;
  timestamp: number;
  attempt: string;
  status: 'failed' | 'success' | 'pending';
}

export interface LeaderboardEntry {
  id: string;
  address: string;
  attempts: number;
  lastAttempt: number;
  successfulAttempts: number;
}

export interface Stats {
  totalAttempts: number;
  uniqueHunters: number;
  successRate: number;
}

// API methods
export const api = {
  // Wallet methods
  wallets: {
    getAll: async (): Promise<Wallet[]> => {
      const response = await fetch(`${API_URL}/wallets`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getByAddress: async (address: string): Promise<Wallet> => {
      const response = await fetch(`${API_URL}/wallets/${address}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    unlock: async (address: string, secretPhrase: string): Promise<{ success: boolean; message: string }> => {
      const response = await fetch(`${API_URL}/wallets/${address}/unlock`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ secretPhrase }),
      });
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
  },
  
  // Attempt methods
  attempts: {
    getAll: async (): Promise<Attempt[]> => {
      const response = await fetch(`${API_URL}/attempts`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getByWallet: async (walletAddress: string): Promise<Attempt[]> => {
      const response = await fetch(`${API_URL}/attempts?wallet=${walletAddress}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getByUser: async (userAddress: string): Promise<Attempt[]> => {
      const response = await fetch(`${API_URL}/attempts?user=${userAddress}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    create: async (data: { walletAddress: string; userAddress: string; attempt: string }): Promise<Attempt> => {
      const response = await fetch(`${API_URL}/attempts`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
  },
  
  // Leaderboard methods
  leaderboard: {
    getAll: async (sortBy: 'attempts' | 'lastAttempt' = 'attempts'): Promise<LeaderboardEntry[]> => {
      const response = await fetch(`${API_URL}/leaderboard?sortBy=${sortBy}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getTopHunters: async (limit: number = 10): Promise<LeaderboardEntry[]> => {
      const response = await fetch(`${API_URL}/leaderboard?limit=${limit}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getUserRank: async (address: string): Promise<{ rank: number; entry: LeaderboardEntry | null }> => {
      const response = await fetch(`${API_URL}/leaderboard/user/${address}`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
    
    getStats: async (): Promise<Stats> => {
      const response = await fetch(`${API_URL}/leaderboard/stats`);
      
      if (!response.ok) {
        throw new Error(`API error: ${response.statusText}`);
      }
      
      return response.json();
    },
  },
}; 