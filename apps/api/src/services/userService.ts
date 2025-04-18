import { users, User } from '../models/userModel';
import { ApiError } from '../utils/errorHandler';
import { v4 as uuidv4 } from 'uuid';
import { logger } from '../utils/logger';

export class UserService {
  // Get all users (without sensitive info)
  getUsers(): Omit<User, 'email'>[] {
    return users.map(user => {
      const { email, ...safeUser } = user;
      return safeUser;
    });
  }

  // Get a user by address
  getUserByAddress(address: string): Omit<User, 'email'> | null {
    const user = users.find(u => u.address === address);
    
    if (!user) {
      return null;
    }
    
    const { email, ...safeUser } = user;
    return safeUser;
  }

  // Get a user by id
  getUserById(id: string): Omit<User, 'email'> | null {
    const user = users.find(u => u.id === id);
    
    if (!user) {
      return null;
    }
    
    const { email, ...safeUser } = user;
    return safeUser;
  }

  // Create or update a user
  createOrUpdateUser(userData: Partial<User> & { address: string }): User {
    const existingUserIndex = users.findIndex(u => u.address === userData.address);
    
    if (existingUserIndex >= 0) {
      // Update existing user
      const updatedUser = {
        ...users[existingUserIndex],
        ...userData,
        updatedAt: new Date()
      };
      
      users[existingUserIndex] = updatedUser;
      logger.info(`Updated user with address ${userData.address}`);
      
      return updatedUser;
    } else {
      // Create new user
      const newUser: User = {
        id: uuidv4(),
        address: userData.address,
        points: userData.points || 0,
        wallets: userData.wallets || [],
        username: userData.username,
        email: userData.email,
        createdAt: new Date(),
        updatedAt: new Date()
      };
      
      users.push(newUser);
      logger.info(`New user created with address ${userData.address}`);
      
      return newUser;
    }
  }

  // Add points to a user
  addPointsToUser(address: string, points: number): User | null {
    const userIndex = users.findIndex(u => u.address === address);
    
    if (userIndex === -1) {
      return null;
    }
    
    users[userIndex].points += points;
    users[userIndex].updatedAt = new Date();
    
    logger.info(`Added ${points} points to user with address ${address}`);
    
    return users[userIndex];
  }

  // Register a wallet to a user
  addWalletToUser(userAddress: string, walletAddress: string): User | null {
    const userIndex = users.findIndex(u => u.address === userAddress);
    
    if (userIndex === -1) {
      return null;
    }
    
    if (!users[userIndex].wallets.includes(walletAddress)) {
      users[userIndex].wallets.push(walletAddress);
      users[userIndex].updatedAt = new Date();
      
      logger.info(`Wallet ${walletAddress} added to user ${userAddress}`);
    }
    
    return users[userIndex];
  }

  // Get user points
  getUserPoints(address: string): number {
    const user = users.find(u => u.address === address);
    
    if (!user) {
      return 0;
    }
    
    return user.points;
  }
} 