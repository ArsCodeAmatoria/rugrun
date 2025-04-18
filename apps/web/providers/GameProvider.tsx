'use client'

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react'
import { GameService, AttemptRecord } from '@/services/GameService'

// Define the Game context shape
interface GameContextType {
  gameService: GameService;
  wallets: any[];
  userAttempts: AttemptRecord[];
  gameStatus: {
    totalWallets: number;
    realWalletFound: boolean;
    totalAttempts: number;
    successfulAttempts: number;
  };
  isLoading: boolean;
  attemptUnlock: (walletAddress: string, secretPhrase: string) => Promise<AttemptRecord>;
  resetGame: (walletCount?: number, totalBalance?: number, secret?: string) => void;
  refreshGameState: () => void;
}

// Create the context with a default value
const GameContext = createContext<GameContextType | undefined>(undefined)

// Provider props interface
interface GameProviderProps {
  children: ReactNode;
}

// Custom hook to use the Game context
export const useGame = () => {
  const context = useContext(GameContext)
  if (context === undefined) {
    throw new Error('useGame must be used within a GameProvider')
  }
  return context
}

// Define the provider component
export const GameProvider: React.FC<GameProviderProps> = ({ children }) => {
  const [gameService] = useState<GameService>(() => new GameService())
  const [wallets, setWallets] = useState<any[]>([])
  const [userAttempts, setUserAttempts] = useState<AttemptRecord[]>([])
  const [gameStatus, setGameStatus] = useState<GameContextType['gameStatus']>({
    totalWallets: 0,
    realWalletFound: false,
    totalAttempts: 0,
    successfulAttempts: 0
  })
  const [isLoading, setIsLoading] = useState(true)

  // Load initial game state
  const refreshGameState = () => {
    setWallets(gameService.getWallets())
    setUserAttempts(gameService.getUserAttempts())
    setGameStatus(gameService.getGameStatus())
    setIsLoading(false)
  }

  // Initialize on component mount
  useEffect(() => {
    refreshGameState()
  }, [])

  // Attempt to unlock a wallet
  const attemptUnlock = async (walletAddress: string, secretPhrase: string): Promise<AttemptRecord> => {
    const result = gameService.attemptWalletUnlock(walletAddress, secretPhrase)
    refreshGameState()
    return result
  }

  // Reset the game
  const resetGame = (walletCount?: number, totalBalance?: number, secret?: string) => {
    setIsLoading(true)
    gameService.initializeGame(walletCount, totalBalance, secret)
    refreshGameState()
  }

  // Create the context value
  const value: GameContextType = {
    gameService,
    wallets,
    userAttempts,
    gameStatus,
    isLoading,
    attemptUnlock,
    resetGame,
    refreshGameState
  }

  // Return the provider with the context value
  return (
    <GameContext.Provider value={value}>
      {children}
    </GameContext.Provider>
  )
} 