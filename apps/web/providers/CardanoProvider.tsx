'use client';

import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import cardanoBridge, { CardanoWallet, CardanoNetwork, OnChainWallet } from '@/utils/cardanoBridge';

// Interface for Cardano context
interface CardanoContextType {
  isWalletAvailable: boolean;
  connectWallet: (walletName: string) => Promise<boolean>;
  connectedWallet: CardanoWallet | null;
  network: CardanoNetwork;
  walletAddresses: string[];
  loadGameWallets: (gameId: string) => Promise<void>;
  gameWallets: OnChainWallet[];
  isLoading: boolean;
  error: string | null;
  attemptUnlock: (walletAddress: string, secretPhrase: string) => Promise<{ success: boolean; txId?: string; message: string }>;
}

// Create the context with a default value
const CardanoContext = createContext<CardanoContextType | undefined>(undefined);

// Provider props interface
interface CardanoProviderProps {
  children: ReactNode;
}

// Custom hook to use the Cardano context
export const useCardano = () => {
  const context = useContext(CardanoContext);
  if (context === undefined) {
    throw new Error('useCardano must be used within a CardanoProvider');
  }
  return context;
};

// Provider component
export const CardanoProvider: React.FC<CardanoProviderProps> = ({ children }) => {
  const [isWalletAvailable, setIsWalletAvailable] = useState<boolean>(false);
  const [connectedWallet, setConnectedWallet] = useState<CardanoWallet | null>(null);
  const [network, setNetwork] = useState<CardanoNetwork>(CardanoNetwork.Testnet);
  const [walletAddresses, setWalletAddresses] = useState<string[]>([]);
  const [gameWallets, setGameWallets] = useState<OnChainWallet[]>([]);
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);

  // Check wallet availability on mount
  useEffect(() => {
    const checkWalletAvailability = async () => {
      try {
        const available = await cardanoBridge.checkWalletAvailability();
        setIsWalletAvailable(available);
      } catch (err) {
        console.error('Error checking wallet availability', err);
        setIsWalletAvailable(false);
      }
    };

    checkWalletAvailability();
  }, []);

  // Connect to a wallet
  const connectWallet = async (walletName: string): Promise<boolean> => {
    setIsLoading(true);
    setError(null);
    
    try {
      // If walletName is empty, disconnect
      if (!walletName) {
        setConnectedWallet(null);
        setWalletAddresses([]);
        setIsLoading(false);
        return false;
      }
      
      const connected = await cardanoBridge.connectWallet(walletName);
      
      if (connected) {
        // We would set the connected wallet here
        setConnectedWallet({
          name: walletName,
          id: walletName,
          icon: `/images/wallets/${walletName.toLowerCase()}.png`,
          apiVersion: '1.0.0',
          isConnected: true,
          isEnabled: async () => true,
          enable: async () => true,
          getUsedAddresses: async () => [],
          getUnusedAddresses: async () => [],
          getNetworkId: async () => 0,
          signTx: async (tx) => tx,
          submitTx: async (tx) => tx
        });
        
        // In a real implementation, we would get network and addresses here
        setNetwork(CardanoNetwork.Testnet);
        setWalletAddresses([`addr_test1${Math.random().toString(36).substring(2, 15)}`]);
      }
      
      return connected;
    } catch (err: any) {
      console.error('Error connecting wallet', err);
      setError(err.message || 'Failed to connect wallet');
      return false;
    } finally {
      setIsLoading(false);
    }
  };

  // Load game wallets
  const loadGameWallets = async (gameId: string): Promise<void> => {
    setIsLoading(true);
    setError(null);
    
    try {
      const wallets = await cardanoBridge.fetchGameWallets(gameId);
      setGameWallets(wallets);
    } catch (err: any) {
      console.error('Error loading game wallets', err);
      setError(err.message || 'Failed to load game wallets');
    } finally {
      setIsLoading(false);
    }
  };

  // Attempt to unlock a wallet
  const attemptUnlock = async (
    walletAddress: string, 
    secretPhrase: string
  ): Promise<{ success: boolean; txId?: string; message: string }> => {
    setIsLoading(true);
    setError(null);
    
    try {
      const result = await cardanoBridge.attemptUnlockWallet(walletAddress, secretPhrase);
      return result;
    } catch (err: any) {
      console.error('Error attempting unlock', err);
      setError(err.message || 'Failed to attempt unlock');
      return {
        success: false,
        message: err.message || 'An unknown error occurred'
      };
    } finally {
      setIsLoading(false);
    }
  };

  // Create the context value
  const value: CardanoContextType = {
    isWalletAvailable,
    connectWallet,
    connectedWallet,
    network,
    walletAddresses,
    loadGameWallets,
    gameWallets,
    isLoading,
    error,
    attemptUnlock
  };

  // Return the provider with the context value
  return (
    <CardanoContext.Provider value={value}>
      {children}
    </CardanoContext.Provider>
  );
};

export default CardanoProvider; 