'use client'

import React, { createContext, useContext, useState, useCallback, ReactNode } from 'react'
import axios, { AxiosInstance } from 'axios'

// Define the API context shape
type ApiContextType = {
  client: AxiosInstance;
  loading: boolean;
  error: string | null;
  submitWallet: (walletAddress: string) => Promise<void>;
  claimReward: (walletAddress: string, signature: string) => Promise<void>;
  clearError: () => void;
  connectWallet: (userAddress: string, walletAddress: string) => Promise<any>;
  getUserProfile: (address: string) => Promise<any>;
  getUserPoints: (address?: string) => Promise<number>;
  addPoints: (address: string, points: number) => Promise<any>;
}

// Create the context with a default value
const ApiContext = createContext<ApiContextType | undefined>(undefined)

// Provider props interface
interface ApiProviderProps {
  children: ReactNode;
  baseURL?: string;
}

// Custom hook to use the API context
export const useApi = () => {
  const context = useContext(ApiContext)
  if (context === undefined) {
    throw new Error('useApi must be used within an ApiProvider')
  }
  return context
}

// Define the provider component
export const ApiProvider: React.FC<ApiProviderProps> = ({ 
  children, 
  baseURL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001'
}) => {
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  
  // Create axios client instance
  const client = axios.create({
    baseURL,
    headers: {
      'Content-Type': 'application/json',
    },
  });

  // Add request interceptor for authentication if needed
  client.interceptors.request.use(
    (config) => {
      // You can add auth tokens here if needed
      return config;
    },
    (error) => {
      return Promise.reject(error);
    }
  );

  // Add response interceptor for error handling
  client.interceptors.response.use(
    (response) => {
      return response;
    },
    (error) => {
      // Handle common errors (401, 403, 500, etc.)
      if (error.response) {
        setError(`Error: ${error.response.status} - ${error.response.data.message || error.response.statusText}`);
      } else if (error.request) {
        setError('Network error: No response received from server');
      } else {
        setError(`Error: ${error.message}`);
      }
      return Promise.reject(error);
    }
  );
  
  // Function to submit a wallet address
  const submitWallet = useCallback(async (walletAddress: string) => {
    setLoading(true)
    setError(null)
    
    try {
      const response = await client.post('/api/submit-wallet', { walletAddress })
      // Handle response as needed
      console.log('Wallet submitted successfully:', response.data)
    } catch (err) {
      console.error('Error submitting wallet:', err)
      setError(err instanceof Error ? err.message : 'Failed to submit wallet')
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to claim reward
  const claimReward = useCallback(async (walletAddress: string, signature: string) => {
    setLoading(true)
    setError(null)
    
    try {
      const response = await client.post('/api/claim-reward', { walletAddress, signature })
      // Handle response as needed
      console.log('Reward claimed successfully:', response.data)
    } catch (err) {
      console.error('Error claiming reward:', err)
      setError(err instanceof Error ? err.message : 'Failed to claim reward')
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to connect wallet to user
  const connectWallet = useCallback(async (userAddress: string, walletAddress: string) => {
    setLoading(true)
    setError(null)
    
    try {
      const response = await client.post('/api/user/connect-wallet', { userAddress, walletAddress })
      console.log('Wallet connected successfully:', response.data)
      return response.data
    } catch (err) {
      console.error('Error connecting wallet:', err)
      setError(err instanceof Error ? err.message : 'Failed to connect wallet')
      throw err
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to get user profile
  const getUserProfile = useCallback(async (address: string) => {
    setLoading(true)
    setError(null)
    
    try {
      const response = await client.get(`/api/user/${address}`)
      console.log('User profile retrieved:', response.data)
      return response.data
    } catch (err) {
      console.error('Error getting user profile:', err)
      setError(err instanceof Error ? err.message : 'Failed to get user profile')
      throw err
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to get user points
  const getUserPoints = useCallback(async (address?: string) => {
    setLoading(true)
    setError(null)
    
    try {
      const url = address ? `/api/user/points?address=${address}` : '/api/user/points'
      const response = await client.get(url)
      console.log('Points retrieved:', response.data)
      return response.data.points
    } catch (err) {
      console.error('Error getting points:', err)
      setError(err instanceof Error ? err.message : 'Failed to get points')
      return 0
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to add points to user
  const addPoints = useCallback(async (address: string, points: number) => {
    setLoading(true)
    setError(null)
    
    try {
      const response = await client.post(`/api/user/${address}/points`, { points })
      console.log('Points added:', response.data)
      return response.data
    } catch (err) {
      console.error('Error adding points:', err)
      setError(err instanceof Error ? err.message : 'Failed to add points')
      throw err
    } finally {
      setLoading(false)
    }
  }, [client])
  
  // Function to clear error
  const clearError = useCallback(() => {
    setError(null)
  }, [])
  
  // Create the context value
  const value = {
    client,
    loading,
    error,
    submitWallet,
    claimReward,
    connectWallet,
    getUserProfile,
    getUserPoints,
    addPoints,
    clearError
  }
  
  // Return the provider with the context value
  return (
    <ApiContext.Provider value={value}>
      {children}
    </ApiContext.Provider>
  )
} 