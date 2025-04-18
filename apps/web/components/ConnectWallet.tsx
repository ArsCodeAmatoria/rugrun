'use client'

import React, { useState } from 'react'
import { useApi } from '@/providers/ApiProvider'
import { motion } from 'framer-motion'

interface ConnectWalletProps {
  onConnect?: (address: string) => void;
}

const ConnectWallet: React.FC<ConnectWalletProps> = ({ onConnect }) => {
  const [walletAddress, setWalletAddress] = useState('')
  const [username, setUsername] = useState('')
  const { submitWallet, loading, error, clearError, client } = useApi()

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    
    if (walletAddress.trim()) {
      try {
        // First submit the wallet
        await submitWallet(walletAddress.trim())
        
        // Then create or update the user profile
        await client.post('/api/user', {
          address: walletAddress.trim(),
          username: username.trim() || undefined
        })
        
        // Call the onConnect callback if provided
        if (onConnect) {
          onConnect(walletAddress.trim())
        }
        
        // Reset form fields
        setWalletAddress('')
        setUsername('')
      } catch (error) {
        console.error('Error connecting wallet:', error)
      }
    }
  }

  return (
    <div className="w-full max-w-md mx-auto p-4">
      <motion.div 
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-purple-500/30"
      >
        <h2 className="text-2xl font-bold text-white mb-4">Connect Your Wallet</h2>
        
        {error && (
          <div className="mb-4 p-3 bg-red-900/50 border border-red-500 rounded-md text-red-200 relative">
            <p>{error}</p>
            <button 
              onClick={clearError}
              className="absolute top-2 right-2 text-red-300 hover:text-red-100"
            >
              &times;
            </button>
          </div>
        )}
        
        <form onSubmit={handleSubmit}>
          <div className="mb-4">
            <label htmlFor="wallet" className="block text-gray-300 mb-2">
              Wallet Address
            </label>
            <input
              id="wallet"
              type="text"
              value={walletAddress}
              onChange={(e) => setWalletAddress(e.target.value)}
              placeholder="Enter your wallet address"
              className="w-full p-3 bg-gray-800 border border-gray-700 rounded-md text-white focus:border-purple-500 focus:outline-none focus:ring-1 focus:ring-purple-500"
              disabled={loading}
            />
          </div>
          
          <div className="mb-4">
            <label htmlFor="username" className="block text-gray-300 mb-2">
              Username (optional)
            </label>
            <input
              id="username"
              type="text"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
              placeholder="Choose a username"
              className="w-full p-3 bg-gray-800 border border-gray-700 rounded-md text-white focus:border-purple-500 focus:outline-none focus:ring-1 focus:ring-purple-500"
              disabled={loading}
            />
          </div>
          
          <button
            type="submit"
            disabled={loading || !walletAddress.trim()}
            className={`w-full py-3 px-4 rounded-md font-medium transition-all duration-200 
              ${loading ? 'bg-purple-700/50 cursor-wait' : 'bg-purple-600 hover:bg-purple-700'} 
              ${!walletAddress.trim() && !loading ? 'opacity-50 cursor-not-allowed' : 'opacity-100'}`}
          >
            {loading ? 'Connecting...' : 'Connect Wallet'}
          </button>
        </form>
      </motion.div>
    </div>
  )
}

export default ConnectWallet 