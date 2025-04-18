'use client'

import React, { useEffect, useState } from 'react'
import { useApi } from '@/providers/ApiProvider'
import { motion } from 'framer-motion'

const PointsBalance: React.FC<{ userAddress?: string }> = ({ userAddress }) => {
  const [balance, setBalance] = useState<number | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  const [isRefreshing, setIsRefreshing] = useState(false)
  const { getUserPoints, error, clearError } = useApi()

  const fetchBalance = async () => {
    try {
      setIsRefreshing(true)
      clearError()
      
      // Use the new getUserPoints method from ApiProvider
      const points = await getUserPoints(userAddress)
      setBalance(points)
    } catch (err) {
      console.error('Failed to fetch points:', err)
      // The error is already set by the ApiProvider's interceptors
    } finally {
      setIsRefreshing(false)
      setIsLoading(false)
    }
  }

  useEffect(() => {
    fetchBalance()
  }, [userAddress, fetchBalance])

  return (
    <div className="w-full max-w-sm mx-auto p-4">
      <motion.div
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30"
      >
        <h2 className="text-2xl font-bold text-white mb-4">Your Points</h2>
        
        {error && (
          <div className="mb-4 p-3 bg-red-900/50 border border-red-500 rounded-md text-red-200">
            <p>{error}</p>
          </div>
        )}
        
        <div className="flex items-center justify-between mb-4">
          <div className="flex items-center">
            <div className="text-4xl font-bold text-indigo-400">
              {isLoading || isRefreshing ? '...' : balance?.toLocaleString() || '0'}
            </div>
            <span className="ml-2 text-gray-400">points</span>
          </div>
          
          <button
            onClick={fetchBalance}
            disabled={isLoading || isRefreshing}
            className={`p-2 rounded-md transition-colors ${
              isLoading || isRefreshing 
                ? 'bg-indigo-700/50 cursor-wait' 
                : 'bg-indigo-600 hover:bg-indigo-700'
            }`}
            aria-label="Refresh balance"
          >
            <RefreshIcon className={`w-5 h-5 ${isRefreshing ? 'animate-spin' : ''}`} />
          </button>
        </div>
        
        <p className="text-sm text-gray-400">
          Earn more points by completing tasks and connecting with the community!
        </p>
      </motion.div>
    </div>
  )
}

const RefreshIcon: React.FC<{ className?: string }> = ({ className }) => (
  <svg 
    xmlns="http://www.w3.org/2000/svg" 
    fill="none" 
    viewBox="0 0 24 24" 
    stroke="currentColor" 
    className={className}
  >
    <path 
      strokeLinecap="round" 
      strokeLinejoin="round" 
      strokeWidth={2} 
      d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" 
    />
  </svg>
)

export default PointsBalance 