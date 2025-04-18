'use client'

import React, { useEffect, useState } from 'react'
import { useApi } from '@/providers/ApiProvider'
import { motion } from 'framer-motion'
import PointsBalance from './PointsBalance'

interface User {
  id: string;
  address: string;
  username?: string;
  points: number;
  wallets: string[];
  createdAt: string;
  updatedAt: string;
}

interface UserProfileProps {
  address: string;
}

const UserProfile: React.FC<UserProfileProps> = ({ address }) => {
  const [user, setUser] = useState<User | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  const { getUserProfile, error, clearError } = useApi()

  useEffect(() => {
    const fetchUser = async () => {
      try {
        setIsLoading(true)
        clearError()
        
        const userData = await getUserProfile(address)
        setUser(userData)
      } catch (err) {
        console.error('Failed to fetch user profile:', err)
        // The error is already set by the ApiProvider's interceptors
      } finally {
        setIsLoading(false)
      }
    }

    if (address) {
      fetchUser()
    }
  }, [address, clearError, getUserProfile])

  if (isLoading) {
    return (
      <div className="w-full max-w-md mx-auto p-4">
        <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-purple-500/30">
          <p className="text-center text-white">Loading profile...</p>
        </div>
      </div>
    )
  }

  if (error || !user) {
    return (
      <div className="w-full max-w-md mx-auto p-4">
        <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-red-500/30">
          <h2 className="text-2xl font-bold text-white mb-4">Error</h2>
          <p className="text-red-400">{error || 'User not found'}</p>
        </div>
      </div>
    )
  }

  return (
    <div className="w-full max-w-2xl mx-auto p-4">
      <motion.div
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-purple-500/30"
      >
        <div className="flex flex-col md:flex-row justify-between items-start md:items-center mb-6">
          <div>
            <h2 className="text-2xl font-bold text-white">
              {user.username || 'Anonymous User'}
            </h2>
            <p className="text-gray-400 text-xs md:text-sm break-all">
              {user.address}
            </p>
          </div>
          
          <div className="mt-4 md:mt-0 text-right">
            <span className="bg-purple-800/40 px-3 py-1 rounded-full text-purple-200 text-xs">
              Joined {new Date(user.createdAt).toLocaleDateString()}
            </span>
          </div>
        </div>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <PointsBalance userAddress={address} />
          </div>
          
          <div className="bg-gray-800/60 p-4 rounded-lg border border-gray-700">
            <h3 className="text-lg font-semibold text-white mb-3">
              Connected Wallets ({user.wallets.length})
            </h3>
            
            {user.wallets.length === 0 ? (
              <p className="text-gray-400 text-sm">No wallets connected yet.</p>
            ) : (
              <ul className="space-y-2">
                {user.wallets.map((wallet, index) => (
                  <li key={index} className="bg-gray-700/30 p-2 rounded text-xs text-gray-300 break-all">
                    {wallet}
                  </li>
                ))}
              </ul>
            )}
          </div>
        </div>
      </motion.div>
    </div>
  )
}

export default UserProfile 