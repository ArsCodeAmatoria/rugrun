'use client'

import React, { useState } from 'react'
import { useGame } from '@/providers/GameProvider'
import { motion } from 'framer-motion'

const GamePage: React.FC = () => {
  return (
    <div className="container mx-auto py-12 px-4">
      <h1 className="text-4xl font-bold text-center text-white mb-8">RugRun Game</h1>
      
      <GameStatus />
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mt-8">
        <div>
          <WalletList />
        </div>
        <div>
          <AttemptsList />
        </div>
      </div>
    </div>
  )
}

const GameStatus: React.FC = () => {
  const { gameStatus, resetGame } = useGame()
  
  const handleReset = () => {
    if (confirm('Are you sure you want to reset the game? All progress will be lost.')) {
      resetGame()
    }
  }
  
  return (
    <motion.div
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-purple-500/30 mb-8"
    >
      <div className="flex flex-col md:flex-row justify-between items-start md:items-center">
        <div>
          <h2 className="text-2xl font-bold text-white mb-2">Game Status</h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <p className="text-gray-400 text-sm">Wallets:</p>
              <p className="text-xl font-bold text-white">{gameStatus.totalWallets}</p>
            </div>
            <div>
              <p className="text-gray-400 text-sm">Real wallet found:</p>
              <p className="text-xl font-bold text-white">
                {gameStatus.realWalletFound ? 
                  <span className="text-green-400">Yes</span> : 
                  <span className="text-red-400">No</span>}
              </p>
            </div>
            <div>
              <p className="text-gray-400 text-sm">Total attempts:</p>
              <p className="text-xl font-bold text-white">{gameStatus.totalAttempts}</p>
            </div>
            <div>
              <p className="text-gray-400 text-sm">Successful attempts:</p>
              <p className="text-xl font-bold text-white">{gameStatus.successfulAttempts}</p>
            </div>
          </div>
        </div>
        
        <button
          onClick={handleReset}
          className="mt-4 md:mt-0 bg-red-600 hover:bg-red-700 text-white font-medium py-2 px-4 rounded-lg transition-colors"
        >
          Reset Game
        </button>
      </div>
    </motion.div>
  )
}

const WalletList: React.FC = () => {
  const { wallets, isLoading } = useGame()
  const [selectedWallet, setSelectedWallet] = useState<string | null>(null)
  
  if (isLoading) {
    return (
      <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
        <h2 className="text-2xl font-bold text-white mb-4">Wallets</h2>
        <p className="text-gray-400">Loading wallets...</p>
      </div>
    )
  }
  
  return (
    <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
      <h2 className="text-2xl font-bold text-white mb-4">Wallets</h2>
      
      {wallets.length === 0 ? (
        <p className="text-gray-400">No wallets found. Reset the game to generate new wallets.</p>
      ) : (
        <div className="space-y-4">
          {wallets.map((wallet) => (
            <WalletItem
              key={wallet.id}
              wallet={wallet}
              isSelected={selectedWallet === wallet.address}
              onSelect={() => setSelectedWallet(wallet.address)}
            />
          ))}
        </div>
      )}
      
      {selectedWallet && (
        <UnlockWalletForm walletAddress={selectedWallet} />
      )}
    </div>
  )
}

const WalletItem: React.FC<{
  wallet: any;
  isSelected: boolean;
  onSelect: () => void;
}> = ({ wallet, isSelected, onSelect }) => {
  return (
    <motion.div
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      className={`p-4 rounded-lg cursor-pointer transition-colors ${
        isSelected 
          ? 'bg-indigo-900/40 border border-indigo-500' 
          : 'bg-gray-800/40 hover:bg-gray-700/30 border border-gray-700'
      }`}
      onClick={onSelect}
    >
      <div className="flex justify-between items-start">
        <div>
          <h3 className="font-medium text-white">{wallet.address.substring(0, 12)}...{wallet.address.substring(wallet.address.length - 8)}</h3>
          <p className="text-sm text-gray-400 mt-1">{wallet.balance} tokens</p>
        </div>
        <div className="bg-indigo-800/50 px-2 py-1 rounded text-xs text-indigo-200">
          ID: {wallet.id.substring(0, 6)}
        </div>
      </div>
      
      <div className="mt-3">
        <p className="text-sm text-gray-300 italic">&ldquo;{wallet.clue}&rdquo;</p>
      </div>
    </motion.div>
  )
}

const UnlockWalletForm: React.FC<{ walletAddress: string }> = ({ walletAddress }) => {
  const { attemptUnlock } = useGame()
  const [secretPhrase, setSecretPhrase] = useState('')
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [result, setResult] = useState<{ success: boolean; message: string } | null>(null)
  
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!secretPhrase.trim()) {
      return
    }
    
    setIsSubmitting(true)
    setResult(null)
    
    try {
      const result = await attemptUnlock(walletAddress, secretPhrase.trim())
      setResult({
        success: result.success,
        message: result.message
      })
      if (result.success) {
        setSecretPhrase('')
      }
    } catch (error) {
      setResult({
        success: false,
        message: error instanceof Error ? error.message : 'An unknown error occurred'
      })
    } finally {
      setIsSubmitting(false)
    }
  }
  
  return (
    <div className="mt-6 bg-gray-800/40 p-4 rounded-lg border border-gray-700">
      <h3 className="text-xl font-bold text-white mb-3">Attempt Unlock</h3>
      
      <form onSubmit={handleSubmit}>
        <div className="mb-4">
          <label htmlFor="secretPhrase" className="block text-gray-300 mb-2">
            Secret Phrase
          </label>
          <input
            id="secretPhrase"
            type="text"
            value={secretPhrase}
            onChange={(e) => setSecretPhrase(e.target.value)}
            placeholder="Enter the secret phrase"
            className="w-full p-3 bg-gray-700/50 border border-gray-600 rounded-md text-white focus:border-indigo-500 focus:outline-none focus:ring-1 focus:ring-indigo-500"
            disabled={isSubmitting}
          />
        </div>
        
        <button
          type="submit"
          disabled={isSubmitting || !secretPhrase.trim()}
          className={`w-full py-3 px-4 rounded-md font-medium transition-colors 
            ${isSubmitting ? 'bg-indigo-700/50 cursor-wait' : 'bg-indigo-600 hover:bg-indigo-700'} 
            ${!secretPhrase.trim() && !isSubmitting ? 'opacity-50 cursor-not-allowed' : 'opacity-100'}`}
        >
          {isSubmitting ? 'Attempting...' : 'Try Unlock'}
        </button>
      </form>
      
      {result && (
        <div className={`mt-4 p-3 rounded-md ${
          result.success 
            ? 'bg-green-900/50 border border-green-500 text-green-200' 
            : 'bg-red-900/50 border border-red-500 text-red-200'
        }`}>
          <p>{result.message}</p>
        </div>
      )}
    </div>
  )
}

const AttemptsList: React.FC = () => {
  const { userAttempts, isLoading } = useGame()
  
  if (isLoading) {
    return (
      <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
        <h2 className="text-2xl font-bold text-white mb-4">Your Attempts</h2>
        <p className="text-gray-400">Loading attempts...</p>
      </div>
    )
  }
  
  const sortedAttempts = [...userAttempts].sort((a, b) => b.timestamp - a.timestamp)
  
  return (
    <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
      <h2 className="text-2xl font-bold text-white mb-4">Your Attempts</h2>
      
      {sortedAttempts.length === 0 ? (
        <p className="text-gray-400">No attempts yet. Try to unlock a wallet!</p>
      ) : (
        <div className="space-y-3 max-h-[600px] overflow-y-auto pr-2 custom-scrollbar">
          {sortedAttempts.map((attempt, index) => (
            <div 
              key={index}
              className={`p-3 rounded-lg border ${
                attempt.success 
                  ? 'bg-green-900/20 border-green-500/50' 
                  : 'bg-gray-800/40 border-gray-700'
              }`}
            >
              <div className="flex justify-between items-start mb-2">
                <span className="text-sm font-medium text-gray-300">
                  {new Date(attempt.timestamp).toLocaleString()}
                </span>
                <span className={`text-xs px-2 py-1 rounded ${
                  attempt.success ? 'bg-green-700/50 text-green-200' : 'bg-red-700/50 text-red-200'
                }`}>
                  {attempt.success ? 'Success' : 'Failed'}
                </span>
              </div>
              
              <div>
                <p className="text-sm text-gray-400">Wallet: {attempt.walletAddress.substring(0, 8)}...{attempt.walletAddress.substring(attempt.walletAddress.length - 8)}</p>
                <p className="text-sm text-gray-400">Attempt: &ldquo;{attempt.attempt}&rdquo;</p>
                <p className="text-sm text-gray-300 mt-1">{attempt.message}</p>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export default GamePage 