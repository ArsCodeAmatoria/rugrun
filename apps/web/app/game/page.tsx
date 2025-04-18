'use client'

import React, { useState, useEffect } from 'react'
import { useGame } from '@/providers/GameProvider'
import { motion, AnimatePresence } from 'framer-motion'
import WalletItem from '@/components/WalletItem'

const GamePage: React.FC = () => {
  const [showInstructions, setShowInstructions] = useState(false);
  const { wallets, userAttempts, refreshGameState, resetGame } = useGame()
  const [isResetting, setIsResetting] = useState(false)
  
  // Animation variants for staggered children
  const containerVariants = {
    hidden: { opacity: 0 },
    visible: {
      opacity: 1,
      transition: {
        staggerChildren: 0.1
      }
    }
  };

  const itemVariants = {
    hidden: { opacity: 0, y: 20 },
    visible: { opacity: 1, y: 0 }
  };

  const handleReset = async () => {
    if (isResetting) return
    
    const confirmed = window.confirm(
      "Are you sure you want to reset the game? This will clear all your attempts and generate new wallets."
    )
    
    if (confirmed) {
      setIsResetting(true)
      try {
        await resetGame()
      } finally {
        setIsResetting(false)
      }
    }
  }

  // Calculate statistics
  const totalWallets = wallets.length
  const unlockedWallets = userAttempts.filter(attempt => attempt.success).length
  const totalAttempts = userAttempts.length
  const successRate = totalAttempts > 0 
    ? ((unlockedWallets / totalAttempts) * 100).toFixed(1) 
    : '0'

  return (
    <motion.div 
      initial="hidden"
      animate="visible"
      variants={containerVariants}
      className="container mx-auto py-8 px-4"
    >
      <motion.div variants={itemVariants} className="relative">
        <div className="absolute left-0 top-2">
          <motion.a
            href="/"
            whileHover={{ scale: 1.05 }}
            whileTap={{ scale: 0.95 }}
            className="flex items-center text-indigo-300 hover:text-indigo-200 px-3 py-1 rounded-lg bg-indigo-900/30 border border-indigo-500/30 text-sm"
          >
            <svg className="w-5 h-5 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"></path>
            </svg>
            Back to Home
          </motion.a>
        </div>
        
        <h1 className="text-4xl md:text-5xl font-bold text-center text-white mb-2">
          <span className="bg-gradient-to-r from-indigo-400 to-purple-500 text-transparent bg-clip-text">
            RugRun Game
          </span>
        </h1>
        <p className="text-center text-indigo-300 mb-8">
          Find the real wallet and unlock it with the correct secret phrase
        </p>
        
        <div className="absolute right-0 top-2">
          <motion.button
            whileHover={{ scale: 1.05 }}
            whileTap={{ scale: 0.95 }}
            onClick={() => setShowInstructions(!showInstructions)}
            className="flex items-center text-indigo-300 hover:text-indigo-200 px-3 py-1 rounded-lg bg-indigo-900/30 border border-indigo-500/30 text-sm"
          >
            <svg className="w-5 h-5 mr-1" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
              <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd"></path>
            </svg>
            {showInstructions ? 'Hide Instructions' : 'How to Play'}
          </motion.button>
        </div>
      </motion.div>
      
      <AnimatePresence>
        {showInstructions && (
          <motion.div
            initial={{ opacity: 0, height: 0 }}
            animate={{ opacity: 1, height: 'auto' }}
            exit={{ opacity: 0, height: 0 }}
            transition={{ duration: 0.3 }}
            className="mb-8 overflow-hidden"
          >
            <div className="bg-gradient-to-r from-indigo-900/30 to-purple-900/30 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
              <h2 className="text-xl font-bold text-white mb-3 flex items-center">
                <svg className="w-5 h-5 mr-2 text-indigo-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                  <path d="M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM15.657 5.757a1 1 0 00-1.414-1.414l-.707.707a1 1 0 001.414 1.414l.707-.707zM18 10a1 1 0 01-1 1h-1a1 1 0 110-2h1a1 1 0 011 1zM5.05 6.464A1 1 0 106.464 5.05l-.707-.707a1 1 0 00-1.414 1.414l.707.707zM5 10a1 1 0 01-1 1H3a1 1 0 110-2h1a1 1 0 011 1zM8 16v-1h4v1a2 2 0 11-4 0zM12 14c.015-.34.208-.646.477-.859a4 4 0 10-4.954 0c.27.213.462.519.476.859h4.002z"></path>
                </svg>
                How to Play RugRun
              </h2>
              <div className="space-y-4 text-gray-300">
                <div>
                  <h3 className="font-bold text-indigo-200">Game Objective:</h3>
                  <p>Identify the real wallet among the decoys and unlock it with the correct secret phrase. The real wallet contains the largest share of tokens.</p>
                </div>
                <div>
                  <h3 className="font-bold text-indigo-200">Instructions:</h3>
                  <ol className="list-decimal list-inside space-y-2 ml-2">
                    <li>Examine each wallet's address, balance, and clue carefully.</li>
                    <li>Decoy wallets have misleading clues, while the real wallet's clue holds a genuine hint.</li>
                    <li>Select a wallet to attempt unlocking it.</li>
                    <li>Enter your guess for the secret phrase in the unlock form.</li>
                    <li>If successful, you'll claim the tokens. If not, you'll receive feedback.</li>
                  </ol>
                </div>
                <div>
                  <h3 className="font-bold text-indigo-200">Tips:</h3>
                  <ul className="list-disc list-inside space-y-1 ml-2">
                    <li>The real wallet typically has a higher balance.</li>
                    <li>Pay close attention to the clues - the real one contains a genuine hint.</li>
                    <li>The secret is usually related to Haskell programming language.</li>
                  </ul>
                </div>
                <div className="bg-black/30 p-3 rounded-lg border border-indigo-500/20 mt-3">
                  <p className="text-sm italic">This game demonstrates the concepts of smart contracts and validators in Haskell-based blockchains, simulating how funds can be locked and unlocked using cryptographic verification.</p>
                </div>
              </div>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
      
      <motion.div variants={itemVariants}>
        <GameStatus />
      </motion.div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mt-8">
        <motion.div 
          initial={{ opacity: 0, scale: 0.9 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 0.1 }}
          className="bg-white p-5 rounded-lg shadow-md border border-gray-200"
        >
          <h3 className="text-xl font-semibold text-gray-800 mb-3">Game Stats</h3>
          <div className="space-y-3">
            <div>
              <p className="text-sm text-gray-500">Wallets Unlocked</p>
              <p className="text-2xl font-bold text-blue-600">{unlockedWallets} / {totalWallets}</p>
            </div>
            <div>
              <p className="text-sm text-gray-500">Attempts Made</p>
              <p className="text-2xl font-bold text-indigo-600">{totalAttempts}</p>
            </div>
            <div>
              <p className="text-sm text-gray-500">Success Rate</p>
              <p className="text-2xl font-bold text-green-600">{successRate}%</p>
            </div>
          </div>
        </motion.div>

        <motion.div 
          initial={{ opacity: 0, scale: 0.9 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 0.2 }}
          className="bg-white p-5 rounded-lg shadow-md border border-gray-200 lg:col-span-2"
        >
          <h3 className="text-xl font-semibold text-gray-800 mb-3">Recent Attempts</h3>
          {userAttempts.length > 0 ? (
            <div className="overflow-y-auto max-h-[12rem]">
              {[...userAttempts].reverse().slice(0, 10).map((attempt, idx) => (
                <div 
                  key={idx} 
                  className={`p-3 mb-2 rounded-md text-sm ${
                    attempt.success 
                      ? 'bg-green-50 border border-green-100' 
                      : 'bg-red-50 border border-red-100'
                  }`}
                >
                  <div className="flex justify-between">
                    <span className="font-medium">
                      {attempt.success ? '✅ Success' : '❌ Failed'}
                    </span>
                    <span className="text-gray-500 text-xs">
                      {new Date(attempt.timestamp).toLocaleTimeString()}
                    </span>
                  </div>
                  <p className="font-mono text-xs mt-1 truncate">
                    <span className="text-gray-600">Wallet:</span> {attempt.walletAddress.substring(0, 16)}...
                  </p>
                  <p className="font-mono text-xs mt-1">
                    <span className="text-gray-600">Phrase:</span> {attempt.attempt}
                  </p>
                </div>
              ))}
            </div>
          ) : (
            <p className="text-gray-500 italic">No attempts yet. Try unlocking a wallet!</p>
          )}
        </motion.div>
      </div>

      <div className="flex justify-between items-center mb-6">
        <h2 className="text-2xl font-bold text-gray-800">Challenge Wallets</h2>
        <div className="flex space-x-3">
          <button
            onClick={refreshGameState}
            className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors text-sm font-medium"
          >
            Refresh Wallets
          </button>
          <button
            onClick={handleReset}
            disabled={isResetting}
            className={`px-4 py-2 text-sm font-medium rounded transition-colors ${
              isResetting 
                ? 'bg-gray-300 text-gray-500 cursor-not-allowed' 
                : 'bg-red-600 text-white hover:bg-red-700'
            }`}
          >
            {isResetting ? 'Resetting...' : 'Reset Game'}
          </button>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {wallets.map((wallet, index) => (
          <WalletItem key={wallet.address} wallet={wallet} index={index} />
        ))}
      </div>

      <motion.div 
        variants={itemVariants}
        className="mt-10 text-center text-sm text-indigo-300/60"
      >
        <p>© 2023 RugRun • A Haskell Smart Contract Simulation Game</p>
      </motion.div>
    </motion.div>
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
      className="bg-gradient-to-r from-gray-900/70 to-indigo-900/40 backdrop-blur-md p-6 rounded-xl shadow-lg border border-purple-500/30 mb-8"
    >
      <div className="flex flex-col md:flex-row justify-between items-start md:items-center">
        <div>
          <h2 className="text-2xl font-bold text-white mb-4 flex items-center">
            <svg className="w-6 h-6 mr-2 text-indigo-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
            </svg>
            Game Status
          </h2>
          <div className="grid grid-cols-2 gap-6">
            <div className="bg-black/20 p-3 rounded-lg border border-indigo-500/20">
              <p className="text-gray-400 text-sm mb-1">Wallets</p>
              <div className="flex items-end">
                <p className="text-2xl font-bold text-white">{gameStatus.totalWallets}</p>
                <motion.div
                  animate={{ 
                    scale: [1, 1.2, 1],
                    rotate: [0, 5, 0] 
                  }}
                  transition={{ 
                    duration: 0.5,
                    repeat: Infinity,
                    repeatDelay: 3
                  }}
                  className="ml-2 mb-1"
                >
                  <svg className="w-5 h-5 text-indigo-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                    <path d="M4 4a2 2 0 00-2 2v1h16V6a2 2 0 00-2-2H4z"></path>
                    <path fillRule="evenodd" d="M18 9H2v5a2 2 0 002 2h12a2 2 0 002-2V9zM4 13a1 1 0 011-1h1a1 1 0 110 2H5a1 1 0 01-1-1zm5-1a1 1 0 100 2h1a1 1 0 100-2H9z" clipRule="evenodd"></path>
                  </svg>
                </motion.div>
              </div>
            </div>
            <div className="bg-black/20 p-3 rounded-lg border border-indigo-500/20">
              <p className="text-gray-400 text-sm mb-1">Real wallet found</p>
              <div className="flex items-center">
                {gameStatus.realWalletFound ? (
                  <>
                    <motion.div
                      initial={{ scale: 0 }}
                      animate={{ scale: 1 }}
                      transition={{ type: "spring", stiffness: 500, damping: 10 }}
                    >
                      <svg className="w-6 h-6 text-green-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                        <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd"></path>
                      </svg>
                    </motion.div>
                    <span className="text-green-400 text-xl font-bold ml-2">Found!</span>
                  </>
                ) : (
                  <>
                    <motion.div
                      animate={{ 
                        opacity: [0.5, 1, 0.5],
                      }}
                      transition={{ 
                        duration: 2,
                        repeat: Infinity,
                      }}
                    >
                      <svg className="w-6 h-6 text-indigo-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                        <path fillRule="evenodd" d="M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z" clipRule="evenodd"></path>
                      </svg>
                    </motion.div>
                    <span className="text-indigo-300 text-xl font-bold ml-2">Searching...</span>
                  </>
                )}
              </div>
            </div>
            <div className="bg-black/20 p-3 rounded-lg border border-indigo-500/20">
              <p className="text-gray-400 text-sm mb-1">Total attempts</p>
              <div className="flex items-end">
                <p className="text-2xl font-bold text-white">{gameStatus.totalAttempts}</p>
                <svg className="w-5 h-5 text-indigo-400 ml-2 mb-1" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd"></path>
                </svg>
              </div>
            </div>
            <div className="bg-black/20 p-3 rounded-lg border border-indigo-500/20">
              <p className="text-gray-400 text-sm mb-1">Successful attempts</p>
              <div className="flex items-end">
                <p className="text-2xl font-bold text-white">{gameStatus.successfulAttempts}</p>
                {gameStatus.successfulAttempts > 0 && (
                  <motion.div
                    animate={{ rotate: [0, 5, 0, -5, 0] }}
                    transition={{ duration: 0.5, repeat: Infinity, repeatDelay: 2 }}
                    className="ml-2 mb-1"
                  >
                    <svg className="w-5 h-5 text-green-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                      <path fillRule="evenodd" d="M6.267 3.455a3.066 3.066 0 001.745-.723 3.066 3.066 0 013.976 0 3.066 3.066 0 001.745.723 3.066 3.066 0 012.812 2.812c.051.643.304 1.254.723 1.745a3.066 3.066 0 010 3.976 3.066 3.066 0 00-.723 1.745 3.066 3.066 0 01-2.812 2.812 3.066 3.066 0 00-1.745.723 3.066 3.066 0 01-3.976 0 3.066 3.066 0 00-1.745-.723 3.066 3.066 0 01-2.812-2.812 3.066 3.066 0 00-.723-1.745 3.066 3.066 0 010-3.976 3.066 3.066 0 00.723-1.745 3.066 3.066 0 012.812-2.812zm7.44 5.252a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd"></path>
                    </svg>
                  </motion.div>
                )}
              </div>
            </div>
          </div>
        </div>
        
        <motion.button
          onClick={handleReset}
          whileHover={{ scale: 1.05 }}
          whileTap={{ scale: 0.95 }}
          className="mt-6 md:mt-0 bg-gradient-to-r from-red-600 to-red-700 text-white font-medium py-2 px-5 rounded-lg transition-all shadow-md hover:shadow-red-600/20 flex items-center"
        >
          <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path>
          </svg>
          Reset Game
        </motion.button>
      </div>
    </motion.div>
  )
}

export default GamePage 