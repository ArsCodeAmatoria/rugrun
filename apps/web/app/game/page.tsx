'use client'

import React, { useState, useEffect } from 'react'
import { useGame } from '@/providers/GameProvider'
import { motion, AnimatePresence } from 'framer-motion'

const GamePage: React.FC = () => {
  const [showInstructions, setShowInstructions] = useState(false);

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
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mt-8">
        <motion.div variants={itemVariants}>
          <WalletList />
        </motion.div>
        <motion.div variants={itemVariants}>
          <AttemptsList />
        </motion.div>
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
  // Generate a unique color based on wallet address
  const getWalletColor = (address: string) => {
    const colors = ['#4F46E5', '#7C3AED', '#8B5CF6', '#EC4899', '#F43F5E', '#6366F1', '#06B6D4'];
    const index = parseInt(address.substring(0, 6), 16) % colors.length;
    return colors[index];
  };

  const walletColor = getWalletColor(wallet.address);

  return (
    <motion.div
      initial={{ opacity: 0, y: 10 }}
      animate={{ opacity: 1, y: 0 }}
      whileHover={{ scale: 1.02 }}
      transition={{ duration: 0.2 }}
      className={`wallet-card p-4 rounded-lg cursor-pointer transition-all ${
        isSelected 
          ? 'bg-indigo-900/40 border border-indigo-500 shadow-[0_0_15px_rgba(99,102,241,0.3)] wallet-glow' 
          : 'bg-gray-800/40 hover:bg-gray-700/30 border border-gray-700 hover:border-gray-500'
      }`}
      onClick={onSelect}
    >
      <div className="flex items-start gap-3">
        {/* Wallet Visual Representation */}
        <div 
          className={`h-16 w-12 rounded-md flex-shrink-0 flex flex-col justify-between p-1.5 ${isSelected ? 'wallet-glow' : ''}`}
          style={{ 
            background: `linear-gradient(145deg, ${walletColor}99, ${walletColor}55)`,
            borderLeft: `3px solid ${walletColor}`,
            boxShadow: `0 4px 12px ${walletColor}33`
          }}
        >
          <div className="flex justify-between">
            <div className="h-1.5 w-1.5 rounded-full bg-white/70"></div>
            <div className="h-1.5 w-1.5 rounded-full bg-white/70"></div>
          </div>
          <div className="flex justify-center">
            <motion.div 
              animate={{ rotate: isSelected ? [0, -5, 5, -5, 5, 0] : 0 }}
              transition={{ duration: 0.5, delay: 0.2 }}
              className="h-3 w-5 bg-white/50 rounded-sm"
            ></motion.div>
          </div>
          <div className="text-[7px] text-white/80 text-center font-mono mt-1">
            {wallet.id.substring(0, 4)}
          </div>
        </div>

        <div className="flex-1">
          <div className="flex justify-between items-start">
            <div>
              <h3 className="font-medium text-white font-mono">{wallet.address.substring(0, 8)}...{wallet.address.substring(wallet.address.length - 6)}</h3>
              <div className="flex items-center mt-1">
                <div className="h-2 w-2 rounded-full bg-purple-400 mr-2 animate-pulse"></div>
                <p className="text-sm text-indigo-200 font-medium">{wallet.balance} tokens</p>
              </div>
            </div>
            <div className="bg-black/30 px-2 py-1 rounded text-xs text-indigo-200 border border-indigo-500/30">
              ID: {wallet.id.substring(0, 6)}
            </div>
          </div>
          
          <div className="mt-3">
            <p className="text-sm text-gray-300 italic leading-relaxed">&ldquo;{wallet.clue}&rdquo;</p>
          </div>
        </div>
      </div>

      {isSelected && (
        <motion.div
          initial={{ height: 0, opacity: 0 }}
          animate={{ height: 'auto', opacity: 1 }}
          exit={{ height: 0, opacity: 0 }}
          className="mt-3 pt-3 border-t border-indigo-500/30"
        >
          <p className="text-xs text-indigo-300">This wallet is selected. Use the form below to attempt to unlock it.</p>
        </motion.div>
      )}
    </motion.div>
  )
}

const UnlockWalletForm: React.FC<{ walletAddress: string }> = ({ walletAddress }) => {
  const { attemptUnlock } = useGame()
  const [secretPhrase, setSecretPhrase] = useState('')
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [result, setResult] = useState<{ success: boolean; message: string } | null>(null)
  const [resultAnimation, setResultAnimation] = useState(false)
  
  useEffect(() => {
    if (result) {
      setResultAnimation(true);
    }
  }, [result]);
  
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!secretPhrase.trim()) {
      return
    }
    
    setIsSubmitting(true)
    setResult(null)
    setResultAnimation(false)
    
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
    <motion.div
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.3 }}
      className="mt-6 bg-gray-800/60 p-5 rounded-lg border border-indigo-500/30 backdrop-blur-sm shadow-lg"
    >
      <h3 className="text-xl font-bold text-white mb-3">Attempt Unlock</h3>
      
      <form onSubmit={handleSubmit}>
        <div className="mb-4">
          <label htmlFor="secretPhrase" className="block text-indigo-200 mb-2 text-sm font-medium">
            Secret Phrase
          </label>
          <input
            id="secretPhrase"
            type="text"
            value={secretPhrase}
            onChange={(e) => setSecretPhrase(e.target.value)}
            placeholder="Enter the secret phrase"
            className="w-full p-3 bg-gray-900/80 border border-indigo-500/30 rounded-md text-white focus:border-indigo-500 focus:outline-none focus:ring-1 focus:ring-indigo-500 placeholder-gray-500"
            disabled={isSubmitting}
          />
        </div>
        
        <motion.button
          type="submit"
          disabled={isSubmitting || !secretPhrase.trim()}
          whileHover={{ scale: 1.02 }}
          whileTap={{ scale: 0.98 }}
          className={`w-full py-3 px-4 rounded-md font-medium transition-colors 
            ${isSubmitting ? 'bg-indigo-700/50 cursor-wait' : 'bg-indigo-600 hover:bg-indigo-700'} 
            ${!secretPhrase.trim() && !isSubmitting ? 'opacity-50 cursor-not-allowed' : 'opacity-100'}
            text-white shadow-md hover:shadow-indigo-500/20`}
        >
          {isSubmitting ? (
            <span className="flex items-center justify-center">
              <svg className="animate-spin -ml-1 mr-2 h-4 w-4 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
                <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
              </svg>
              Attempting...
            </span>
          ) : 'Try Unlock'}
        </motion.button>
      </form>
      
      <AnimatePresence>
        {result && resultAnimation && (
          <motion.div 
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -10 }}
            className={`mt-4 p-4 rounded-md ${
              result.success 
                ? 'bg-green-900/40 border border-green-500/50 text-green-200' 
                : 'bg-red-900/40 border border-red-500/50 text-red-200'
            }`}
          >
            <div className="flex items-center">
              {result.success ? (
                <svg className="w-5 h-5 mr-2 text-green-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd"></path>
                </svg>
              ) : (
                <svg className="w-5 h-5 mr-2 text-red-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd"></path>
                </svg>
              )}
              <p>{result.message}</p>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </motion.div>
  )
}

const AttemptsList: React.FC = () => {
  const { userAttempts, isLoading } = useGame()
  
  if (isLoading) {
    return (
      <div className="bg-gray-900/60 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
        <h2 className="text-2xl font-bold text-white mb-4 flex items-center">
          <svg className="w-6 h-6 mr-2 text-indigo-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path>
          </svg>
          Your Attempts
        </h2>
        <div className="flex items-center justify-center h-32">
          <div className="animate-pulse flex space-x-4 items-center">
            <div className="rounded-full bg-indigo-700/40 h-10 w-10"></div>
            <div className="flex-1 space-y-3">
              <div className="h-3 bg-indigo-700/40 rounded w-3/4"></div>
              <div className="h-3 bg-indigo-700/40 rounded w-1/2"></div>
            </div>
          </div>
        </div>
      </div>
    )
  }
  
  const sortedAttempts = [...userAttempts].sort((a, b) => b.timestamp - a.timestamp)
  
  return (
    <div className="bg-gradient-to-r from-gray-900/70 to-indigo-900/40 backdrop-blur-md p-6 rounded-xl shadow-lg border border-indigo-500/30">
      <h2 className="text-2xl font-bold text-white mb-4 flex items-center">
        <svg className="w-6 h-6 mr-2 text-indigo-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path>
        </svg>
        Your Attempts
      </h2>
      
      {sortedAttempts.length === 0 ? (
        <div className="bg-black/30 p-8 rounded-lg border border-indigo-500/20 flex flex-col items-center justify-center text-center">
          <svg className="w-16 h-16 text-indigo-300/50 mb-4" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
            <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z" clipRule="evenodd"></path>
          </svg>
          <p className="text-gray-300 text-lg">No attempts yet.</p>
          <p className="text-indigo-300 mt-2">Select a wallet and try to unlock it with a secret phrase!</p>
        </div>
      ) : (
        <div className="space-y-3 max-h-[600px] overflow-y-auto pr-2 custom-scrollbar">
          {sortedAttempts.map((attempt, index) => (
            <motion.div 
              key={index}
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: index * 0.05 }}
              className={`p-4 rounded-lg border ${
                attempt.success 
                  ? 'bg-green-900/20 border-green-500/50 shadow-[0_0_10px_rgba(74,222,128,0.1)]' 
                  : 'bg-black/30 border-gray-700 hover:border-gray-600'
              }`}
            >
              <div className="flex justify-between items-start mb-2">
                <span className="text-sm font-medium text-gray-300 flex items-center">
                  <svg className="w-4 h-4 mr-1 text-indigo-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 00-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z" clipRule="evenodd"></path>
                  </svg>
                  {new Date(attempt.timestamp).toLocaleString()}
                </span>
                <span className={`text-xs px-2 py-1 rounded-full ${
                  attempt.success 
                    ? 'bg-green-900/70 text-green-200 border border-green-500/50' 
                    : 'bg-red-900/70 text-red-200 border border-red-500/50'
                }`}>
                  {attempt.success ? 'Success' : 'Failed'}
                </span>
              </div>
              
              <div className="mt-2">
                <div className="flex items-center">
                  <div className="bg-black/40 px-2 py-1 rounded text-xs text-indigo-300 font-mono border border-indigo-500/20">
                    {attempt.walletAddress.substring(0, 8)}...{attempt.walletAddress.substring(attempt.walletAddress.length - 8)}
                  </div>
                </div>
                <div className="mt-2 bg-black/20 p-2 rounded border border-indigo-500/10">
                  <p className="text-sm text-indigo-200 mb-1 font-medium">Attempted phrase:</p>
                  <p className="text-sm text-white font-mono">&ldquo;{attempt.attempt}&rdquo;</p>
                </div>
                <div className={`mt-2 p-2 rounded ${
                  attempt.success 
                    ? 'bg-green-900/30 border border-green-500/30' 
                    : 'bg-red-900/20 border border-red-500/30'
                }`}>
                  <p className="text-sm text-gray-300">{attempt.message}</p>
                </div>
              </div>
            </motion.div>
          ))}
        </div>
      )}
    </div>
  )
}

export default GamePage 