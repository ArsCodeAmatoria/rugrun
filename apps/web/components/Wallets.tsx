'use client';

import React, { useEffect } from 'react';
import { motion } from 'framer-motion';
import { useCardano } from '@/providers/CardanoProvider';
import WalletItem from './WalletItem';
import ConnectWalletButton from './ConnectWalletButton';

const Wallets: React.FC = () => {
  const { 
    isWalletAvailable, 
    connectedWallet, 
    gameWallets, 
    loadGameWallets, 
    isLoading, 
    error 
  } = useCardano();
  
  // Game ID for wallet filtering (in a real app, this would be dynamically set)
  const gameId = "current-game";
  
  // Load wallets when component mounts or wallet connects
  useEffect(() => {
    if (connectedWallet) {
      loadGameWallets(gameId);
    }
  }, [connectedWallet, gameId, loadGameWallets]);

  return (
    <div className="max-w-6xl mx-auto p-4">
      <div className="flex flex-col md:flex-row justify-between items-start md:items-center mb-8">
        <div>
          <h2 className="text-2xl md:text-3xl font-bold bg-clip-text text-transparent bg-gradient-to-r from-indigo-400 to-purple-600">
            On-Chain Wallets
          </h2>
          <p className="text-gray-400 mt-2">
            Connect your Cardano wallet and try to unlock these wallets using secret phrases. 
            Choose wisely - only one is real!
          </p>
        </div>
        
        <div className="mt-4 md:mt-0">
          <ConnectWalletButton />
        </div>
      </div>

      {!isWalletAvailable && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-amber-900/30 border border-amber-700 text-amber-200 p-4 rounded-lg mb-6"
        >
          <h3 className="text-lg font-semibold mb-2">No Cardano Wallet Detected</h3>
          <p>
            To interact with the Cardano blockchain, you need to install a compatible wallet extension. 
            We recommend <a href="https://namiwallet.io/" target="_blank" rel="noreferrer" className="text-blue-400 hover:underline">Nami</a> or {' '}
            <a href="https://eternl.io/" target="_blank" rel="noreferrer" className="text-blue-400 hover:underline">Eternl</a>.
          </p>
        </motion.div>
      )}

      {isWalletAvailable && !connectedWallet && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-indigo-900/30 border border-indigo-700 text-indigo-200 p-4 rounded-lg mb-6"
        >
          <h3 className="text-lg font-semibold mb-2">Connect Your Wallet</h3>
          <p>
            Connect your Cardano wallet to interact with the RugRun game and attempt to unlock wallets.
          </p>
        </motion.div>
      )}

      {error && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-red-900/30 border border-red-700 text-red-200 p-4 rounded-lg mb-6"
        >
          <h3 className="text-lg font-semibold mb-2">Error</h3>
          <p>{error}</p>
        </motion.div>
      )}

      {isLoading && connectedWallet && (
        <div className="flex justify-center items-center my-16">
          <svg className="animate-spin h-12 w-12 text-indigo-500" fill="none" viewBox="0 0 24 24">
            <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
            <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
          </svg>
          <span className="ml-3 text-lg text-gray-300">Loading wallets...</span>
        </div>
      )}

      {connectedWallet && !isLoading && gameWallets.length === 0 && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-gray-800 border border-gray-700 p-6 rounded-lg text-center"
        >
          <svg className="w-16 h-16 mx-auto text-gray-600" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
          <h3 className="text-xl font-semibold mt-4 text-gray-300">No Wallets Found</h3>
          <p className="text-gray-400 mt-2">
            No active wallets were found for the current game session.
          </p>
        </motion.div>
      )}

      {connectedWallet && !isLoading && gameWallets.length > 0 && (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {gameWallets.map((wallet, index) => (
            <WalletItem
              key={wallet.address}
              wallet={wallet}
              index={index}
            />
          ))}
        </div>
      )}

      {connectedWallet && gameWallets.length > 0 && (
        <div className="mt-8 p-4 bg-gray-800/50 border border-gray-700 rounded-lg">
          <h3 className="text-lg font-semibold text-gray-300 mb-2">How to Play</h3>
          <ol className="list-decimal list-inside text-gray-400 space-y-2">
            <li>Examine the wallets and their balances carefully.</li>
            <li>Look for clues in the wallet addresses and token amounts.</li>
            <li>Try to identify the real wallet among the decoys.</li>
            <li>Enter the correct secret phrase to unlock the wallet.</li>
            <li>If successful, you&apos;ll receive tokens minus a small fee!</li>
          </ol>
          <div className="mt-4 text-sm text-amber-400">
            <strong>Hint:</strong> The secret phrase for the demo is &quot;<code>haskellrocks</code>&quot;
          </div>
        </div>
      )}
    </div>
  );
};

export default Wallets; 