'use client';

import React, { useState } from 'react';
import { motion } from 'framer-motion';
import { useCardano } from '@/providers/CardanoProvider';

const ConnectWalletButton: React.FC = () => {
  const { 
    isWalletAvailable, 
    connectedWallet, 
    connectWallet, 
    disconnectWallet, 
    availableWallets,
    isLoading 
  } = useCardano();
  
  const [showWalletList, setShowWalletList] = useState(false);

  // Format wallet address for display
  const formatAddress = (address: string) => {
    if (!address) return '';
    return `${address.slice(0, 8)}...${address.slice(-8)}`;
  };

  // Handle wallet selection
  const handleSelectWallet = async (walletName: string) => {
    setShowWalletList(false);
    await connectWallet(walletName);
  };

  if (!isWalletAvailable) {
    return (
      <motion.button
        whileHover={{ scale: 1.02 }}
        whileTap={{ scale: 0.98 }}
        className="bg-gray-800 text-gray-400 px-5 py-2 rounded-lg border border-gray-700 cursor-not-allowed"
        disabled
      >
        No Wallet Available
      </motion.button>
    );
  }

  if (connectedWallet) {
    return (
      <div className="flex items-center">
        <div className="mr-3 flex items-center">
          <div className="h-2 w-2 rounded-full bg-green-500 mr-2"></div>
          <span className="text-sm text-gray-400">
            {formatAddress(connectedWallet.address)}
          </span>
        </div>
        <motion.button
          whileHover={{ scale: 1.02 }}
          whileTap={{ scale: 0.98 }}
          onClick={disconnectWallet}
          className="bg-red-600/20 hover:bg-red-600/30 border border-red-700/50 text-red-400 px-3 py-1 text-sm rounded-lg"
        >
          Disconnect
        </motion.button>
      </div>
    );
  }

  return (
    <div className="relative">
      <motion.button
        whileHover={{ scale: 1.02 }}
        whileTap={{ scale: 0.98 }}
        onClick={() => setShowWalletList(!showWalletList)}
        disabled={isLoading}
        className={`bg-gradient-to-r from-indigo-600 to-purple-600 text-white px-5 py-2 rounded-lg flex items-center shadow-lg ${
          isLoading ? 'opacity-70 cursor-not-allowed' : 'hover:from-indigo-500 hover:to-purple-500'
        }`}
      >
        {isLoading ? (
          <>
            <svg className="animate-spin -ml-1 mr-2 h-4 w-4 text-white" fill="none" viewBox="0 0 24 24">
              <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
              <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            Connecting...
          </>
        ) : (
          <>
            <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M17 9V7a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2m2 4h10a2 2 0 002-2v-6a2 2 0 00-2-2H9a2 2 0 00-2 2v6a2 2 0 002 2zm7-5a2 2 0 11-4 0 2 2 0 014 0z" />
            </svg>
            Connect Wallet
          </>
        )}
      </motion.button>

      {showWalletList && (
        <motion.div
          initial={{ opacity: 0, y: 10 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: 10 }}
          className="absolute right-0 mt-2 w-60 bg-gray-800 border border-gray-700 rounded-lg shadow-xl z-10"
        >
          <div className="p-2">
            <div className="text-sm text-gray-400 mb-2 px-3 py-2 border-b border-gray-700">
              Available Wallets
            </div>
            <div className="space-y-1">
              {availableWallets.map(wallet => (
                <button
                  key={wallet.name}
                  onClick={() => handleSelectWallet(wallet.name)}
                  className="w-full text-left px-3 py-2 text-sm text-gray-300 hover:bg-gray-700 rounded flex items-center"
                >
                  <img 
                    src={wallet.icon || '/wallet-icon.svg'} 
                    alt={wallet.name} 
                    className="w-5 h-5 mr-2"
                    onError={(e) => {
                      (e.target as HTMLImageElement).src = '/wallet-icon.svg';
                    }}
                  />
                  {wallet.name}
                </button>
              ))}
              
              {availableWallets.length === 0 && (
                <div className="px-3 py-2 text-sm text-gray-500 italic">
                  No wallets detected
                </div>
              )}
            </div>
          </div>
        </motion.div>
      )}
    </div>
  );
};

export default ConnectWalletButton; 