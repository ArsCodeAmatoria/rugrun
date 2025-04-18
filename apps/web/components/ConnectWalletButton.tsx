'use client';

import React, { useState } from 'react';
import { useCardano } from '@/providers/CardanoProvider';
import { motion, AnimatePresence } from 'framer-motion';

// Supported wallet types
export enum WalletType {
  Nami = 'nami',
  Eternl = 'eternl',
  Flint = 'flint',
  Typhon = 'typhon',
  Yoroi = 'yoroi'
}

// Dropdown item interface
interface WalletOption {
  type: WalletType;
  name: string;
  icon: string;
}

// Available wallets
const WALLET_OPTIONS: WalletOption[] = [
  { type: WalletType.Nami, name: 'Nami', icon: '/images/wallets/nami.svg' },
  { type: WalletType.Eternl, name: 'Eternl', icon: '/images/wallets/eternl.svg' },
  { type: WalletType.Flint, name: 'Flint', icon: '/images/wallets/flint.svg' },
  { type: WalletType.Typhon, name: 'Typhon', icon: '/images/wallets/typhon.svg' },
  { type: WalletType.Yoroi, name: 'Yoroi', icon: '/images/wallets/yoroi.svg' }
];

const ConnectWalletButton: React.FC = () => {
  const { isWalletAvailable, connectWallet, connectedWallet, isLoading, error } = useCardano();
  const [isOpen, setIsOpen] = useState(false);

  // Handle wallet connection
  const handleConnect = async (walletType: WalletType) => {
    try {
      await connectWallet(walletType);
      setIsOpen(false);
    } catch (error) {
      console.error('Error connecting wallet:', error);
    }
  };

  return (
    <div className="relative">
      {/* Main button */}
      <motion.button
        whileHover={{ scale: 1.05 }}
        whileTap={{ scale: 0.95 }}
        onClick={() => setIsOpen(!isOpen)}
        disabled={isLoading || !isWalletAvailable}
        className={`flex items-center px-4 py-2 rounded-lg font-medium transition-colors ${
          connectedWallet 
            ? 'bg-gradient-to-r from-green-600 to-green-700 text-white hover:from-green-700 hover:to-green-800' 
            : 'bg-gradient-to-r from-indigo-600 to-purple-600 text-white hover:from-indigo-700 hover:to-purple-700'
        } ${(!isWalletAvailable || isLoading) ? 'opacity-60 cursor-not-allowed' : ''}`}
      >
        {isLoading ? (
          <>
            <svg className="animate-spin -ml-1 mr-2 h-4 w-4 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
              <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
              <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            Connecting...
          </>
        ) : connectedWallet ? (
          <>
            <img 
              src={connectedWallet.icon || '/images/wallets/default.svg'} 
              alt={connectedWallet.name} 
              className="w-5 h-5 mr-2" 
            />
            {connectedWallet.name}
          </>
        ) : (
          <>
            <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"></path>
            </svg>
            Connect Wallet
          </>
        )}
      </motion.button>

      {/* Dropdown menu */}
      <AnimatePresence>
        {isOpen && !connectedWallet && (
          <motion.div
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -10 }}
            className="absolute z-20 right-0 mt-2 w-56 rounded-md shadow-lg bg-gray-900 border border-indigo-500/30"
          >
            <div className="py-1 divide-y divide-gray-700">
              {WALLET_OPTIONS.map((wallet) => (
                <motion.button
                  key={wallet.type}
                  whileHover={{ backgroundColor: 'rgba(79, 70, 229, 0.2)' }}
                  onClick={() => handleConnect(wallet.type)}
                  className="flex items-center w-full px-4 py-3 text-left text-white"
                >
                  <img 
                    src={wallet.icon} 
                    alt={wallet.name} 
                    className="w-6 h-6 mr-3" 
                    onError={(e) => {
                      (e.target as HTMLImageElement).src = '/images/wallets/default.svg';
                    }}
                  />
                  <span>{wallet.name}</span>
                </motion.button>
              ))}
            </div>
            {!isWalletAvailable && (
              <div className="px-4 py-2 text-xs text-red-400 bg-red-800/30 border-t border-red-900 rounded-b-md">
                No Cardano wallets detected. Please install a compatible wallet.
              </div>
            )}
            {error && (
              <div className="px-4 py-2 text-xs text-red-400 bg-red-800/30 border-t border-red-900 rounded-b-md">
                {error}
              </div>
            )}
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  );
};

export default ConnectWalletButton; 