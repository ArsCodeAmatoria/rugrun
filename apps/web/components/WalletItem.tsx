'use client';

import React, { useState } from 'react';
import { motion } from 'framer-motion';
import { WalletData, WalletType, RugStatus } from '@/utils/haskellSimulation';
import { useGame } from '@/providers/GameProvider';
import { FaLock, FaLockOpen, FaKey } from 'react-icons/fa';
import { HiCheck, HiX, HiRefresh } from 'react-icons/hi';

interface WalletItemProps {
  wallet: WalletData;
  index: number;
}

export default function WalletItem({ wallet, index }: WalletItemProps) {
  const [secret, setSecret] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState('');
  const { attemptUnlock, userAttempts } = useGame();
  
  // Check if wallet has been successfully unlocked
  const isUnlocked = userAttempts.some(
    attempt => attempt.walletAddress === wallet.address && attempt.success
  );

  const handleUnlock = async () => {
    if (!secret.trim()) {
      setError('Please enter a secret phrase');
      return;
    }

    setIsSubmitting(true);
    setError('');
    
    try {
      const result = await attemptUnlock(wallet.address, secret.trim());
      if (!result.success) {
        setError('Incorrect secret phrase');
      }
    } catch (err) {
      setError('An error occurred');
      console.error(err);
    } finally {
      setIsSubmitting(false);
    }
  };

  // Check if wallet is a real wallet and if it has been claimed
  const isRealAndClaimed = 
    wallet.type === WalletType.RealWallet && 
    'rugStatus' in wallet.datum && 
    wallet.datum.rugStatus === RugStatus.Claimed;

  return (
    <motion.div
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ delay: index * 0.1 }}
      className={`rounded-lg shadow-md p-5 ${
        isUnlocked || isRealAndClaimed
          ? 'bg-gradient-to-r from-green-900/30 to-emerald-900/30 backdrop-blur-md border border-green-500/30'
          : 'bg-gradient-to-r from-indigo-900/30 to-purple-900/30 backdrop-blur-md border border-indigo-500/30'
      }`}
    >
      <div className="flex justify-between items-center mb-3">
        <div className="flex items-center">
          {isUnlocked || isRealAndClaimed ? (
            <FaLockOpen className="text-green-400 mr-2" />
          ) : (
            <FaLock className="text-indigo-400 mr-2" />
          )}
          <h3 className="text-lg font-semibold text-white truncate">
            Wallet {index + 1}
          </h3>
        </div>

        {(isUnlocked || isRealAndClaimed) && (
          <span className="px-2 py-1 text-xs font-medium rounded-full bg-green-600/40 text-green-200">
            Unlocked
          </span>
        )}
      </div>

      <div className="space-y-3">
        <div>
          <p className="text-xs text-indigo-300 mb-1">Address</p>
          <p className="font-mono text-sm bg-indigo-950/50 p-2 rounded text-indigo-200 break-all">
            {wallet.address}
          </p>
        </div>

        {isUnlocked || isRealAndClaimed ? (
          <>
            <div>
              <p className="text-xs text-green-300 mb-1">Balance</p>
              <p className="font-bold text-xl text-green-200">{wallet.balance} HBAR</p>
            </div>
            <div>
              <p className="text-xs text-green-300 mb-1">Secret Phrase</p>
              <p className="font-mono text-sm bg-green-950/50 p-2 rounded text-green-200 break-all">
                {userAttempts.find(a => a.walletAddress === wallet.address && a.success)?.attempt || "Unknown"}
              </p>
            </div>
          </>
        ) : (
          <>
            <div>
              <p className="text-xs text-indigo-300 mb-1">Clue</p>
              <p className="italic text-sm text-indigo-200">{wallet.clue}</p>
            </div>
            <div className="mt-4">
              <div className="flex items-center mb-2">
                <FaKey className="text-indigo-400 mr-2" />
                <p className="text-sm text-indigo-200">Enter Secret Phrase</p>
              </div>
              <div className="flex space-x-2">
                <input
                  type="text"
                  value={secret}
                  onChange={(e) => setSecret(e.target.value)}
                  placeholder="Secret phrase..."
                  className="flex-1 p-2 rounded bg-indigo-950/70 border border-indigo-700/50 text-white placeholder-indigo-400 text-sm focus:outline-none focus:ring-2 focus:ring-indigo-500"
                  disabled={isSubmitting}
                />
                <button
                  onClick={handleUnlock}
                  disabled={isSubmitting}
                  className={`p-2 rounded ${
                    isSubmitting
                      ? 'bg-indigo-700/50 cursor-not-allowed'
                      : 'bg-indigo-600 hover:bg-indigo-700'
                  } transition-colors`}
                >
                  {isSubmitting ? (
                    <HiRefresh className="animate-spin text-white" />
                  ) : (
                    <HiCheck className="text-white" />
                  )}
                </button>
              </div>
              {error && (
                <p className="mt-2 text-red-400 text-xs flex items-center">
                  <HiX className="mr-1" /> {error}
                </p>
              )}
            </div>
          </>
        )}
      </div>
    </motion.div>
  );
} 