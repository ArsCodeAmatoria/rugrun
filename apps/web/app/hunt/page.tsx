"use client"

import { useState, useEffect } from "react"
import { motion, AnimatePresence } from "framer-motion"
import { Search, Clock, List, Sparkles } from "lucide-react"
import Navbar from "@/components/Navbar"
import WalletCard from "@/components/WalletCard"
import { wallets, attemptHistory } from "@/utils/mockWallets"
import { truncateAddress } from "@/lib/utils"
import Footer from "@/components/Footer"
import TypingText from "@/components/ui/typing-text"
import GlowCard from "@/components/ui/glow-card"
import { Spinner } from "@/components/ui/spinner"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"

export default function HuntPage() {
  const [attempts, setAttempts] = useState(attemptHistory)
  const [searchQuery, setSearchQuery] = useState("")
  const [isLoading, setIsLoading] = useState(true)
  const [introCompleted, setIntroCompleted] = useState(false)
  
  // Simulate loading state
  useEffect(() => {
    const timer = setTimeout(() => {
      setIsLoading(false);
    }, 1500);
    
    return () => clearTimeout(timer);
  }, []);
  
  const filteredWallets = wallets.filter((wallet) =>
    wallet.address.toLowerCase().includes(searchQuery.toLowerCase()) ||
    wallet.clue.toLowerCase().includes(searchQuery.toLowerCase())
  )
  
  // Format timestamp to readable format
  const formatTimestamp = (timestamp: number) => {
    return new Date(timestamp).toLocaleString()
  }
  
  const handleAttemptUnlock = (address: string, secretPhrase: string) => {
    // In a real app, this would call a contract interaction
    console.log(`Attempting to unlock ${address} with phrase: ${secretPhrase}`)
    
    // For demo, we'll just add it to our attempt history
    const newAttempt = {
      walletAddress: address,
      userAddress: "addr1qym5htxrsdpjq60kk5cgg8gxxgj6m3r4pqk4xenh4myt3fc08cgvsj4xvwq66n4u46a2jzyzn6jndu8z3qhcn8v26raqx8f93n", // Demo user address
      timestamp: Date.now(),
      attempt: secretPhrase,
      status: "failed" as const // All attempts fail except for the real one with correct phrase
    }
    
    setAttempts((prev) => [newAttempt, ...prev])
  }

  // Staggered reveal animation for wallet cards
  const containerVariants = {
    hidden: { opacity: 0 },
    show: {
      opacity: 1,
      transition: {
        staggerChildren: 0.1
      }
    }
  }

  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-grow pt-16 sm:pt-20">
        <AnimatePresence>
          {isLoading ? (
            <motion.div 
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              className="fixed inset-0 flex items-center justify-center z-50"
            >
              <div className="bg-black/50 backdrop-blur-lg p-8 rounded-xl border border-primary/20 flex flex-col items-center">
                <Spinner size="lg" />
                <p className="mt-4 text-primary font-orbitron">Scanning Blockchain...</p>
              </div>
            </motion.div>
          ) : (
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-8">
              <div className="space-y-4 sm:space-y-6">
                <div className="flex flex-col md:flex-row justify-between items-start md:items-center gap-3 sm:gap-4">
                  <motion.div
                    initial={{ opacity: 0, y: 20 }}
                    animate={{ opacity: 1, y: 0 }}
                    transition={{ duration: 0.3 }}
                  >
                    <h1 className="text-2xl sm:text-3xl font-bold font-orbitron tracking-tight">
                      <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-white flex items-center">
                        <Sparkles className="h-5 w-5 mr-2 text-primary" />
                        Hunt the Rug
                      </span>
                    </h1>
                    <div className="h-6 mt-1 sm:mt-2">
                      {!introCompleted ? (
                        <TypingText 
                          text="Find the real wallet and unlock it with the correct phrase" 
                          className="text-sm sm:text-base text-zinc-400"
                          typingSpeed={30}
                          onComplete={() => setIntroCompleted(true)}
                        />
                      ) : (
                        <p className="text-sm sm:text-base text-zinc-400">
                          Find the real wallet and unlock it with the correct phrase
                        </p>
                      )}
                    </div>
                  </motion.div>
                  
                  <motion.div 
                    className="w-full md:w-72 mt-2 md:mt-0"
                    initial={{ opacity: 0, scale: 0.95 }}
                    animate={{ opacity: 1, scale: 1 }}
                    transition={{ duration: 0.3, delay: 0.2 }}
                  >
                    <div className="relative">
                      <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-zinc-500" />
                      <motion.input
                        type="text"
                        placeholder="Search wallet address or clue..."
                        value={searchQuery}
                        onChange={(e) => setSearchQuery(e.target.value)}
                        className="pl-9 pr-4 py-2 w-full bg-black/30 border border-zinc-800 rounded-lg focus:outline-none focus:ring-1 focus:ring-primary text-sm"
                        whileFocus={{ scale: 1.01, borderColor: 'rgba(147, 51, 234, 0.8)' }}
                        transition={{ duration: 0.2 }}
                      />
                    </div>
                  </motion.div>
                </div>
                
                {/* Wallets Grid */}
                <motion.div 
                  className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4 sm:gap-6"
                  variants={containerVariants}
                  initial="hidden"
                  animate="show"
                >
                  {filteredWallets.map((wallet) => (
                    <WalletCard 
                      key={wallet.address} 
                      wallet={wallet} 
                      onAttemptUnlock={handleAttemptUnlock}
                    />
                  ))}
                </motion.div>
                
                {/* Attempt History */}
                <motion.div 
                  className="mt-12 sm:mt-16"
                  initial={{ opacity: 0, y: 30 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ duration: 0.5, delay: 0.3 }}
                >
                  <GlowCard glowColor="rgba(6, 182, 212, 0.3)">
                    <CardHeader className="px-3 sm:px-6 py-3 sm:py-4">
                      <div className="flex items-center justify-between">
                        <CardTitle className="text-lg sm:text-xl text-primary font-orbitron">
                          <List className="inline-block mr-2 h-4 w-4 sm:h-5 sm:w-5" /> 
                          Recent Attempts
                        </CardTitle>
                      </div>
                    </CardHeader>
                    <CardContent className="px-3 sm:px-6 py-2 sm:py-3">
                      <div className="overflow-x-auto -mx-3 sm:-mx-0">
                        <table className="w-full">
                          <thead>
                            <tr className="border-b border-zinc-800">
                              <th className="text-left py-2 sm:py-3 px-2 text-xs text-zinc-500">Wallet</th>
                              <th className="text-left py-2 sm:py-3 px-2 text-xs text-zinc-500">User</th>
                              <th className="text-left py-2 sm:py-3 px-2 text-xs text-zinc-500">Attempt</th>
                              <th className="text-left py-2 sm:py-3 px-2 text-xs text-zinc-500">
                                <Clock className="inline-block mr-1 h-3 w-3" /> 
                                Timestamp
                              </th>
                              <th className="text-left py-2 sm:py-3 px-2 text-xs text-zinc-500">Status</th>
                            </tr>
                          </thead>
                          <tbody>
                            <AnimatePresence>
                              {attempts.map((attempt, index) => (
                                <motion.tr 
                                  key={index + attempt.timestamp}
                                  initial={{ opacity: 0, backgroundColor: "rgba(147, 51, 234, 0.2)" }}
                                  animate={{ opacity: 1, backgroundColor: "rgba(0, 0, 0, 0)" }}
                                  exit={{ opacity: 0 }}
                                  transition={{ duration: 0.5 }}
                                  className="border-b border-zinc-800/50 hover:bg-black/20 transition-colors"
                                >
                                  <td className="py-2 sm:py-3 px-2 text-xs sm:text-sm">
                                    {truncateAddress(attempt.walletAddress)}
                                  </td>
                                  <td className="py-2 sm:py-3 px-2 text-xs sm:text-sm">
                                    {truncateAddress(attempt.userAddress)}
                                  </td>
                                  <td className="py-2 sm:py-3 px-2 text-xs sm:text-sm font-mono">
                                    {attempt.attempt}
                                  </td>
                                  <td className="py-2 sm:py-3 px-2 text-xs sm:text-sm text-zinc-400">
                                    {formatTimestamp(attempt.timestamp)}
                                  </td>
                                  <td className="py-2 sm:py-3 px-2 text-xs sm:text-sm">
                                    <span className={`inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium ${
                                      attempt.status === "failed" 
                                        ? "bg-red-900/30 text-red-400" 
                                        : attempt.status === "success" 
                                        ? "bg-green-900/30 text-green-400" 
                                        : "bg-yellow-900/30 text-yellow-400"
                                    }`}>
                                      {attempt.status}
                                    </span>
                                  </td>
                                </motion.tr>
                              ))}
                            </AnimatePresence>
                          </tbody>
                        </table>
                      </div>
                    </CardContent>
                  </GlowCard>
                </motion.div>
              </div>
            </div>
          )}
        </AnimatePresence>
      </main>
      
      <Footer />
    </div>
  )
} 