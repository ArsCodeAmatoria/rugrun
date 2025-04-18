"use client"

import { useState } from "react"
import { motion } from "framer-motion"
import { Award, ChevronUp, Clock } from "lucide-react"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import Navbar from "@/components/Navbar"
import { leaderboard } from "@/utils/mockWallets"
import { truncateAddress } from "@/lib/utils"
import Footer from "@/components/Footer"

export default function LeaderboardPage() {
  const [sortBy, setSortBy] = useState<'attempts' | 'lastAttempt'>('attempts')
  
  const sortedLeaderboard = [...leaderboard].sort((a, b) => {
    if (sortBy === 'attempts') {
      return b.attempts - a.attempts
    } else {
      return b.lastAttempt - a.lastAttempt
    }
  })
  
  // Format timestamp to readable format
  const formatTimestamp = (timestamp: number) => {
    return new Date(timestamp).toLocaleString()
  }

  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-grow pt-16 sm:pt-20">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-6 sm:py-8">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3 }}
            className="text-center mb-8 sm:mb-12"
          >
            <h1 className="text-2xl sm:text-3xl font-bold font-orbitron tracking-tight">
              <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-white">
                Top Hunters
              </span>
            </h1>
            <p className="mt-1 sm:mt-2 text-sm sm:text-base text-zinc-400 max-w-xl mx-auto px-2">
              The most persistent challengers trying to find the real wallet and unlock the tokens
            </p>
          </motion.div>
          
          <Card className="border-zinc-800 bg-zinc-900/70 backdrop-blur-sm overflow-hidden">
            <CardHeader className="px-3 sm:px-6 py-3 sm:py-4">
              <div className="flex items-center justify-between">
                <CardTitle className="text-lg sm:text-xl text-primary font-orbitron flex items-center">
                  <Award className="mr-2 h-4 w-4 sm:h-5 sm:w-5" /> 
                  Leaderboard
                </CardTitle>
              </div>
            </CardHeader>
            <CardContent className="px-2 sm:px-6 py-2 sm:py-3">
              <div className="overflow-x-auto -mx-2 sm:mx-0">
                <table className="w-full">
                  <thead>
                    <tr className="border-b border-zinc-800">
                      <th className="text-left py-2 sm:py-3 px-2 sm:px-4 text-xs text-zinc-500">Rank</th>
                      <th className="text-left py-2 sm:py-3 px-2 sm:px-4 text-xs text-zinc-500">Wallet Address</th>
                      <th className="text-left py-2 sm:py-3 px-2 sm:px-4 text-xs text-zinc-500">
                        <button 
                          onClick={() => setSortBy('attempts')} 
                          className={`flex items-center ${sortBy === 'attempts' ? 'text-primary' : ''}`}
                        >
                          Attempts
                          {sortBy === 'attempts' && <ChevronUp className="ml-1 h-3 w-3" />}
                        </button>
                      </th>
                      <th className="text-left py-2 sm:py-3 px-2 sm:px-4 text-xs text-zinc-500">
                        <button 
                          onClick={() => setSortBy('lastAttempt')} 
                          className={`flex items-center ${sortBy === 'lastAttempt' ? 'text-primary' : ''}`}
                        >
                          <Clock className="mr-1 h-3 w-3" /> 
                          <span className="hidden sm:inline">Last Attempt</span>
                          <span className="sm:hidden">Time</span>
                          {sortBy === 'lastAttempt' && <ChevronUp className="ml-1 h-3 w-3" />}
                        </button>
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {sortedLeaderboard.map((entry, index) => (
                      <motion.tr 
                        key={entry.address}
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ duration: 0.3, delay: index * 0.1 }}
                        className="border-b border-zinc-800/50 hover:bg-black/20 transition-colors"
                      >
                        <td className="py-2 sm:py-4 px-2 sm:px-4">
                          <div className={`flex items-center justify-center w-6 h-6 sm:w-8 sm:h-8 rounded-full text-xs sm:text-sm
                            ${index === 0 ? 'bg-yellow-500/20 text-yellow-500' : 
                              index === 1 ? 'bg-zinc-400/20 text-zinc-400' : 
                              index === 2 ? 'bg-amber-700/20 text-amber-700' : 
                              'bg-zinc-900 text-zinc-500'}`}
                          >
                            {index + 1}
                          </div>
                        </td>
                        <td className="py-2 sm:py-4 px-2 sm:px-4 font-mono text-xs sm:text-sm">
                          <span className="hidden sm:inline">{truncateAddress(entry.address, 8, 8)}</span>
                          <span className="sm:hidden">{truncateAddress(entry.address, 4, 4)}</span>
                        </td>
                        <td className="py-2 sm:py-4 px-2 sm:px-4 font-mono text-xs sm:text-sm">
                          <span className="text-primary font-bold">{entry.attempts}</span>
                        </td>
                        <td className="py-2 sm:py-4 px-2 sm:px-4 text-zinc-400 text-xs sm:text-sm">
                          <span className="hidden sm:inline">{formatTimestamp(entry.lastAttempt)}</span>
                          <span className="sm:hidden">{new Date(entry.lastAttempt).toLocaleDateString()}</span>
                        </td>
                      </motion.tr>
                    ))}
                    
                    {/* Empty State */}
                    {sortedLeaderboard.length === 0 && (
                      <tr>
                        <td colSpan={4} className="py-6 sm:py-8 text-center text-zinc-500 text-sm">
                          No attempts recorded yet
                        </td>
                      </tr>
                    )}
                  </tbody>
                </table>
              </div>
            </CardContent>
          </Card>
          
          {/* Game Stats */}
          <div className="mt-8 sm:mt-12 grid grid-cols-1 sm:grid-cols-3 gap-4 sm:gap-6">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.3 }}
              className="bg-black/30 rounded-xl p-4 sm:p-6 border border-zinc-800"
            >
              <h3 className="text-base sm:text-lg font-orbitron mb-1 sm:mb-2 text-zinc-300">Total Attempts</h3>
              <p className="text-2xl sm:text-3xl font-bold text-primary">
                {leaderboard.reduce((sum, entry) => sum + entry.attempts, 0)}
              </p>
            </motion.div>
            
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.4 }}
              className="bg-black/30 rounded-xl p-4 sm:p-6 border border-zinc-800"
            >
              <h3 className="text-base sm:text-lg font-orbitron mb-1 sm:mb-2 text-zinc-300">Unique Hunters</h3>
              <p className="text-2xl sm:text-3xl font-bold text-primary">
                {leaderboard.length}
              </p>
            </motion.div>
            
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.5 }}
              className="bg-black/30 rounded-xl p-4 sm:p-6 border border-zinc-800"
            >
              <h3 className="text-base sm:text-lg font-orbitron mb-1 sm:mb-2 text-zinc-300">Success Rate</h3>
              <p className="text-2xl sm:text-3xl font-bold text-primary">
                0%
              </p>
              <p className="text-xs text-zinc-500 mt-1">No successful claims yet</p>
            </motion.div>
          </div>
        </div>
      </main>
      
      <Footer />
    </div>
  )
} 