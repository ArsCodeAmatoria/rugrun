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
      
      <main className="flex-grow pt-20">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3 }}
            className="text-center mb-12"
          >
            <h1 className="text-3xl font-bold font-orbitron tracking-tight">
              <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-white">
                Top Hunters
              </span>
            </h1>
            <p className="mt-2 text-zinc-400 max-w-xl mx-auto">
              The most persistent challengers trying to find the real wallet and unlock the tokens
            </p>
          </motion.div>
          
          <Card className="border-zinc-800 bg-zinc-900/70 backdrop-blur-sm overflow-hidden">
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle className="text-xl text-primary font-orbitron flex items-center">
                  <Award className="mr-2 h-5 w-5" /> 
                  Leaderboard
                </CardTitle>
              </div>
            </CardHeader>
            <CardContent>
              <div className="overflow-x-auto">
                <table className="w-full">
                  <thead>
                    <tr className="border-b border-zinc-800">
                      <th className="text-left py-3 px-4 text-xs text-zinc-500">Rank</th>
                      <th className="text-left py-3 px-4 text-xs text-zinc-500">Wallet Address</th>
                      <th className="text-left py-3 px-4 text-xs text-zinc-500">
                        <button 
                          onClick={() => setSortBy('attempts')} 
                          className={`flex items-center ${sortBy === 'attempts' ? 'text-primary' : ''}`}
                        >
                          Attempts
                          {sortBy === 'attempts' && <ChevronUp className="ml-1 h-3 w-3" />}
                        </button>
                      </th>
                      <th className="text-left py-3 px-4 text-xs text-zinc-500">
                        <button 
                          onClick={() => setSortBy('lastAttempt')} 
                          className={`flex items-center ${sortBy === 'lastAttempt' ? 'text-primary' : ''}`}
                        >
                          <Clock className="mr-1 h-3 w-3" /> 
                          Last Attempt
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
                        <td className="py-4 px-4">
                          <div className={`flex items-center justify-center w-8 h-8 rounded-full 
                            ${index === 0 ? 'bg-yellow-500/20 text-yellow-500' : 
                              index === 1 ? 'bg-zinc-400/20 text-zinc-400' : 
                              index === 2 ? 'bg-amber-700/20 text-amber-700' : 
                              'bg-zinc-900 text-zinc-500'}`}
                          >
                            {index + 1}
                          </div>
                        </td>
                        <td className="py-4 px-4 font-mono">
                          {truncateAddress(entry.address, 8, 8)}
                        </td>
                        <td className="py-4 px-4 font-mono">
                          <span className="text-primary font-bold">{entry.attempts}</span>
                        </td>
                        <td className="py-4 px-4 text-zinc-400">
                          {formatTimestamp(entry.lastAttempt)}
                        </td>
                      </motion.tr>
                    ))}
                    
                    {/* Empty State */}
                    {sortedLeaderboard.length === 0 && (
                      <tr>
                        <td colSpan={4} className="py-8 text-center text-zinc-500">
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
          <div className="mt-12 grid grid-cols-1 md:grid-cols-3 gap-6">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.3 }}
              className="bg-black/30 rounded-xl p-6 border border-zinc-800"
            >
              <h3 className="text-lg font-orbitron mb-2 text-zinc-300">Total Attempts</h3>
              <p className="text-3xl font-bold text-primary">
                {leaderboard.reduce((sum, entry) => sum + entry.attempts, 0)}
              </p>
            </motion.div>
            
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.4 }}
              className="bg-black/30 rounded-xl p-6 border border-zinc-800"
            >
              <h3 className="text-lg font-orbitron mb-2 text-zinc-300">Unique Hunters</h3>
              <p className="text-3xl font-bold text-primary">
                {leaderboard.length}
              </p>
            </motion.div>
            
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.3, delay: 0.5 }}
              className="bg-black/30 rounded-xl p-6 border border-zinc-800"
            >
              <h3 className="text-lg font-orbitron mb-2 text-zinc-300">Success Rate</h3>
              <p className="text-3xl font-bold text-primary">
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