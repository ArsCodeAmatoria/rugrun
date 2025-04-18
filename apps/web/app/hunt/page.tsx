"use client"

import { useState } from "react"
import { motion } from "framer-motion"
import { Search, Clock, List } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import Navbar from "@/components/Navbar"
import WalletCard from "@/components/WalletCard"
import { wallets, attemptHistory } from "@/utils/mockWallets"
import { truncateAddress } from "@/lib/utils"
import Footer from "@/components/Footer"

export default function HuntPage() {
  const [attempts, setAttempts] = useState(attemptHistory)
  const [searchQuery, setSearchQuery] = useState("")
  
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

  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-grow pt-20">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="space-y-6">
            <div className="flex flex-col md:flex-row justify-between items-start md:items-center gap-4">
              <motion.div
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.3 }}
              >
                <h1 className="text-3xl font-bold font-orbitron tracking-tight">
                  <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-white">
                    Hunt the Rug
                  </span>
                </h1>
                <p className="mt-2 text-zinc-400">
                  Find the real wallet and unlock it with the correct phrase
                </p>
              </motion.div>
              
              <div className="w-full md:w-72">
                <div className="relative">
                  <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-zinc-500" />
                  <input
                    type="text"
                    placeholder="Search wallet address or clue..."
                    value={searchQuery}
                    onChange={(e) => setSearchQuery(e.target.value)}
                    className="pl-9 pr-4 py-2 w-full bg-black/30 border border-zinc-800 rounded-lg focus:outline-none focus:ring-1 focus:ring-primary"
                  />
                </div>
              </div>
            </div>
            
            {/* Wallets Grid */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {filteredWallets.map((wallet) => (
                <WalletCard 
                  key={wallet.address} 
                  wallet={wallet} 
                  onAttemptUnlock={handleAttemptUnlock}
                />
              ))}
            </div>
            
            {/* Attempt History */}
            <div className="mt-16">
              <Card className="border-zinc-800 bg-zinc-900/70 backdrop-blur-sm">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <CardTitle className="text-xl text-primary font-orbitron">
                      <List className="inline-block mr-2 h-5 w-5" /> 
                      Recent Attempts
                    </CardTitle>
                  </div>
                </CardHeader>
                <CardContent>
                  <div className="overflow-x-auto">
                    <table className="w-full">
                      <thead>
                        <tr className="border-b border-zinc-800">
                          <th className="text-left py-3 px-2 text-xs text-zinc-500">Wallet</th>
                          <th className="text-left py-3 px-2 text-xs text-zinc-500">User</th>
                          <th className="text-left py-3 px-2 text-xs text-zinc-500">Attempt</th>
                          <th className="text-left py-3 px-2 text-xs text-zinc-500">
                            <Clock className="inline-block mr-1 h-3 w-3" /> 
                            Timestamp
                          </th>
                          <th className="text-left py-3 px-2 text-xs text-zinc-500">Status</th>
                        </tr>
                      </thead>
                      <tbody>
                        {attempts.map((attempt, index) => (
                          <tr 
                            key={index} 
                            className="border-b border-zinc-800/50 hover:bg-black/20 transition-colors"
                          >
                            <td className="py-3 px-2 text-sm">
                              {truncateAddress(attempt.walletAddress)}
                            </td>
                            <td className="py-3 px-2 text-sm">
                              {truncateAddress(attempt.userAddress)}
                            </td>
                            <td className="py-3 px-2 text-sm font-mono">
                              {attempt.attempt}
                            </td>
                            <td className="py-3 px-2 text-sm text-zinc-400">
                              {formatTimestamp(attempt.timestamp)}
                            </td>
                            <td className="py-3 px-2 text-sm">
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
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </CardContent>
              </Card>
            </div>
          </div>
        </div>
      </main>
      
      <Footer />
    </div>
  )
} 