"use client"

import { useState } from "react"
import { motion } from "framer-motion"
import { Copy, ExternalLink, Lock, Unlock } from "lucide-react"
import { truncateAddress } from "@/lib/utils"
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import type { Wallet } from "@/utils/mockWallets"

interface WalletCardProps {
  wallet: Wallet
  onAttemptUnlock: (address: string, secretPhrase: string) => void
}

export default function WalletCard({ wallet, onAttemptUnlock }: WalletCardProps) {
  const [secretPhrase, setSecretPhrase] = useState("")
  const [copied, setCopied] = useState(false)
  const [expanded, setExpanded] = useState(false)

  const copyToClipboard = () => {
    navigator.clipboard.writeText(wallet.address)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const handleUnlockAttempt = () => {
    if (secretPhrase.trim()) {
      onAttemptUnlock(wallet.address, secretPhrase)
      setSecretPhrase("")
    }
  }

  return (
    <motion.div
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.3 }}
      whileHover={{ scale: 1.02 }}
    >
      <Card className="border-zinc-800 bg-zinc-900/70 backdrop-blur-sm overflow-hidden">
        <CardHeader>
          <div className="flex justify-between items-center">
            <CardTitle className="text-xl text-primary">
              Wallet {truncateAddress(wallet.address, 4, 4)}
            </CardTitle>
            <Button 
              size="icon" 
              variant="ghost" 
              onClick={copyToClipboard} 
              className="h-8 w-8"
            >
              {copied ? <span className="text-xs">Copied!</span> : <Copy size={16} />}
            </Button>
          </div>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="bg-black/30 rounded-lg p-3 border border-zinc-800">
              <p className="text-sm text-zinc-400 italic">&quot;{wallet.clue}&quot;</p>
            </div>
            
            <div>
              <button 
                onClick={() => setExpanded(!expanded)}
                className="text-sm text-primary hover:text-primary/80 flex items-center gap-1"
              >
                {expanded ? "Hide Unlock Interface" : "Attempt Unlock"}
                {expanded ? <Unlock size={14} /> : <Lock size={14} />}
              </button>
            </div>

            {expanded && (
              <motion.div
                initial={{ height: 0, opacity: 0 }}
                animate={{ height: "auto", opacity: 1 }}
                exit={{ height: 0, opacity: 0 }}
                transition={{ duration: 0.2 }}
                className="space-y-3"
              >
                <input
                  type="text"
                  value={secretPhrase}
                  onChange={(e) => setSecretPhrase(e.target.value)}
                  placeholder="Enter secret phrase or key"
                  className="w-full px-3 py-2 bg-black/50 border border-zinc-800 rounded-lg text-sm text-white focus:outline-none focus:ring-1 focus:ring-primary"
                />
                <Button 
                  variant="glow" 
                  size="sm" 
                  className="w-full"
                  onClick={handleUnlockAttempt}
                  disabled={!secretPhrase.trim()}
                >
                  Attempt Unlock
                </Button>
              </motion.div>
            )}
          </div>
        </CardContent>
        <CardFooter className="border-t border-zinc-800 pt-3">
          <a 
            href={`https://explorer.cardano.org/en/address/${wallet.address}`}
            target="_blank" 
            rel="noopener noreferrer"
            className="text-xs text-zinc-500 flex items-center hover:text-primary"
          >
            View on Explorer <ExternalLink size={12} className="ml-1" />
          </a>
        </CardFooter>
      </Card>
    </motion.div>
  )
} 