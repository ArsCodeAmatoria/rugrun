"use client"

import { useEffect } from "react"
import Link from "next/link"
import { motion } from "framer-motion"
import confetti from "canvas-confetti"
import { Trophy, Share2, ArrowLeft } from "lucide-react"
import { Button } from "@/components/ui/button"
import { GlowCard, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import Navbar from "@/components/Navbar"
import { truncateAddress } from "@/lib/utils"

// Sample winner address - in a real app this would come from contract state
const WINNER_ADDRESS = "addr1qx8z0j76x5ylc6xh3dk4r5m82f8xtslzfs9slnefyg0fjdnvjdz48zg42xk4hmayuuhqynasjvq8h3qcxwxleh6e95rsagfjwx"

export default function RugSlayerPage() {
  // Trigger confetti on page load
  useEffect(() => {
    const duration = 5 * 1000
    const animationEnd = Date.now() + duration
    const defaults = { startVelocity: 30, spread: 360, ticks: 60, zIndex: 0 }

    function randomInRange(min: number, max: number) {
      return Math.random() * (max - min) + min
    }

    const interval = setInterval(() => {
      const timeLeft = animationEnd - Date.now()

      if (timeLeft <= 0) {
        return clearInterval(interval)
      }

      const particleCount = 50 * (timeLeft / duration)
      
      // Since particles fall down, start a bit higher than random
      confetti({
        ...defaults,
        particleCount,
        origin: { x: randomInRange(0.1, 0.3), y: Math.random() - 0.2 }
      })
      confetti({
        ...defaults,
        particleCount,
        origin: { x: randomInRange(0.7, 0.9), y: Math.random() - 0.2 }
      })
    }, 250)

    return () => clearInterval(interval)
  }, [])

  const shareWin = () => {
    // In a real app, this would share via social media
    const text = `I just slayed the rug and won all the $RGRN tokens! #RugRun #DeFi #Cardano`
    const url = window.location.href
    
    // For demo purposes, just copy to clipboard
    navigator.clipboard.writeText(`${text} ${url}`)
      .then(() => alert("Copied to clipboard! Share your victory!"))
      .catch(err => console.error("Could not copy text: ", err))
  }

  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-grow pt-20">
        <div className="max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="text-center mb-12">
            <Link href="/" className="inline-flex items-center text-sm text-zinc-400 hover:text-white mb-6">
              <ArrowLeft className="mr-2 h-4 w-4" /> Back to home
            </Link>
            
            <motion.div
              initial={{ scale: 0.8, opacity: 0 }}
              animate={{ scale: 1, opacity: 1 }}
              transition={{ type: "spring", duration: 0.8 }}
            >
              <h1 className="text-4xl md:text-6xl font-extrabold font-orbitron tracking-tight mb-8">
                <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary via-pink-500 to-white">
                  Rug Slayer!
                </span>
              </h1>
            </motion.div>
            
            <motion.div
              initial={{ y: 20, opacity: 0 }}
              animate={{ y: 0, opacity: 1 }}
              transition={{ delay: 0.3 }}
            >
              <div className="flex justify-center mb-8">
                <div className="w-32 h-32 rounded-full bg-black border-2 border-primary shadow-[0_0_30px_#ff007f] flex items-center justify-center">
                  <Trophy className="h-16 w-16 text-primary" />
                </div>
              </div>
              
              <p className="text-xl text-zinc-300 max-w-xl mx-auto mb-12">
                The real wallet was found and all $RGRN tokens have been claimed!
              </p>
            </motion.div>
          </div>
          
          <motion.div
            initial={{ y: 30, opacity: 0 }}
            animate={{ y: 0, opacity: 1 }}
            transition={{ delay: 0.5 }}
          >
            <GlowCard className="mb-8">
              <CardHeader>
                <CardTitle className="text-center text-white font-orbitron">
                  Winner
                </CardTitle>
              </CardHeader>
              <CardContent className="text-center">
                <div className="font-mono text-xl mb-4 text-primary">
                  {truncateAddress(WINNER_ADDRESS, 12, 12)}
                </div>
                <div className="bg-black/30 rounded-lg p-4 mb-6">
                  <p className="text-zinc-300">
                    This wallet discovered the correct secret phrase and unlocked all the $RGRN tokens. 
                    The rug has officially been pulled, but this time the community won!
                  </p>
                </div>
                <Button variant="glow" size="lg" onClick={shareWin} className="gap-2">
                  <Share2 className="h-4 w-4" />
                  Share Victory
                </Button>
              </CardContent>
            </GlowCard>
            
            <div className="text-center">
              <h3 className="text-xl font-orbitron mb-4">NFT Badge Awarded</h3>
              <div className="border border-zinc-800 rounded-xl p-6 bg-black/30 inline-block shadow-[0_0_15px_#ff007f]">
                <div className="aspect-square w-40 md:w-60 bg-gradient-to-br from-primary/30 via-black to-purple-900/30 rounded-lg border border-primary/50 flex flex-col items-center justify-center p-4">
                  <div className="text-5xl mb-4">🏃‍♂️</div>
                  <div className="text-lg font-orbitron text-center">Rug Slayer</div>
                  <div className="text-xs text-zinc-400 mt-2">Special Edition NFT</div>
                </div>
              </div>
            </div>
          </motion.div>
        </div>
      </main>
    </div>
  )
} 