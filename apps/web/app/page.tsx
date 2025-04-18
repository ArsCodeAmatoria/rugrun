"use client"

import { useState, useEffect } from "react"
import Link from "next/link"
import { motion } from "framer-motion"
import { Shield, Code, Database, Zap, ArrowRight } from "lucide-react"
import { Button } from "@/components/ui/button"
import Navbar from "@/components/Navbar"
import Footer from "@/components/Footer"
import { haskellBenefits, whyHaskellForContracts } from "@/utils/haskellBenefits"

export default function LandingPage() {
  const [currentBenefit, setCurrentBenefit] = useState(0)
  
  // Cycle through Haskell benefits
  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentBenefit((prev) => (prev + 1) % haskellBenefits.length)
    }, 5000)
    return () => clearInterval(interval)
  }, [])

  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-grow">
        {/* Hero Section */}
        <section className="pt-24 sm:pt-32 pb-12 sm:pb-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
            className="text-center space-y-6 sm:space-y-8"
          >
            <h1 className="text-3xl sm:text-5xl md:text-6xl font-extrabold font-orbitron tracking-tight">
              <span className="block flex items-center justify-center gap-2">
                <Zap className="h-8 w-8 sm:h-12 sm:w-12 text-primary" />
                RugRun
              </span>
              <span className="block mt-2 text-transparent bg-clip-text bg-gradient-to-r from-primary via-pink-500 to-white text-2xl sm:text-4xl md:text-5xl">
                Catch the Dev. Win the Pool.
              </span>
            </h1>
            
            <p className="max-w-2xl mx-auto text-lg sm:text-xl text-zinc-400 px-2">
              A decentralized on-chain Capture The Flag game where only one wallet holds the real $RGRN tokens.
            </p>
            
            <div className="pt-4 sm:pt-6">
              <Link href="/hunt">
                <Button 
                  variant="glow" 
                  size="lg" 
                  className="font-orbitron group"
                >
                  Begin the Hunt
                  <ArrowRight className="ml-2 group-hover:translate-x-1 transition-transform" />
                </Button>
              </Link>
            </div>
          </motion.div>
        </section>
        
        {/* Feature Sections */}
        <section className="py-12 sm:py-16 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto grid gap-6 sm:gap-8 grid-cols-1 sm:grid-cols-2 lg:grid-cols-4">
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.1 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-5 sm:p-6 hover:border-primary/50 transition-colors"
          >
            <Shield className="h-7 w-7 sm:h-8 sm:w-8 text-primary mb-3 sm:mb-4" />
            <h3 className="text-lg sm:text-xl font-orbitron mb-2">One Wallet Holds the Bag</h3>
            <p className="text-zinc-400 text-sm sm:text-base">
              100% of $RGRN tokens are in a single wallet, locked by a secret phrase.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-5 sm:p-6 hover:border-primary/50 transition-colors"
          >
            <Database className="h-7 w-7 sm:h-8 sm:w-8 text-primary mb-3 sm:mb-4" />
            <h3 className="text-lg sm:text-xl font-orbitron mb-2">Many Wallets. One is Real.</h3>
            <p className="text-zinc-400 text-sm sm:text-base">
              Navigate through decoy wallets with misleading clues to find the one true wallet.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.3 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-5 sm:p-6 hover:border-primary/50 transition-colors"
          >
            <Code className="h-7 w-7 sm:h-8 sm:w-8 text-primary mb-3 sm:mb-4" />
            <h3 className="text-lg sm:text-xl font-orbitron mb-2">Powered by Haskell</h3>
            <p className="text-zinc-400 text-sm sm:text-base">
              Built with Plutus, the functional and battle-tested smart contract language.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.4 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-5 sm:p-6 hover:border-primary/50 transition-colors"
          >
            <Zap className="h-7 w-7 sm:h-8 sm:w-8 text-primary mb-3 sm:mb-4" />
            <h3 className="text-lg sm:text-xl font-orbitron mb-2">Decentralized CTF</h3>
            <p className="text-zinc-400 text-sm sm:text-base">
              The entire game runs on-chain. No central authority. Pure blockchain.
            </p>
          </motion.div>
        </section>
        
        {/* Haskell Benefits */}
        <section className="py-12 sm:py-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <div className="text-center mb-10 sm:mb-16">
            <h2 className="text-2xl sm:text-3xl font-orbitron font-bold mb-4 sm:mb-6">Why Haskell?</h2>
            <div className="h-24 sm:h-20 flex items-center justify-center px-2">
              <motion.p
                key={currentBenefit}
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                exit={{ opacity: 0, y: -20 }}
                transition={{ duration: 0.5 }}
                className="text-lg sm:text-xl text-zinc-300 max-w-2xl mx-auto"
              >
                {haskellBenefits[currentBenefit]}
              </motion.p>
            </div>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 sm:gap-8">
            {whyHaskellForContracts.map((benefit, index) => (
              <motion.div
                key={index}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.3, delay: index * 0.1 }}
                viewport={{ once: true }}
                className="bg-black/30 border border-zinc-800 rounded-xl p-4 sm:p-5 flex items-start"
              >
                <div className="h-5 w-5 sm:h-6 sm:w-6 rounded-full bg-primary/20 flex items-center justify-center text-primary mr-3 sm:mr-4 mt-0.5 text-xs sm:text-sm">
                  {index + 1}
                </div>
                <p className="text-zinc-300 text-sm sm:text-base">{benefit}</p>
              </motion.div>
            ))}
          </div>
        </section>
        
        {/* CTA Section */}
        <section className="py-12 sm:py-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <motion.div 
            initial={{ opacity: 0 }}
            whileInView={{ opacity: 1 }}
            viewport={{ once: true }}
            transition={{ duration: 0.6 }}
            className="bg-gradient-to-br from-black via-black to-primary/20 border border-zinc-800 rounded-2xl p-6 sm:p-8 md:p-12 text-center"
          >
            <h2 className="text-2xl sm:text-3xl md:text-4xl font-bold font-orbitron mb-4 sm:mb-6">
              Ready to Find the Real Wallet?
            </h2>
            <p className="text-lg sm:text-xl text-zinc-300 mb-6 sm:mb-8 max-w-2xl mx-auto px-2">
              Analyze the clues, crack the code, and claim the entire $RGRN pool.
            </p>
            <Link href="/hunt">
              <Button 
                variant="glow" 
                size="lg" 
                className="font-orbitron animate-glow"
              >
                Start Scanning
              </Button>
            </Link>
          </motion.div>
        </section>
      </main>
      
      <Footer />
    </div>
  )
} 