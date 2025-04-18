"use client"

import { useState, useEffect } from "react"
import Link from "next/link"
import { motion } from "framer-motion"
import { Shield, Code, Database, Zap, ArrowRight, Twitter, Github, MessageSquare } from "lucide-react"
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
        <section className="pt-32 pb-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
            className="text-center space-y-8"
          >
            <h1 className="text-4xl sm:text-5xl md:text-6xl font-extrabold font-orbitron tracking-tight">
              <span className="block flex items-center justify-center gap-2">
                <Zap className="h-12 w-12 text-primary" />
                RugRun
              </span>
              <span className="block mt-2 text-transparent bg-clip-text bg-gradient-to-r from-primary via-pink-500 to-white">
                Catch the Dev. Win the Pool.
              </span>
            </h1>
            
            <p className="max-w-2xl mx-auto text-xl text-zinc-400">
              A decentralized on-chain Capture The Flag game where only one wallet holds the real $RGRN tokens.
            </p>
            
            <div className="pt-6">
              <Link href="/hunt">
                <Button 
                  variant="glow" 
                  size="xl" 
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
        <section className="py-16 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto grid gap-12 md:grid-cols-2 lg:grid-cols-4">
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.1 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-6 hover:border-primary/50 transition-colors"
          >
            <Shield className="h-8 w-8 text-primary mb-4" />
            <h3 className="text-xl font-orbitron mb-2">One Wallet Holds the Bag</h3>
            <p className="text-zinc-400">
              100% of $RGRN tokens are in a single wallet, locked by a secret phrase.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-6 hover:border-primary/50 transition-colors"
          >
            <Database className="h-8 w-8 text-primary mb-4" />
            <h3 className="text-xl font-orbitron mb-2">Many Wallets. Only One is Real.</h3>
            <p className="text-zinc-400">
              Navigate through decoy wallets with misleading clues to find the one true wallet.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.3 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-6 hover:border-primary/50 transition-colors"
          >
            <Code className="h-8 w-8 text-primary mb-4" />
            <h3 className="text-xl font-orbitron mb-2">Powered by Haskell</h3>
            <p className="text-zinc-400">
              Built with Plutus, the functional and battle-tested smart contract language.
            </p>
          </motion.div>
          
          <motion.div 
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.4 }}
            className="bg-zinc-900/50 border border-zinc-800 rounded-xl p-6 hover:border-primary/50 transition-colors"
          >
            <Zap className="h-8 w-8 text-primary mb-4" />
            <h3 className="text-xl font-orbitron mb-2">Decentralized CTF</h3>
            <p className="text-zinc-400">
              The entire game runs on-chain. No central authority. Pure blockchain.
            </p>
          </motion.div>
        </section>
        
        {/* Haskell Benefits */}
        <section className="py-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-orbitron font-bold mb-6">Why Haskell?</h2>
            <div className="h-20 flex items-center justify-center">
              <motion.p
                key={currentBenefit}
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                exit={{ opacity: 0, y: -20 }}
                transition={{ duration: 0.5 }}
                className="text-xl text-zinc-300 max-w-2xl mx-auto"
              >
                {haskellBenefits[currentBenefit]}
              </motion.p>
            </div>
          </div>
          
          <div className="grid md:grid-cols-2 gap-8">
            {whyHaskellForContracts.map((benefit, index) => (
              <motion.div
                key={index}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.3, delay: index * 0.1 }}
                viewport={{ once: true }}
                className="bg-black/30 border border-zinc-800 rounded-xl p-5 flex items-start"
              >
                <div className="h-6 w-6 rounded-full bg-primary/20 flex items-center justify-center text-primary mr-4 mt-0.5">
                  {index + 1}
                </div>
                <p className="text-zinc-300">{benefit}</p>
              </motion.div>
            ))}
          </div>
        </section>
        
        {/* CTA Section */}
        <section className="py-20 px-4 sm:px-6 lg:px-8 max-w-7xl mx-auto">
          <motion.div 
            initial={{ opacity: 0 }}
            whileInView={{ opacity: 1 }}
            viewport={{ once: true }}
            transition={{ duration: 0.6 }}
            className="bg-gradient-to-br from-black via-black to-primary/20 border border-zinc-800 rounded-2xl p-8 md:p-12 text-center"
          >
            <h2 className="text-3xl md:text-4xl font-bold font-orbitron mb-6">
              Ready to Find the Real Wallet?
            </h2>
            <p className="text-xl text-zinc-300 mb-8 max-w-2xl mx-auto">
              Analyze the clues, crack the code, and claim the entire $RGRN pool.
            </p>
            <Link href="/hunt">
              <Button 
                variant="glow" 
                size="xl" 
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