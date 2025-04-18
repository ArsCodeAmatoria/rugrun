"use client"

import { useState } from "react"
import Link from "next/link"
import { usePathname } from "next/navigation"
import { motion, AnimatePresence } from "framer-motion"
import { Zap, Menu, X } from "lucide-react"
import { cn } from "@/lib/utils"

const navigation = [
  { name: "Home", href: "/" },
  { name: "Hunt", href: "/hunt" },
  { name: "Game", href: "/game" },
  { name: "Leaderboard", href: "/leaderboard" },
  { name: "Winner", href: "/rugslayer" },
]

export default function Navbar() {
  const pathname = usePathname()
  const [hoveredItem, setHoveredItem] = useState<string | null>(null)
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false)

  const toggleMobileMenu = () => {
    setMobileMenuOpen(!mobileMenuOpen)
  }

  return (
    <nav className="fixed top-0 left-0 right-0 z-50 bg-black/60 backdrop-blur-lg border-b border-zinc-800">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          <div className="flex-shrink-0">
            <Link href="/" className="font-orbitron text-xl text-white font-bold flex items-center gap-2">
              <Zap className="h-6 w-6 text-primary" />
              <span className="text-transparent bg-clip-text bg-gradient-to-r from-primary to-white">RugRun</span>
            </Link>
          </div>
          <div className="hidden md:block">
            <div className="ml-10 flex items-center space-x-4">
              {navigation.map((item) => {
                const isActive = pathname === item.href
                
                return (
                  <Link
                    key={item.name}
                    href={item.href}
                    className={cn(
                      "relative px-3 py-2 rounded-lg text-sm font-medium transition-colors",
                      isActive 
                        ? "text-white" 
                        : "text-gray-300 hover:text-white"
                    )}
                    onMouseEnter={() => setHoveredItem(item.name)}
                    onMouseLeave={() => setHoveredItem(null)}
                  >
                    {item.name}
                    {(isActive || hoveredItem === item.name) && (
                      <motion.span
                        layoutId="navbar-highlight"
                        className="absolute inset-0 rounded-lg bg-primary/10 ring-1 ring-primary/30"
                        initial={{ opacity: 0 }}
                        animate={{ opacity: 1 }}
                        exit={{ opacity: 0 }}
                        transition={{ duration: 0.2 }}
                      />
                    )}
                  </Link>
                )
              })}
            </div>
          </div>
          <div className="hidden md:block">
            <div className="ml-4 flex items-center md:ml-6">
              <div className="text-sm text-white px-3 py-1 rounded-full bg-primary/20 border border-primary/30">
                <span className="font-medium text-primary">$RGRN</span>
              </div>
            </div>
          </div>
          
          {/* Mobile menu button */}
          <div className="md:hidden flex items-center">
            <div className="mr-4">
              <div className="text-sm text-white px-3 py-1 rounded-full bg-primary/20 border border-primary/30">
                <span className="font-medium text-primary">$RGRN</span>
              </div>
            </div>
            <button
              onClick={toggleMobileMenu}
              className="text-gray-300 hover:text-white focus:outline-none p-2"
              aria-expanded={mobileMenuOpen}
            >
              <span className="sr-only">Open main menu</span>
              {mobileMenuOpen ? (
                <X className="h-6 w-6 text-primary" />
              ) : (
                <Menu className="h-6 w-6 text-primary" />
              )}
            </button>
          </div>
        </div>
      </div>
      
      {/* Mobile menu, show/hide based on menu state */}
      <AnimatePresence>
        {mobileMenuOpen && (
          <motion.div
            initial={{ opacity: 0, height: 0 }}
            animate={{ opacity: 1, height: "auto" }}
            exit={{ opacity: 0, height: 0 }}
            transition={{ duration: 0.2 }}
            className="md:hidden"
          >
            <div className="px-2 pt-2 pb-3 space-y-1 border-t border-zinc-800 bg-black/80 backdrop-blur-md">
              {navigation.map((item) => {
                const isActive = pathname === item.href
                
                return (
                  <Link
                    key={item.name}
                    href={item.href}
                    className={cn(
                      "block px-3 py-2 rounded-lg text-base font-medium transition-colors",
                      isActive 
                        ? "text-white bg-primary/10 border border-primary/30" 
                        : "text-gray-300 hover:text-white hover:bg-zinc-800"
                    )}
                    onClick={() => setMobileMenuOpen(false)}
                  >
                    {item.name}
                  </Link>
                )
              })}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </nav>
  )
} 