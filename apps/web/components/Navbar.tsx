"use client"

import { useState } from "react"
import Link from "next/link"
import { usePathname } from "next/navigation"
import { motion } from "framer-motion"
import { cn } from "@/lib/utils"

const navigation = [
  { name: "Home", href: "/" },
  { name: "Hunt", href: "/hunt" },
  { name: "Leaderboard", href: "/leaderboard" },
  { name: "Winner", href: "/rugslayer" },
]

export default function Navbar() {
  const pathname = usePathname()
  const [hoveredItem, setHoveredItem] = useState<string | null>(null)

  return (
    <nav className="fixed top-0 left-0 right-0 z-50 bg-black/60 backdrop-blur-lg border-b border-zinc-800">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          <div className="flex-shrink-0">
            <Link href="/" className="font-orbitron text-xl text-white font-bold flex items-center gap-2">
              <span role="img" aria-label="RugRun" className="text-2xl">🏃</span>
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
        </div>
      </div>
    </nav>
  )
} 