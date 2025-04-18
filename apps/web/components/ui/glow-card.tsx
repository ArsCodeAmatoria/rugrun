'use client'

import { ReactNode, useState } from 'react'
import { motion } from 'framer-motion'
import { cn } from '@/lib/utils'

interface GlowCardProps {
  children: ReactNode
  className?: string
  glowColor?: string
}

export default function GlowCard({
  children,
  className,
  glowColor = 'rgba(147, 51, 234, 0.4)'
}: GlowCardProps) {
  const [mousePosition, setMousePosition] = useState<{ x: number; y: number }>({ x: 0, y: 0 })
  const [isHovered, setIsHovered] = useState(false)
  
  const handleMouseMove = (e: React.MouseEvent<HTMLDivElement>) => {
    const rect = e.currentTarget.getBoundingClientRect()
    setMousePosition({
      x: e.clientX - rect.left,
      y: e.clientY - rect.top
    })
  }
  
  return (
    <motion.div
      className={cn(
        'relative overflow-hidden rounded-xl p-6 border border-zinc-800 bg-zinc-900/50 backdrop-blur-sm',
        className
      )}
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      whileHover={{ scale: 1.02 }}
      transition={{
        duration: 0.2,
        scale: {
          type: 'spring',
          stiffness: 300,
          damping: 15
        }
      }}
      onMouseMove={handleMouseMove}
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
    >
      {isHovered && (
        <motion.div
          className="absolute pointer-events-none"
          animate={{
            x: mousePosition.x - 75,
            y: mousePosition.y - 75,
            opacity: 0.6,
            scale: 1.2
          }}
          transition={{
            type: 'spring',
            stiffness: 350,
            damping: 25
          }}
          style={{
            width: '150px',
            height: '150px',
            borderRadius: '50%',
            background: `radial-gradient(circle, ${glowColor} 0%, rgba(0,0,0,0) 70%)`,
            filter: 'blur(10px)'
          }}
        />
      )}
      {children}
    </motion.div>
  )
} 