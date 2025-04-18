'use client'

import { motion } from 'framer-motion'
import { cn } from '@/lib/utils'

type SpinnerProps = {
  size?: 'sm' | 'md' | 'lg'
  className?: string
}

const variants = {
  sm: 'h-4 w-4 border-2',
  md: 'h-8 w-8 border-2',
  lg: 'h-12 w-12 border-3'
}

const spinTransition = {
  repeat: Infinity,
  ease: 'linear',
  duration: 1
}

export function Spinner({ size = 'md', className }: SpinnerProps) {
  return (
    <div className="flex items-center justify-center">
      <motion.div
        className={cn(
          'rounded-full border-b-transparent', 
          variants[size],
          'border-primary',
          className
        )}
        animate={{ rotate: 360 }}
        transition={spinTransition}
      />
    </div>
  )
}

export function FullPageSpinner() {
  return (
    <div className="fixed inset-0 flex items-center justify-center bg-black/70 z-50">
      <motion.div
        initial={{ opacity: 0, scale: 0.8 }}
        animate={{ opacity: 1, scale: 1 }}
        exit={{ opacity: 0, scale: 0.8 }}
        transition={{ duration: 0.2 }}
        className="bg-black/50 backdrop-blur-md p-6 rounded-xl border border-zinc-800 flex flex-col items-center"
      >
        <Spinner size="lg" />
        <p className="mt-4 text-zinc-300">Loading...</p>
      </motion.div>
    </div>
  )
} 