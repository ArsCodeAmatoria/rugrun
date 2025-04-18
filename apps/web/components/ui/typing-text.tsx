'use client'

import { useState, useEffect, ReactNode, useMemo } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { cn } from '@/lib/utils'

interface TypingTextProps {
  text: string
  typingSpeed?: number
  className?: string
  cursorColor?: string
  onComplete?: () => void
  delay?: number
}

export default function TypingText({
  text,
  typingSpeed = 40,
  className,
  cursorColor = '#9333EA',
  onComplete,
  delay = 0
}: TypingTextProps) {
  const [displayedText, setDisplayedText] = useState('')
  const [isComplete, setIsComplete] = useState(false)
  const [started, setStarted] = useState(false)

  useEffect(() => {
    let timeout: NodeJS.Timeout
    
    // Initial delay before starting
    if (!started) {
      timeout = setTimeout(() => {
        setStarted(true)
      }, delay)
      return () => clearTimeout(timeout)
    }
    
    if (started && displayedText.length < text.length) {
      timeout = setTimeout(() => {
        setDisplayedText(text.slice(0, displayedText.length + 1))
      }, typingSpeed)
    } else if (started && displayedText.length === text.length && !isComplete) {
      setIsComplete(true)
      if (onComplete) {
        onComplete()
      }
    }
    
    return () => clearTimeout(timeout)
  }, [displayedText, text, typingSpeed, started, isComplete, onComplete, delay])

  return (
    <div className={cn('relative font-mono', className)}>
      <span>{displayedText}</span>
      <AnimatePresence>
        {(!isComplete || !started) && (
          <motion.span
            initial={{ opacity: 1 }}
            animate={{ opacity: [1, 0, 1] }}
            exit={{ opacity: 0 }}
            transition={{ duration: 0.8, repeat: Infinity }}
            style={{ backgroundColor: cursorColor }}
            className="inline-block w-2 h-4 ml-0.5 align-middle"
          />
        )}
      </AnimatePresence>
    </div>
  )
}

interface TypewriterProps {
  children: ReactNode
  typingSpeed?: number
  className?: string
  cursorColor?: string
}

export function Typewriter({
  children,
  typingSpeed = 40,
  className,
  cursorColor = '#9333EA'
}: TypewriterProps) {
  const [displayedContent, setDisplayedContent] = useState<ReactNode[]>([])
  const childrenArray = useMemo(() => 
    Array.isArray(children) ? children : [children]
  , [children])
  
  useEffect(() => {
    if (displayedContent.length < childrenArray.length) {
      const timeout = setTimeout(() => {
        setDisplayedContent([...displayedContent, childrenArray[displayedContent.length]])
      }, typingSpeed * 20)
      
      return () => clearTimeout(timeout)
    }
  }, [displayedContent, childrenArray, typingSpeed])
  
  return (
    <div className={className}>
      {displayedContent}
      <AnimatePresence>
        {displayedContent.length < childrenArray.length && (
          <motion.span
            initial={{ opacity: 1 }}
            animate={{ opacity: [1, 0, 1] }}
            exit={{ opacity: 0 }}
            transition={{ duration: 0.8, repeat: Infinity }}
            style={{ backgroundColor: cursorColor }}
            className="inline-block w-2 h-4 ml-0.5 align-middle"
          />
        )}
      </AnimatePresence>
    </div>
  )
} 