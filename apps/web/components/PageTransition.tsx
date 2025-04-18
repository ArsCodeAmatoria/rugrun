'use client'

import { motion } from 'framer-motion'
import { usePathname } from 'next/navigation'

interface PageTransitionProps {
  children: React.ReactNode
}

const variants = {
  hidden: { 
    opacity: 0,
    y: 20
  },
  enter: { 
    opacity: 1,
    y: 0,
    transition: { 
      duration: 0.4, 
      ease: [0.61, 1, 0.88, 1],
      staggerChildren: 0.1
    }
  },
  exit: { 
    opacity: 0,
    y: -20,
    transition: { 
      duration: 0.3, 
      ease: [0.37, 0, 0.63, 1]
    }
  }
}

export default function PageTransition({ children }: PageTransitionProps) {
  const pathname = usePathname()
  
  return (
    <motion.div
      key={pathname}
      initial="hidden"
      animate="enter"
      exit="exit"
      variants={variants}
      className="flex-grow flex flex-col"
    >
      {children}
    </motion.div>
  )
} 