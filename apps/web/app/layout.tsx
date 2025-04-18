import './globals.css'
import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import StarfieldBackground from '@/components/StarfieldBackground'
import ParticleBackground from '@/components/ParticleBackground'
import { Suspense } from 'react'
import PageTransition from '@/components/PageTransition'
import { ApiProvider } from '@/providers/ApiProvider'
import { GameProvider } from '@/providers/GameProvider'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'RugRun - Catch the Dev. Win the Pool.',
  description: 'A decentralized CTF game on the blockchain - find the real wallet and win the tokens',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={inter.className}>
        {/* Primary background */}
        <StarfieldBackground />
        
        {/* Overlay with particle effects */}
        <Suspense fallback={null}>
          <ParticleBackground />
        </Suspense>
        
        {/* Main content with page transitions */}
        <ApiProvider>
          <GameProvider>
            <PageTransition>
              {children}
            </PageTransition>
          </GameProvider>
        </ApiProvider>
      </body>
    </html>
  )
} 