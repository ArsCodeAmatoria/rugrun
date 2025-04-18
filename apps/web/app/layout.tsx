import './globals.css'
import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import StarfieldBackground from '@/components/StarfieldBackground'

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
        <StarfieldBackground />
        {children}
      </body>
    </html>
  )
} 