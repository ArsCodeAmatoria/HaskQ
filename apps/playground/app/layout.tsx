import type { Metadata } from 'next'
import './globals.css'
import { Inter } from 'next/font/google'

const inter = Inter({ 
  subsets: ['latin'],
  variable: '--font-inter',
  display: 'swap'
})

export const metadata: Metadata = {
  title: 'HaskQ Playground | Interactive Quantum Circuit Editor',
  description: 'Build and simulate quantum circuits with HaskQ - a purely functional quantum programming toolkit',
  keywords: 'quantum computing, HaskQ, quantum circuits, simulation, haskell'
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" className="dark">
      <body className={`${inter.variable} font-sans`}>
        {children}
      </body>
    </html>
  )
}
