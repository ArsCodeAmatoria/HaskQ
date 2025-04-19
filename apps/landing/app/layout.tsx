'use client'

import './globals.css'
import { Inter } from 'next/font/google'
import { useState, useEffect } from 'react'

const inter = Inter({ subsets: ['latin'] })

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  const [isDarkMode, setIsDarkMode] = useState(true)
  
  useEffect(() => {
    // Check system preference or stored value
    const savedMode = localStorage.getItem('darkMode')
    const systemPrefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches
    
    setIsDarkMode(savedMode ? savedMode === 'true' : systemPrefersDark)
  }, [])
  
  useEffect(() => {
    // Apply theme class
    if (isDarkMode) {
      document.documentElement.classList.add('dark')
    } else {
      document.documentElement.classList.remove('dark')
    }
    
    // Store preference
    localStorage.setItem('darkMode', isDarkMode.toString())
  }, [isDarkMode])

  return (
    <html lang="en" className={isDarkMode ? 'dark' : ''}>
      <head>
        <title>HaskQ - Quantum Circuits, Purely Functional</title>
        <meta name="description" content="A functional quantum programming toolkit built with Haskell" />
        <link rel="icon" href="/favicon.ico" />
      </head>
      <body className={`${inter.className} bg-white dark:bg-quantum-dark-950 text-quantum-dark-800 dark:text-quantum-dark-100 min-h-screen`}>
        {children}
      </body>
    </html>
  )
} 