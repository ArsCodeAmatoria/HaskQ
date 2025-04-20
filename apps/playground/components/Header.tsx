'use client'

import React, { useState, useEffect } from 'react'
import Link from 'next/link'
import { useTheme } from 'next-themes'
import { Play, Save, FileDown, Sun, Moon, Github } from 'lucide-react'

// Dynamic URL construction based on environment
const isDev = process.env.NODE_ENV === 'development'
const baseUrl = isDev ? 'http://localhost:' : 'https://haskq.vercel.app'
const docsPort = '3000'
const landingPort = '3000'

// Helper function to construct URLs
const getUrl = (path: string = '', port: string = docsPort) => {
  return isDev ? `${baseUrl}${port}${path}` : `https://haskq.vercel.app${path}`
}

interface HeaderProps {
  onRunSimulation: () => void
  onSaveCircuit: () => void
  onExportCircuit: () => void
  isSimulating: boolean
  className?: string
}

const Header: React.FC<HeaderProps> = ({ 
  onRunSimulation, 
  onSaveCircuit, 
  onExportCircuit, 
  isSimulating,
  className
}) => {
  const [mounted, setMounted] = useState(false)
  const { theme, setTheme } = useTheme()

  // Use useEffect to avoid hydration mismatch
  useEffect(() => {
    setMounted(true)
  }, [])

  const toggleTheme = () => {
    if (theme === 'dark') {
      setTheme('light')
    } else {
      setTheme('dark')
    }
  }

  return (
    <header className={`bg-black border-b-2 border-red-500 py-4 px-6 shadow-md ${className || ''}`}>
      <div className="max-w-7xl mx-auto flex items-center justify-between">
        <div className="flex items-center space-x-6">
          <a href={getUrl('', landingPort)} className="flex items-center">
            <span className="text-xl font-bold text-red-500">
              HaskQ
            </span>
            <span className="ml-2 text-xs bg-red-900 px-2 py-0.5 rounded-md text-white">
              Playground
            </span>
          </a>
          
          <nav className="hidden md:flex space-x-6">
            <a href={getUrl()} className="text-white hover:text-red-300 text-sm">
              Documentation
            </a>
            <a href={getUrl('/category/tutorials')} className="text-white hover:text-red-300 text-sm">
              Tutorials
            </a>
            <a href={getUrl('/category/core-concepts')} className="text-white hover:text-red-300 text-sm">
              Core Concepts
            </a>
          </nav>
        </div>
        
        <div className="flex items-center space-x-3">
          <button
            onClick={onRunSimulation}
            disabled={isSimulating}
            className="bg-red-600 hover:bg-red-700 text-white px-3 py-1.5 rounded-md text-sm flex items-center disabled:opacity-50 disabled:cursor-not-allowed"
          >
            <Play size={16} className="mr-1" />
            {isSimulating ? 'Simulating...' : 'Run'}
          </button>
          
          <button
            onClick={onSaveCircuit}
            className="bg-gray-800 hover:bg-gray-700 text-white px-3 py-1.5 rounded-md text-sm flex items-center"
          >
            <Save size={16} className="mr-1" />
            Save
          </button>
          
          <button
            onClick={onExportCircuit}
            className="bg-gray-800 hover:bg-gray-700 text-white px-3 py-1.5 rounded-md text-sm flex items-center"
          >
            <FileDown size={16} className="mr-1" />
            Export
          </button>
          
          {mounted && (
            <button
              onClick={toggleTheme}
              className="bg-gray-800 hover:bg-gray-700 text-white p-1.5 rounded-md"
            >
              {theme === 'dark' ? <Sun size={18} /> : <Moon size={18} />}
            </button>
          )}
          
          <a 
            href="https://github.com/ArsCodeAmatoria/HaskQ" 
            target="_blank" 
            rel="noopener noreferrer"
            className="bg-gray-800 hover:bg-gray-700 text-white p-1.5 rounded-md"
          >
            <Github size={18} />
          </a>
        </div>
      </div>
    </header>
  )
}

export default Header 