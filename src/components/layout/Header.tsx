'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { useState, useEffect } from 'react';
import { Github } from 'lucide-react';

const navigation = [
  { name: 'Home', href: '/' },
  { name: 'Playground', href: '/playground' },
  { name: 'Docs', href: '/docs' },
  { 
    name: 'Theory Projects', 
    href: '#',
    submenu: [
      { name: 'Phantasius', href: 'https://phantasius.vercel.app/', description: 'AGDEF Theory & Consciousness' },
      { name: 'Romulus', href: 'https://romulus-rouge.vercel.app/', description: 'Modified Gravity & Dark Matter' },
      { name: 'Arcana Obscura', href: 'https://arcana-obscura.vercel.app/', description: 'Esoteric Wisdom & Hermetic Principles' }
    ]
  }
];

export default function Header() {
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [scrolled, setScrolled] = useState(false);
  const pathname = usePathname();

  useEffect(() => {
    const handleScroll = () => {
      setScrolled(window.scrollY > 10);
    };
    
    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);

  const isActive = (path: string) => {
    if (path === '/' && pathname === '/') return true;
    if (path !== '/' && pathname.startsWith(path)) return true;
    return false;
  };

  return (
    <header className={`fixed top-0 left-0 right-0 z-50 transition-all duration-200 ${
      scrolled ? 'bg-gray-900/80 backdrop-blur-md shadow-sm' : 'bg-transparent'
    }`}>
      <div className="container mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between h-16">
          <div className="flex items-center">
            <Link href="/" className="flex-shrink-0 flex items-center">
              <span className={`text-2xl font-bold ${scrolled ? 'text-white' : 'text-white'}`}>HaskQ</span>
            </Link>

            <nav className="hidden md:ml-8 md:flex md:space-x-8">
              <Link 
                href="/" 
                className={`inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium ${
                  isActive('/') 
                    ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                    : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
                }`}
              >
                Home
              </Link>
              <Link 
                href="/docs" 
                className={`inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium ${
                  isActive('/docs') 
                    ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                    : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
                }`}
              >
                Documentation
              </Link>
              <Link 
                href="/playground" 
                className={`inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium ${
                  isActive('/playground') 
                    ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                    : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
                }`}
              >
                Playground
              </Link>
              <Link 
                href="/downloads" 
                className={`inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium ${
                  isActive('/downloads') 
                    ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                    : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
                }`}
              >
                Downloads
              </Link>
            </nav>
          </div>

          <div className="flex items-center">
            <a 
              href="https://github.com/ArsCodeAmatoria/HaskQ" 
              target="_blank" 
              rel="noopener noreferrer"
              className={`transition-colors ${scrolled ? 'text-gray-300 hover:text-indigo-400' : 'text-indigo-100 hover:text-white'}`}
            >
              <span className="sr-only">GitHub</span>
              <Github className="h-6 w-6" />
            </a>
          </div>

          <div className="-mr-2 flex items-center md:hidden">
            <button
              type="button"
              className={`inline-flex items-center justify-center p-2 rounded-md focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500 ${
                scrolled ? 'text-gray-300 hover:text-indigo-400' : 'text-indigo-100 hover:text-white'
              }`}
              aria-expanded="false"
              onClick={() => setIsMenuOpen(!isMenuOpen)}
            >
              <span className="sr-only">Open main menu</span>
              <svg
                className={`${isMenuOpen ? 'hidden' : 'block'} h-6 w-6`}
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                aria-hidden="true"
              >
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M4 6h16M4 12h16M4 18h16" />
              </svg>
              <svg
                className={`${isMenuOpen ? 'block' : 'hidden'} h-6 w-6`}
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                aria-hidden="true"
              >
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
              </svg>
            </button>
          </div>
        </div>
      </div>

      <div className={`${isMenuOpen ? 'block' : 'hidden'} md:hidden ${
        scrolled ? 'bg-gray-900/90' : 'bg-indigo-900/90'
      } backdrop-blur-md`}>
        <div className="pt-2 pb-3 space-y-1">
          <Link
            href="/"
            className={`block pl-3 pr-4 py-2 border-l-4 text-base font-medium ${
              isActive('/')
                ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
            }`}
          >
            Home
          </Link>
          <Link
            href="/docs"
            className={`block pl-3 pr-4 py-2 border-l-4 text-base font-medium ${
              isActive('/docs')
                ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
            }`}
          >
            Documentation
          </Link>
          <Link
            href="/playground"
            className={`block pl-3 pr-4 py-2 border-l-4 text-base font-medium ${
              isActive('/playground')
                ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
            }`}
          >
            Playground
          </Link>
          <Link
            href="/downloads"
            className={`block pl-3 pr-4 py-2 border-l-4 text-base font-medium ${
              isActive('/downloads')
                ? `border-indigo-500 ${scrolled ? 'text-indigo-400' : 'text-white'}`
                : `border-transparent ${scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'}`
            }`}
          >
            Downloads
          </Link>
          <a
            href="https://github.com/ArsCodeAmatoria/HaskQ"
            target="_blank"
            rel="noopener noreferrer"
            className={`block pl-3 pr-4 py-2 border-l-4 border-transparent text-base font-medium ${
              scrolled ? 'text-gray-300 hover:text-indigo-400 hover:border-indigo-400' : 'text-indigo-100 hover:text-white hover:border-indigo-200'
            }`}
          >
            GitHub
          </a>
        </div>
      </div>
    </header>
  );
} 