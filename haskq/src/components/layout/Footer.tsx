'use client';

import Link from 'next/link';

export default function Footer() {
  return (
    <footer className="bg-gray-900 border-t border-gray-800">
      <div className="container mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="md:flex md:justify-between">
          <div className="mb-8 md:mb-0">
            <Link href="/" className="flex items-center">
              <span className="text-xl font-bold text-indigo-400">HaskQ</span>
            </Link>
            <p className="mt-2 text-sm text-gray-400">
              A linear quantum programming language
            </p>
          </div>
          
          <div className="grid grid-cols-2 gap-8 sm:grid-cols-3">
            <div>
              <h3 className="text-sm font-semibold tracking-wider text-gray-400 uppercase">
                Documentation
              </h3>
              <ul className="mt-4 space-y-2">
                <li>
                  <Link 
                    href="/docs/getting-started"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                  >
                    Getting Started
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/docs/core-concepts"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                  >
                    Core Concepts
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/docs/tutorials"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                  >
                    Tutorials
                  </Link>
                </li>
              </ul>
            </div>
            
            <div>
              <h3 className="text-sm font-semibold tracking-wider text-gray-400 uppercase">
                Community
              </h3>
              <ul className="mt-4 space-y-2">
                <li>
                  <a 
                    href="https://github.com/ArsCodeAmatoria/HaskQ"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    GitHub
                  </a>
                </li>
                <li>
                  <a 
                    href="https://github.com/ArsCodeAmatoria/HaskQ/issues"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    Issues
                  </a>
                </li>
                <li>
                  <a 
                    href="https://github.com/ArsCodeAmatoria/HaskQ/discussions"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    Discussions
                  </a>
                </li>
              </ul>
            </div>
            
            <div>
              <h3 className="text-sm font-semibold tracking-wider text-gray-400 uppercase">
                Resources
              </h3>
              <ul className="mt-4 space-y-2">
                <li>
                  <Link 
                    href="/playground"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                  >
                    Playground
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/downloads"
                    className="text-sm text-gray-400 hover:text-indigo-400"
                  >
                    Downloads
                  </Link>
                </li>
              </ul>
            </div>
          </div>
        </div>
        
        <div className="mt-8 pt-8 border-t border-gray-800">
          <div className="md:flex md:items-center md:justify-between">
            <div className="flex space-x-6 md:order-2">
              <a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-gray-400 hover:text-indigo-400">
                <span className="sr-only">GitHub</span>
                <svg className="h-6 w-6" fill="currentColor" viewBox="0 0 24 24" aria-hidden="true">
                  <path fillRule="evenodd" d="M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z" clipRule="evenodd" />
                </svg>
              </a>
            </div>
            <p className="mt-8 text-base text-gray-400 md:mt-0 md:order-1">
              &copy; 2024 HaskQ. Built with quantum love.
            </p>
          </div>
        </div>
      </div>
    </footer>
  );
} 