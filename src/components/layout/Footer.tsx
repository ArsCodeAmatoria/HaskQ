import Link from 'next/link';

export default function Footer() {
  return (
    <footer className="bg-white dark:bg-gray-900 border-t border-gray-200 dark:border-gray-800">
      <div className="container mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="md:flex md:justify-between">
          <div className="mb-8 md:mb-0">
            <Link href="/" className="flex items-center">
              <span className="text-xl font-bold text-indigo-600 dark:text-indigo-400">HaskQ</span>
            </Link>
            <p className="mt-2 text-sm text-gray-500 dark:text-gray-400">
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
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    Getting Started
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/docs/core-concepts"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    Core Concepts
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/docs/tutorials"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    Tutorials
                  </Link>
                </li>
              </ul>
            </div>
            
            <div>
              <h3 className="text-sm font-semibold tracking-wider text-gray-400 uppercase">
                Resources
              </h3>
              <ul className="mt-4 space-y-2">
                <li>
                  <a 
                    href="https://github.com/ArsCodeAmatoria/HaskQ"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    GitHub
                  </a>
                </li>
              </ul>
            </div>
            
            <div>
              <h3 className="text-sm font-semibold tracking-wider text-gray-400 uppercase">
                Quantum Ecosystem
              </h3>
              <ul className="mt-4 space-y-2">
                <li>
                  <a 
                    href="https://quantum-computing.ibm.com/"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    IBM Quantum
                  </a>
                </li>
                <li>
                  <a 
                    href="https://qiskit.org/"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    Qiskit
                  </a>
                </li>
                <li>
                  <a 
                    href="https://www.mathstat.dal.ca/~selinger/quipper/"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-sm text-gray-500 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
                  >
                    Quipper
                  </a>
                </li>
              </ul>
            </div>
          </div>
        </div>
        
        <div className="pt-8 mt-8 border-t border-gray-200 dark:border-gray-700">
          <p className="text-sm text-gray-500 dark:text-gray-400 text-center">
            &copy; {new Date().getFullYear()} HaskQ. All rights reserved.
          </p>
        </div>
      </div>
    </footer>
  );
} 