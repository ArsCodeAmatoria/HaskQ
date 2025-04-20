'use client';

import Link from "next/link";
import { motion } from "framer-motion";
import { ArrowRight, Code, Box, Zap, Shield, Github, Twitter, Book } from "lucide-react";

function FeatureCard({ icon, title, description }: { icon: React.ReactNode, title: string, description: string }) {
  return (
    <motion.div 
      className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg"
      whileHover={{ y: -5, boxShadow: '0 10px 25px -5px rgba(0, 0, 0, 0.1)' }}
      transition={{ duration: 0.2 }}
    >
      <div className="mb-4">{icon}</div>
      <h3 className="text-xl font-semibold mb-2">{title}</h3>
      <p className="text-gray-600 dark:text-gray-300">{description}</p>
    </motion.div>
  );
}

export default function Home() {
  return (
    <main className="overflow-hidden">
      {/* Hero Section */}
      <section className="relative min-h-[90vh] flex items-center justify-center overflow-hidden bg-gradient-to-br from-indigo-900 via-indigo-800 to-blue-900">
        <div className="absolute inset-0 z-0 opacity-20 dark:opacity-10 bg-[radial-gradient(#ffffff33_1px,transparent_1px)] [background-size:24px_24px]">
        </div>
        <div className="container mx-auto relative z-10 flex flex-col items-center justify-center py-20 text-center px-4">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8 }}
          >
            <h1 className="text-4xl md:text-5xl lg:text-6xl font-bold mb-6 text-white">
              <span className="block">Quantum Circuits,</span>
              <span className="block text-indigo-300">Purely Functional.</span>
            </h1>
            <p className="mx-auto mb-10 max-w-2xl text-lg text-indigo-100">
              HaskQ brings together the elegance of Haskell and the power of quantum computing,
              providing a type-safe, purely functional approach to quantum circuit design and simulation.
            </p>
          </motion.div>
          
          <motion.div 
            className="flex flex-wrap justify-center gap-4"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ delay: 0.3, duration: 0.5 }}
          >
            <Link 
              href="/docs/getting-started" 
              className="bg-indigo-500 hover:bg-indigo-600 text-white rounded-full px-6 py-3 flex items-center transition-colors"
            >
              Get Started <ArrowRight className="ml-2 h-4 w-4" />
            </Link>
            <Link 
              href="/docs" 
              className="bg-white hover:bg-indigo-50 text-indigo-600 rounded-full px-6 py-3 flex items-center transition-colors"
            >
              View Docs <Book className="ml-2 h-4 w-4" />
            </Link>
            <Link 
              href="/playground" 
              className="bg-transparent hover:bg-indigo-700/30 text-white border border-indigo-300 rounded-full px-6 py-3 flex items-center transition-colors"
            >
              Try Playground <Code className="ml-2 h-4 w-4" />
            </Link>
          </motion.div>
          
          <motion.div
            className="mt-16"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ delay: 0.6, duration: 0.8 }}
          >
            <div className="relative h-40 w-40 sm:h-60 sm:w-60 mx-auto">
              {/* Quantum circuit animation/illustration */}
              <div className="absolute inset-0 rounded-full bg-indigo-500/20 backdrop-blur-sm"></div>
              <div className="absolute inset-4 rounded-full bg-indigo-500/30 animate-pulse"></div>
              <div className="absolute inset-10 rounded-full bg-indigo-500/40">
                <div className="absolute inset-0 rounded-full animate-spin duration-10000"></div>
              </div>
              <div className="absolute inset-16 rounded-full border-2 border-dashed border-indigo-500/60"></div>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-20 bg-white dark:bg-gray-900" id="features">
        <div className="container mx-auto px-4">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold mb-4">Features</h2>
            <p className="mx-auto max-w-2xl text-lg text-gray-600 dark:text-gray-300">
              Discover what makes HaskQ a powerful tool for quantum programming
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            <FeatureCard 
              icon={<Shield className="h-8 w-8 text-indigo-500" />}
              title="Type-Safe Circuits"
              description="Linear types ensure no-cloning principle at compile time, making invalid quantum programs impossible to construct."
            />
            <FeatureCard 
              icon={<Box className="h-8 w-8 text-indigo-500" />}
              title="Functional Composition"
              description="Monad-based circuit composition that enables clean, declarative quantum algorithms."
            />
            <FeatureCard 
              icon={<Zap className="h-8 w-8 text-indigo-500" />}
              title="Built-in Simulation"
              description="Simulate quantum circuits with up to 5 qubits using state vectors and unitary matrices."
            />
          </div>
        </div>
      </section>

      {/* Code Example Section */}
      <section className="py-20 bg-gray-50 dark:bg-gray-950" id="code-example">
        <div className="container mx-auto px-4">
          <div className="text-center mb-12">
            <h2 className="text-3xl font-bold mb-4">Elegant Code</h2>
            <p className="mx-auto max-w-2xl text-lg text-gray-600 dark:text-gray-300">
              Build quantum circuits with clean, type-safe Haskell code
            </p>
          </div>
          
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-center">
            <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg">
              <pre className="overflow-x-auto font-mono text-sm leading-relaxed">
                <code className="language-haskell">{`-- Bell state circuit
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \\[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')`}</code>
              </pre>
            </div>
            
            <div>
              <h3 className="text-2xl font-bold text-indigo-600 dark:text-indigo-400 mb-4">No Cloning, At The Type Level</h3>
              <p className="mb-4 text-gray-700 dark:text-gray-300">
                HaskQ leverages Haskell's linear types to enforce the quantum no-cloning theorem at compile time. 
                This means you simply cannot write code that violates quantum mechanics.
              </p>
              <p className="mb-4 text-gray-700 dark:text-gray-300">
                The code example shows how to create a Bell state—a maximally entangled pair of qubits—with 
                just a few lines of functional code.
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-20 bg-indigo-600 dark:bg-indigo-700 text-white" id="get-started">
        <div className="container mx-auto px-4 text-center">
          <h2 className="text-3xl font-bold mb-6">Ready to Dive Into Quantum?</h2>
          <p className="mx-auto mb-10 max-w-2xl text-lg opacity-90">
            Start building quantum circuits with HaskQ today. Check out our documentation, examples, and join our community.
          </p>
          <div className="flex flex-wrap justify-center gap-4">
            <Link 
              href="/docs/intro" 
              className="bg-white text-indigo-700 hover:bg-indigo-50 rounded-full px-6 py-3 transition-colors"
            >
              Get Started Guide
            </Link>
            <a 
              href="https://github.com/ArsCodeAmatoria/HaskQ" 
              className="bg-indigo-700 hover:bg-indigo-800 text-white rounded-full px-6 py-3 flex items-center justify-center transition-colors"
              target="_blank"
              rel="noopener noreferrer"
            >
              <Github className="mr-2 h-4 w-4" />
              GitHub Repository
            </a>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-12 bg-white dark:bg-gray-900 border-t border-gray-200 dark:border-gray-800">
        <div className="container mx-auto px-4">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div className="md:col-span-2">
              <h3 className="text-xl font-bold mb-4">HaskQ</h3>
              <p className="mb-4 text-gray-600 dark:text-gray-400">
                A functional quantum programming toolkit that brings together the elegance of Haskell 
                and the power of quantum computing.
              </p>
              <div className="flex space-x-4">
                <a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-gray-500 hover:text-indigo-500 transition-colors">
                  <Github className="h-5 w-5" />
                </a>
                <a href="https://twitter.com/ArsCodeAmatoria" className="text-gray-500 hover:text-indigo-500 transition-colors">
                  <Twitter className="h-5 w-5" />
                </a>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-4">Resources</h4>
              <ul className="space-y-2">
                <li><Link href="/docs" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Documentation</Link></li>
                <li><Link href="/docs/tutorials/bell-states" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Tutorials</Link></li>
                <li><Link href="/docs/core-concepts/quantum-computing-basics" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Core Concepts</Link></li>
                <li><Link href="/playground" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Playground</Link></li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-4">Community</h4>
              <ul className="space-y-2">
                <li><a href="https://github.com/ArsCodeAmatoria/HaskQ/discussions" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Join Us</a></li>
                <li><a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">GitHub</a></li>
                <li><Link href="/blog" className="text-gray-600 dark:text-gray-400 hover:text-indigo-500 transition-colors">Blog</Link></li>
              </ul>
            </div>
          </div>
          
          <div className="mt-12 pt-8 border-t border-gray-200 dark:border-gray-800 text-center text-sm text-gray-500">
            <p>© {new Date().getFullYear()} HaskQ. All rights reserved.</p>
          </div>
        </div>
      </footer>
    </main>
  );
}
