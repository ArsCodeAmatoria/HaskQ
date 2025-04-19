'use client'

import Image from 'next/image'
import { motion } from 'framer-motion'
import { ArrowRight, Code, Box, Zap, Shield, Github, Twitter, Book } from 'lucide-react'

export default function LandingPage() {
  return (
    <main className="overflow-hidden">
      {/* Hero Section */}
      <section className="relative min-h-screen flex items-center justify-center overflow-hidden bg-quantum-gradient">
        <div className="absolute inset-0 z-0 opacity-20 dark:opacity-10">
          <div className="h-full w-full bg-quantum-grid bg-repeat"></div>
        </div>
        <div className="container-custom relative z-10 flex flex-col items-center justify-center py-20 text-center">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8 }}
          >
            <h1 className="mb-6 text-quantum-dark-900 dark:text-white">
              <span className="block">Quantum Circuits,</span>
              <span className="block text-quantum-blue-500">Purely Functional.</span>
            </h1>
            <p className="mx-auto mb-10 max-w-2xl text-lg text-quantum-dark-700 dark:text-quantum-dark-200">
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
            <a href="#get-started" className="btn-primary px-6 py-3">
              Get Started <ArrowRight className="ml-2 h-4 w-4" />
            </a>
            <a href="/docs" className="btn-secondary px-6 py-3">
              View Docs <Book className="ml-2 h-4 w-4" />
            </a>
            <a href="/playground" className="btn-outline px-6 py-3">
              Try Playground <Code className="ml-2 h-4 w-4" />
            </a>
          </motion.div>
          
          <motion.div
            className="mt-16 animate-superposition"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ delay: 0.6, duration: 0.8 }}
          >
            <div className="relative h-60 w-60 sm:h-80 sm:w-80">
              {/* Placeholder for quantum circuit animation/illustration */}
              <div className="absolute inset-0 rounded-full bg-quantum-blue-500/20 backdrop-blur"></div>
              <div className="absolute inset-4 rounded-full bg-quantum-blue-500/30 animate-pulse"></div>
              <div className="absolute inset-10 rounded-full bg-quantum-blue-500/40 animate-entanglement"></div>
              <div className="absolute inset-16 rounded-full border-2 border-dashed border-quantum-blue-500/60"></div>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-20 bg-white dark:bg-quantum-dark-900" id="features">
        <div className="container-custom">
          <div className="text-center mb-16">
            <h2 className="mb-4">Features</h2>
            <p className="mx-auto max-w-2xl text-lg text-quantum-dark-600 dark:text-quantum-dark-300">
              Discover what makes HaskQ a powerful tool for quantum programming
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            <FeatureCard 
              icon={<Shield className="h-8 w-8 text-quantum-blue-500" />}
              title="Type-Safe Circuits"
              description="Linear types ensure no-cloning principle at compile time, making invalid quantum programs impossible to construct."
            />
            <FeatureCard 
              icon={<Box className="h-8 w-8 text-quantum-blue-500" />}
              title="Functional Composition"
              description="Monad-based circuit composition that enables clean, declarative quantum algorithms."
            />
            <FeatureCard 
              icon={<Zap className="h-8 w-8 text-quantum-blue-500" />}
              title="Built-in Simulation"
              description="Simulate quantum circuits with up to 5 qubits using state vectors and unitary matrices."
            />
          </div>
        </div>
      </section>

      {/* Code Example Section */}
      <section className="py-20 bg-quantum-dark-50 dark:bg-quantum-dark-950" id="code-example">
        <div className="container-custom">
          <div className="text-center mb-12">
            <h2 className="mb-4">Elegant Code</h2>
            <p className="mx-auto max-w-2xl text-lg text-quantum-dark-600 dark:text-quantum-dark-300">
              Build quantum circuits with clean, type-safe Haskell code
            </p>
          </div>
          
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-center">
            <div className="bg-white dark:bg-quantum-dark-800 p-6 rounded-lg shadow-lg">
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
              <h3 className="mb-4 text-quantum-blue-600 dark:text-quantum-blue-400">No Cloning, At The Type Level</h3>
              <p className="mb-4 text-quantum-dark-700 dark:text-quantum-dark-300">
                HaskQ leverages Haskell's linear types to enforce the quantum no-cloning theorem at compile time. 
                This means you simply cannot write code that violates quantum mechanics.
              </p>
              <p className="mb-4 text-quantum-dark-700 dark:text-quantum-dark-300">
                The code example shows how to create a Bell state—a maximally entangled pair of qubits—with 
                just a few lines of functional code.
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-20 bg-quantum-blue-600 dark:bg-quantum-blue-800 text-white" id="get-started">
        <div className="container-custom text-center">
          <h2 className="mb-6">Ready to Dive Into Quantum?</h2>
          <p className="mx-auto mb-10 max-w-2xl text-lg opacity-90">
            Start building quantum circuits with HaskQ today. Check out our documentation, examples, and join our community.
          </p>
          <div className="flex flex-wrap justify-center gap-4">
            <a href="/docs/introduction" className="bg-white text-quantum-blue-700 hover:bg-quantum-blue-50 btn px-6 py-3">
              Get Started Guide
            </a>
            <a href="https://github.com/haskq/haskq" className="bg-quantum-blue-700 hover:bg-quantum-blue-800 btn px-6 py-3">
              <Github className="mr-2 h-4 w-4" />
              GitHub Repository
            </a>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-12 bg-white dark:bg-quantum-dark-900 border-t border-quantum-dark-200 dark:border-quantum-dark-800">
        <div className="container-custom">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div className="md:col-span-2">
              <h3 className="text-xl mb-4">HaskQ</h3>
              <p className="mb-4 text-quantum-dark-600 dark:text-quantum-dark-400">
                A functional quantum programming toolkit that brings together the elegance of Haskell 
                and the power of quantum computing.
              </p>
              <div className="flex space-x-4">
                <a href="https://github.com/haskq/haskq" className="text-quantum-dark-500 hover:text-quantum-blue-500">
                  <Github className="h-5 w-5" />
                </a>
                <a href="https://twitter.com/haskq" className="text-quantum-dark-500 hover:text-quantum-blue-500">
                  <Twitter className="h-5 w-5" />
                </a>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-4">Resources</h4>
              <ul className="space-y-2">
                <li><a href="/docs" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">Documentation</a></li>
                <li><a href="/tutorials" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">Tutorials</a></li>
                <li><a href="/api" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">API Reference</a></li>
                <li><a href="/playground" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">Playground</a></li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-4">Community</h4>
              <ul className="space-y-2">
                <li><a href="/community" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">Join Us</a></li>
                <li><a href="https://github.com/haskq/haskq" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">GitHub</a></li>
                <li><a href="/blog" className="text-quantum-dark-600 dark:text-quantum-dark-400 hover:text-quantum-blue-500">Blog</a></li>
              </ul>
            </div>
          </div>
          
          <div className="mt-12 pt-8 border-t border-quantum-dark-200 dark:border-quantum-dark-800 text-center text-sm text-quantum-dark-500">
            <p>© {new Date().getFullYear()} HaskQ. All rights reserved.</p>
          </div>
        </div>
      </footer>
    </main>
  )
}

// Feature Card Component
function FeatureCard({ icon, title, description }: { icon: React.ReactNode, title: string, description: string }) {
  return (
    <motion.div 
      className="bg-white dark:bg-quantum-dark-800 p-6 rounded-lg shadow-lg"
      whileHover={{ y: -5, boxShadow: '0 10px 25px -5px rgba(0, 0, 0, 0.1)' }}
      transition={{ duration: 0.2 }}
    >
      <div className="mb-4">{icon}</div>
      <h3 className="text-xl font-semibold mb-2">{title}</h3>
      <p className="text-quantum-dark-600 dark:text-quantum-dark-300">{description}</p>
    </motion.div>
  )
} 