'use client';

import Link from "next/link";
import { ArrowRight, Code, Box, Zap, Shield, Github, Twitter, Book } from "lucide-react";

function FeatureCard({ icon, title, description }: { icon: React.ReactNode, title: string, description: string }) {
  return (
    <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg transform transition-all duration-200 hover:-translate-y-1 hover:shadow-xl">
      <div className="mb-4">{icon}</div>
      <h3 className="text-xl font-semibold mb-2">{title}</h3>
      <p className="text-gray-600 dark:text-gray-300">{description}</p>
    </div>
  );
}

// Add custom CSS for orbit animation
const orbitStyle = `
@keyframes orbit {
  from { transform: translate(-50%, -50%) rotate(0deg) translateX(var(--radius)) rotate(0deg); }
  to { transform: translate(-50%, -50%) rotate(360deg) translateX(var(--radius)) rotate(-360deg); }
}
.animate-orbit {
  animation: orbit var(--duration) linear infinite;
}
`;

export default function Home() {
  return (
    <main className="overflow-hidden">
      {/* Add the orbit animation CSS */}
      <style jsx global>{orbitStyle}</style>
      
      {/* Hero Section */}
      <section className="relative min-h-screen flex items-center justify-center overflow-hidden bg-gradient-to-b from-gray-950 via-indigo-950 to-gray-950">
        {/* Animated particle background */}
        <div className="absolute inset-0 z-0">
          <div className="absolute inset-0 opacity-20 bg-[radial-gradient(#6366f133_1px,transparent_1px)] [background-size:20px_20px]"></div>
          <div className="absolute inset-0 opacity-10 bg-[radial-gradient(#a5b4fc33_2px,transparent_2px)] [background-size:30px_30px]"></div>
        </div>
        
        {/* Glowing accent elements */}
        <div className="absolute top-1/4 -left-20 w-80 h-80 bg-indigo-600/20 rounded-full filter blur-3xl opacity-30 animate-pulse"></div>
        <div className="absolute bottom-1/4 -right-20 w-80 h-80 bg-blue-600/20 rounded-full filter blur-3xl opacity-30 animate-pulse" style={{ animationDelay: '1s' }}></div>
        
        <div className="container mx-auto relative z-10 flex flex-col items-center justify-center py-20 text-center px-4">
          <div className="relative z-10 flex flex-col items-center w-full max-w-5xl px-5 mx-auto text-center">
            <h1 
              className="text-4xl font-bold tracking-tight sm:text-5xl md:text-6xl lg:text-7xl dark:text-white animate-fade-in"
            >
              <span className="block bg-clip-text text-transparent bg-gradient-to-r from-indigo-200 to-indigo-400 pb-1">HaskQ: Type-Safe</span>{" "}
              <span className="block bg-clip-text text-transparent bg-gradient-to-r from-indigo-400 to-purple-300 pb-3">Quantum Programming</span>
            </h1>
            <p 
              className="max-w-md mx-auto mt-5 text-xl text-gray-500 dark:text-gray-400 animate-fade-in" 
              style={{ animationDelay: '200ms' }}
            >
              A Haskell library for quantum computing with built-in simulation capabilities and a powerful visualization toolkit.
            </p>
            <div className="flex flex-col items-center justify-center w-full mt-8 space-y-4 sm:flex-row sm:space-y-0 sm:space-x-4">
              <a
                href="/docs/intro"
                className="inline-flex items-center justify-center w-full px-8 py-3 text-base font-medium text-white border border-transparent rounded-md shadow-sm bg-primary-500 hover:bg-primary-600 sm:w-auto animate-fade-in" 
                style={{ animationDelay: '400ms' }}
              >
                Get Started
              </a>
              <a
                href="https://github.com/haskell-monad/haskq"
                className="inline-flex items-center justify-center w-full px-8 py-3 text-base font-medium text-gray-700 bg-white border border-gray-300 rounded-md shadow-sm dark:bg-gray-800 dark:border-gray-700 dark:text-gray-300 dark:hover:bg-gray-700 hover:bg-gray-50 sm:w-auto animate-fade-in" 
                style={{ animationDelay: '600ms' }}
              >
                View on GitHub
              </a>
            </div>
          </div>
          
          <div className="mt-20 animate-fade-in" style={{ animationDelay: '0.7s' }}>
            <div className="relative h-64 w-64 sm:h-80 sm:w-80 mx-auto">
              {/* Enhanced quantum circuit visualization */}
              <div className="absolute inset-0 rounded-full bg-gradient-to-br from-indigo-600/20 to-purple-600/20 backdrop-blur-sm"></div>
              <div className="absolute inset-4 rounded-full bg-gradient-to-tr from-indigo-500/10 to-purple-500/10 animate-pulse"></div>
              
              {/* Orbital rings */}
              <div className="absolute inset-0 rounded-full border border-indigo-500/20 animate-spin-slow"></div>
              <div className="absolute inset-8 rounded-full border border-indigo-400/30 animate-spin-reverse"></div>
              <div className="absolute inset-16 rounded-full border border-indigo-300/40 animate-spin-slower"></div>
              
              {/* Central quantum core */}
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-16 h-16 bg-gradient-to-br from-indigo-400 to-purple-400 rounded-full blur-sm animate-pulse"></div>
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-8 h-8 bg-white rounded-full blur-sm"></div>
              
              {/* Quantum particles */}
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-2 h-2 bg-indigo-400 rounded-full animate-orbit" style={{ '--radius': '60px', '--duration': '5s' } as React.CSSProperties}></div>
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-2 h-2 bg-purple-400 rounded-full animate-orbit" style={{ '--radius': '60px', '--duration': '5s', animationDelay: '2.5s' } as React.CSSProperties}></div>
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-2 h-2 bg-blue-400 rounded-full animate-orbit" style={{ '--radius': '100px', '--duration': '8s' } as React.CSSProperties}></div>
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-2 h-2 bg-indigo-200 rounded-full animate-orbit" style={{ '--radius': '100px', '--duration': '8s', animationDelay: '4s' } as React.CSSProperties}></div>
              <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-2 h-2 bg-cyan-400 rounded-full animate-orbit" style={{ '--radius': '130px', '--duration': '12s' } as React.CSSProperties}></div>
            </div>
            
            {/* Code snippet preview */}
            <div className="mt-8 relative max-w-md mx-auto animate-fade-in" style={{ animationDelay: '1s' }}>
              <div className="bg-gray-900/50 backdrop-blur-sm border border-indigo-500/20 rounded-lg p-4 text-left shadow-xl">
                <pre className="text-xs sm:text-sm font-mono text-indigo-200">
                  <code>{`-- Bell state circuit with HaskQ
bellState :: Circ (Qubit, Qubit)
bellState = do
  q1 <- createQubit Zero  
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')`}</code>
                </pre>
              </div>
            </div>
          </div>
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
      <section className="py-20 bg-gradient-to-r from-indigo-600 to-indigo-800 dark:from-indigo-800 dark:to-purple-900 text-white" id="get-started">
        <div className="container mx-auto px-4 text-center">
          <h2 className="text-3xl font-bold mb-6">Ready to Dive Into Quantum?</h2>
          <p className="mx-auto mb-10 max-w-2xl text-lg opacity-90">
            Start building quantum circuits with HaskQ today. Check out our documentation, examples, and join our community.
          </p>
          <div className="flex flex-wrap justify-center gap-4">
            <Link 
              href="/docs/intro" 
              className="bg-white text-indigo-700 hover:bg-indigo-50 rounded-md px-6 py-3 transition-colors font-medium border border-white shadow-lg"
              style={{ backgroundColor: '#ffffff !important', color: '#3730a3 !important' }}
            >
              Get Started Guide
            </Link>
            <a 
              href="https://github.com/ArsCodeAmatoria/HaskQ" 
              className="bg-indigo-700 hover:bg-indigo-800 text-white rounded-md px-6 py-3 flex items-center justify-center transition-colors font-medium border border-indigo-500"
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
