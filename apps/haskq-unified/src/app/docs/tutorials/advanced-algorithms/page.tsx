'use client';

import { DocLayout } from '@/components/DocLayout';
import Link from 'next/link';
import { BookOpen, Code, Cpu, ChevronRight, ArrowRight, GitBranch, Layers, Workflow } from 'lucide-react';

export default function AdvancedAlgorithmsPage() {
  const algorithms = [
    {
      title: "Shor's Algorithm",
      description: "Integer factorization with exponential quantum speedup",
      difficulty: "Advanced",
      applications: ["Cryptography", "RSA Breaking", "Number Theory"],
      icon: <Code className="h-6 w-6 text-red-500" />
    },
    {
      title: "Variational Quantum Eigensolver (VQE)",
      description: "Quantum chemistry ground state energy calculation",
      difficulty: "Advanced", 
      applications: ["Drug Discovery", "Materials Science", "Catalyst Design"],
      icon: <Workflow className="h-6 w-6 text-red-500" />
    },
    {
      title: "QAOA Max-Cut",
      description: "Quantum approximate optimization for graph problems",
      difficulty: "Advanced",
      applications: ["Graph Problems", "Portfolio Optimization", "Logistics"],
      icon: <GitBranch className="h-6 w-6 text-red-500" />
    },
    {
      title: "Quantum Walk Search",
      description: "Quantum walk with quadratic search speedup",
      difficulty: "Intermediate",
      applications: ["Database Search", "Network Analysis", "Algorithm Design"],
      icon: <Layers className="h-6 w-6 text-yellow-500" />
    },
    {
      title: "Quantum Machine Learning",
      description: "Quantum neural network for pattern recognition",
      difficulty: "Advanced",
      applications: ["Pattern Recognition", "Financial Modeling", "Optimization"],
      icon: <Cpu className="h-6 w-6 text-red-500" />
    },
    {
      title: "BB84 Quantum Cryptography",
      description: "Quantum key distribution with perfect security",
      difficulty: "Advanced",
      applications: ["Secure Communications", "Banking", "Internet Security"],
      icon: <Code className="h-6 w-6 text-red-500" />
    },
    {
      title: "Quantum Error Correction",
      description: "Protect quantum information from decoherence",
      difficulty: "Advanced",
      applications: ["Fault-Tolerant Computing", "Quantum Memory", "Scientific Computing"],
      icon: <BookOpen className="h-6 w-6 text-red-500" />
    },
    {
      title: "Quantum Phase Estimation",
      description: "Estimates eigenvalues of unitary operators with exponential precision",
      difficulty: "Advanced",
      applications: ["Quantum Chemistry", "Prime Factorization", "Quantum Metrology"],
      icon: <Workflow className="h-6 w-6 text-red-500" />
    },
    {
      title: "Adiabatic Quantum Computing",
      description: "Quantum optimization using adiabatic evolution",
      difficulty: "Advanced",
      applications: ["Combinatorial Optimization", "Machine Learning", "Scientific Computing"],
      icon: <ArrowRight className="h-6 w-6 text-red-500" />
    },
    {
      title: "Quantum Supremacy Circuits",
      description: "Random quantum circuit demonstrating computational advantage",
      difficulty: "Advanced",
      applications: ["Quantum Advantage Demonstration", "Benchmarking", "Research"],
      icon: <ChevronRight className="h-6 w-6 text-red-500" />
    }
  ];

  const getDifficultyColor = (difficulty: string) => {
    switch (difficulty) {
      case 'Intermediate':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/20 dark:text-yellow-200';
      case 'Advanced':
        return 'bg-red-100 text-red-800 dark:bg-red-900/20 dark:text-red-200';
      default:
        return 'bg-gray-100 text-gray-800 dark:bg-gray-900/20 dark:text-gray-200';
    }
  };

  return (
    <DocLayout 
      title="Advanced Quantum Algorithms" 
      description="Comprehensive guide to cutting-edge quantum algorithms that demonstrate quantum advantage and enable breakthrough applications in cryptography, optimization, machine learning, and scientific simulation."
    >
      <div className="space-y-8">
        <div className="bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-900/20 dark:to-purple-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800">
          <h2 className="text-2xl font-bold mb-4 text-indigo-900 dark:text-indigo-100">
            Welcome to Advanced Quantum Algorithms
          </h2>
          <p className="text-indigo-800 dark:text-indigo-200 mb-4">
            This comprehensive tutorial covers cutting-edge quantum algorithms that demonstrate quantum advantage 
            and enable breakthrough applications in cryptography, optimization, machine learning, and scientific simulation.
          </p>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm">
            <div className="bg-white/50 dark:bg-gray-800/50 rounded p-3">
              <strong className="text-indigo-900 dark:text-indigo-100">10+ Algorithms</strong>
              <p className="text-indigo-700 dark:text-indigo-300">From Shor's factorization to quantum machine learning</p>
            </div>
            <div className="bg-white/50 dark:bg-gray-800/50 rounded p-3">
              <strong className="text-indigo-900 dark:text-indigo-100">Real Applications</strong>
              <p className="text-indigo-700 dark:text-indigo-300">Cryptography, chemistry, optimization, and more</p>
            </div>
            <div className="bg-white/50 dark:bg-gray-800/50 rounded p-3">
              <strong className="text-indigo-900 dark:text-indigo-100">Hands-on Examples</strong>
              <p className="text-indigo-700 dark:text-indigo-300">Complete HaskQ implementations ready to run</p>
            </div>
          </div>
        </div>

        <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-6 border border-blue-100 dark:border-blue-800">
          <h3 className="text-xl font-bold mb-3 text-blue-900 dark:text-blue-100">Prerequisites</h3>
          <p className="text-blue-800 dark:text-blue-200 mb-3">
            Before diving into these advanced algorithms, ensure you understand:
          </p>
          <ul className="list-disc ml-6 space-y-1 text-blue-800 dark:text-blue-200">
            <li>Basic quantum gates (Hadamard, CNOT, Pauli gates)</li>
            <li>Quantum superposition and entanglement</li>
            <li>Quantum measurement and probability amplitudes</li>
            <li>The Quantum Fourier Transform (QFT)</li>
          </ul>
          <p className="mt-3 text-blue-800 dark:text-blue-200">
            If you need to review these concepts, check out our{' '}
            <Link href="/docs/core-concepts/quantum-computing-basics" className="text-blue-600 dark:text-blue-400 hover:underline">
              Core Concepts
            </Link>{' '}
            and{' '}
            <Link href="/docs/tutorials/bell-states" className="text-blue-600 dark:text-blue-400 hover:underline">
              Basic Tutorials
            </Link>.
          </p>
        </div>

        <div>
          <h2 className="text-2xl font-bold mb-6">Algorithm Overview</h2>
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {algorithms.map((algorithm, index) => (
              <div 
                key={index}
                className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border border-gray-200 dark:border-gray-700 hover:shadow-xl transition-shadow"
              >
                <div className="flex items-start justify-between mb-3">
                  <div className="flex items-center space-x-3">
                    {algorithm.icon}
                    <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                      {algorithm.title}
                    </h3>
                  </div>
                  <span className={`text-xs px-2 py-1 rounded-full font-medium ${getDifficultyColor(algorithm.difficulty)}`}>
                    {algorithm.difficulty}
                  </span>
                </div>
                
                <p className="text-gray-600 dark:text-gray-300 mb-4">
                  {algorithm.description}
                </p>
                
                <div>
                  <h4 className="text-sm font-medium text-gray-900 dark:text-white mb-2">Applications:</h4>
                  <div className="flex flex-wrap gap-1">
                    {algorithm.applications.map((app, appIndex) => (
                      <span 
                        key={appIndex}
                        className="text-xs bg-indigo-100 text-indigo-700 dark:bg-indigo-900/30 dark:text-indigo-300 px-2 py-1 rounded"
                      >
                        {app}
                      </span>
                    ))}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>

        <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-6 border border-green-100 dark:border-green-800">
          <h3 className="text-xl font-bold mb-3 text-green-900 dark:text-green-100">Getting Started</h3>
          <div className="space-y-3 text-green-800 dark:text-green-200">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <h4 className="font-semibold mb-2">Running the Examples</h4>
                <ol className="list-decimal ml-4 space-y-1 text-sm">
                  <li>Open the <Link href="/playground" className="text-green-600 dark:text-green-400 hover:underline">HaskQ Playground</Link></li>
                  <li>Select an algorithm from the examples panel</li>
                  <li>Study the code structure and operations</li>
                  <li>Modify parameters and experiment</li>
                  <li>Run simulations and analyze results</li>
                </ol>
              </div>
              <div>
                <h4 className="font-semibold mb-2">Parameter Tuning</h4>
                <ul className="list-disc ml-4 space-y-1 text-sm">
                  <li>Circuit depth and complexity</li>
                  <li>Optimization parameters for variational algorithms</li>
                  <li>Problem size and qubit count</li>
                  <li>Noise and error mitigation settings</li>
                </ul>
              </div>
            </div>
          </div>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="bg-purple-50 dark:bg-purple-900/20 rounded-lg p-6 border border-purple-100 dark:border-purple-800">
            <h3 className="text-xl font-bold mb-3 text-purple-900 dark:text-purple-100">Quantum Advantage</h3>
            <p className="text-purple-800 dark:text-purple-200 mb-3">
              These algorithms demonstrate various types of quantum advantage:
            </p>
            <ul className="list-disc ml-4 space-y-1 text-purple-800 dark:text-purple-200 text-sm">
              <li><strong>Exponential speedup</strong>: Shor's algorithm for factorization</li>
              <li><strong>Quadratic speedup</strong>: Grover's search and quantum walks</li>
              <li><strong>Simulation advantage</strong>: VQE for quantum chemistry</li>
              <li><strong>Security advantage</strong>: BB84 quantum cryptography</li>
            </ul>
          </div>

          <div className="bg-orange-50 dark:bg-orange-900/20 rounded-lg p-6 border border-orange-100 dark:border-orange-800">
            <h3 className="text-xl font-bold mb-3 text-orange-900 dark:text-orange-100">Real-World Impact</h3>
            <p className="text-orange-800 dark:text-orange-200 mb-3">
              Applications transforming industries:
            </p>
            <ul className="list-disc ml-4 space-y-1 text-orange-800 dark:text-orange-200 text-sm">
              <li><strong>Cryptography</strong>: Breaking RSA, post-quantum security</li>
              <li><strong>Drug Discovery</strong>: Molecular simulation and binding</li>
              <li><strong>Finance</strong>: Portfolio optimization and risk analysis</li>
              <li><strong>AI/ML</strong>: Quantum-enhanced machine learning</li>
            </ul>
          </div>
        </div>

        <div className="bg-gray-50 dark:bg-gray-800/50 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
          <h3 className="text-xl font-bold mb-3">Next Steps</h3>
          <p className="mb-4">
            Ready to dive deeper? Here's your learning path:
          </p>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Link 
              href="/playground" 
              className="flex items-center p-4 bg-indigo-100 dark:bg-indigo-900/30 rounded-lg hover:bg-indigo-200 dark:hover:bg-indigo-900/50 transition-colors"
            >
              <Code className="h-5 w-5 text-indigo-600 dark:text-indigo-400 mr-3" />
              <div>
                <div className="font-medium text-indigo-900 dark:text-indigo-100">Try the Playground</div>
                <div className="text-sm text-indigo-700 dark:text-indigo-300">Interactive algorithm experimentation</div>
              </div>
            </Link>
            
            <Link 
              href="/docs/tutorials/consciousness-algorithms" 
              className="flex items-center p-4 bg-purple-100 dark:bg-purple-900/30 rounded-lg hover:bg-purple-200 dark:hover:bg-purple-900/50 transition-colors"
            >
              <BookOpen className="h-5 w-5 text-purple-600 dark:text-purple-400 mr-3" />
              <div>
                <div className="font-medium text-purple-900 dark:text-purple-100">Consciousness Algorithms</div>
                <div className="text-sm text-purple-700 dark:text-purple-300">Cutting-edge quantum consciousness research</div>
              </div>
            </Link>
            
            <Link 
              href="/docs/core-concepts/quantum-algorithms" 
              className="flex items-center p-4 bg-green-100 dark:bg-green-900/30 rounded-lg hover:bg-green-200 dark:hover:bg-green-900/50 transition-colors"
            >
              <Workflow className="h-5 w-5 text-green-600 dark:text-green-400 mr-3" />
              <div>
                <div className="font-medium text-green-900 dark:text-green-100">Theory Deep Dive</div>
                <div className="text-sm text-green-700 dark:text-green-300">Mathematical foundations and proofs</div>
              </div>
            </Link>
          </div>
        </div>

        <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800">
          <h3 className="text-xl font-bold mb-3 text-indigo-900 dark:text-indigo-100">Practice Exercises</h3>
          <p className="text-indigo-800 dark:text-indigo-200 mb-4">
            Challenge yourself with these hands-on exercises:
          </p>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
            <div>
              <ol className="list-decimal ml-4 space-y-2 text-indigo-800 dark:text-indigo-200">
                <li><strong>Modify Shor's Algorithm</strong>: Try factoring different small integers</li>
                <li><strong>Design VQE Circuits</strong>: Create ansatz circuits for different molecules</li>
                <li><strong>Optimize QAOA</strong>: Find better parameters for graph problems</li>
              </ol>
            </div>
            <div>
              <ol className="list-decimal ml-4 space-y-2 text-indigo-800 dark:text-indigo-200" start={4}>
                <li><strong>Implement Variations</strong>: Create your own quantum walk or ML models</li>
                <li><strong>Combine Algorithms</strong>: Use quantum phase estimation in other contexts</li>
                <li><strong>Benchmark Performance</strong>: Compare quantum vs classical solutions</li>
              </ol>
            </div>
          </div>
        </div>

        <div className="text-center py-8">
          <p className="text-lg text-gray-600 dark:text-gray-300 mb-4">
            Ready to explore the future of quantum computing?
          </p>
          <Link 
            href="/playground"
            className="inline-flex items-center px-6 py-3 bg-indigo-600 text-white rounded-lg hover:bg-indigo-700 transition-colors font-medium"
          >
            <Code className="h-5 w-5 mr-2" />
            Start Programming Quantum Algorithms
          </Link>
        </div>
      </div>
    </DocLayout>
  );
} 