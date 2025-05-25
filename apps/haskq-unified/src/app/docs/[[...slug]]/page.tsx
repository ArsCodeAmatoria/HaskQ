'use client';

import { useCallback, useEffect, useState } from 'react';
import { notFound, usePathname, useParams } from 'next/navigation';
import Link from 'next/link';
import { Book, Code, ArrowRight } from 'lucide-react';
import React from 'react';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import dynamic from 'next/dynamic';

// Improved DocsIndexPage with better spacing and styling
function DocsIndexPage() {
  const sections = [
    {
      title: 'Getting Started',
      description: 'Learn how to install HaskQ and set up your first quantum circuit',
      href: '/docs/getting-started',
      icon: <Book className="h-8 w-8 text-indigo-500" />,
    },
    {
      title: 'Core Concepts',
      description: 'Understand the fundamental concepts of quantum computing with HaskQ',
      href: '/docs/core-concepts/quantum-computing-basics',
      icon: <Code className="h-8 w-8 text-indigo-500" />,
    },
    {
      title: 'Tutorials',
      description: 'Step-by-step guides to building quantum circuits and algorithms',
      href: '/docs/tutorials/bell-states',
      icon: <ArrowRight className="h-8 w-8 text-indigo-500" />,
    },
  ];
  
  return (
    <div>
      <p className="mb-8 text-gray-600 dark:text-gray-300 max-w-3xl">
        Welcome to the HaskQ documentation. Here you'll find comprehensive guides and documentation to help you start working with HaskQ as quickly as possible.
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-12">
        {sections.map((section) => (
          <Link 
            key={section.href} 
            href={section.href}
            className="flex flex-col h-full bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-shadow border border-gray-100 dark:border-gray-700"
          >
            <div className="mb-4">{section.icon}</div>
            <h2 className="text-xl font-semibold text-indigo-600 dark:text-indigo-400 mb-2">{section.title}</h2>
            <p className="text-gray-600 dark:text-gray-300 flex-grow">{section.description}</p>
          </Link>
        ))}
      </div>
      
      <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800">
        <h2 className="text-xl font-semibold mb-4">Getting Help</h2>
        <p className="mb-4">
          If you have questions or need help with HaskQ, there are several ways to get assistance:
        </p>
        <ul className="list-disc list-inside space-y-2 text-gray-700 dark:text-gray-300">
          <li>Check out the <Link href="/docs/getting-started" className="text-indigo-600 dark:text-indigo-400 hover:underline">Getting Started</Link> guide</li>
          <li>Browse the <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Core Concepts</Link> documentation</li>
          <li>Visit our <a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">GitHub repository</a> to file issues or contribute</li>
        </ul>
      </div>
    </div>
  );
}

// Static mapping for documentation pages with improved code formatting
const pageMapping = {
  'intro': {
    title: 'Introduction to HaskQ',
    description: 'A functional quantum programming language',
    content: () => (
      <>
        <p className="mb-6">HaskQ is a quantum programming language that combines the elegance of Haskell with the power of quantum computing. It provides a type-safe, purely functional approach to quantum circuit design and simulation.</p>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Why HaskQ?</h2>
        <p className="mb-4">Quantum computing is a fundamentally different paradigm from classical computing, and traditional programming languages weren't designed with quantum mechanics in mind. HaskQ addresses this gap by:</p>
        <ul className="list-disc ml-6 my-4 space-y-2">
          <li><strong>Enforcing quantum mechanics laws at compile time</strong> through linear types</li>
          <li><strong>Providing clean, functional abstractions</strong> for quantum circuit design</li>
          <li><strong>Including a built-in simulator</strong> for testing and debugging quantum algorithms</li>
          <li><strong>Enabling seamless integration</strong> with classical Haskell code</li>
        </ul>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Key Features</h2>
        <h3 className="text-xl font-bold mt-6 mb-3">Type-Safe Quantum Programming</h3>
        <p className="mb-4">HaskQ uses Haskell's linear types to enforce the no-cloning theorem at compile time. This means the compiler prevents you from writing programs that violate the laws of quantum mechanics.</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- This will compile
validCircuit :: Circ (Qubit, Qubit)
validCircuit = do
  q <- createQubit Zero
  q' <- hadamard q
  (q'', q''') <- controlled not q' (createQubit Zero)
  pure (q'', q''')

-- This will NOT compile - attempts to use q twice
invalidCircuit :: Circ (Qubit, Qubit)
invalidCircuit = do
  q <- createQubit Zero
  q' <- hadamard q
  q'' <- pauliX q'  -- This uses q'
  q''' <- hadamard q'  -- Error! q' was already consumed
  pure (q'', q''')`}
          className="my-6"
        />
      </>
    )
  },
  'core-concepts/simulation': {
    title: 'Quantum Simulation',
    description: 'Understand how the HaskQ simulator works and how to use it effectively',
    content: () => (
      <>
        <p className="mb-6">HaskQ includes a powerful quantum circuit simulator that allows you to test and analyze quantum algorithms without a physical quantum computer. This document explains how simulation works in HaskQ and how to use it effectively.</p>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Getting Started with HaskQ Simulator</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Installation</h3>
        <p className="mb-4">To use the HaskQ simulator, first ensure you have the HaskQ package installed:</p>
        
        <CodeBlock 
          language="bash" 
          code={`# Install HaskQ from source
git clone https://github.com/haskq/haskq.git
cd haskq
cabal install

# Or using stack
stack install`}
          className="my-6"
        />
        
        <p className="mb-4">Once installed, you'll have access to the command-line simulator <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">haskq-sim</code> and the simulator library for use in your Haskell programs.</p>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Quick Start Example</h3>
        <p className="mb-4">Create a file named <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">BellState.hs</code> with the following content:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)

main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn $ "Measurement results: " ++ show (measurements result)`}
          className="my-6"
        />
        
        <p className="mb-4">Compile and run:</p>
        
        <CodeBlock 
          language="bash" 
          code={`ghc -o bell-state BellState.hs
./bell-state`}
          className="my-6"
        />
        
        <p className="mb-4">You should see either <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">[Zero, Zero]</code> or <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">[One, One]</code> as the output, demonstrating quantum entanglement.</p>
      </>
    )
  },
  'core-concepts/quantum-computing-basics': {
    title: 'Quantum Computing Basics',
    description: 'A brief introduction to the fundamental concepts of quantum computing',
    content: () => {
      const QuantumComputingBasics = dynamic(() => import('@/app/docs/core-concepts/quantum-computing-basics/page'), { ssr: true });
      return <QuantumComputingBasics />;
    }
  },
  'core-concepts/superposition': {
    title: 'Quantum Superposition',
    description: 'Understanding quantum superposition and its role in quantum computing',
    content: () => {
      const SuperpositionPage = dynamic(() => import('@/app/docs/core-concepts/superposition/page'), { ssr: true });
      return <SuperpositionPage />;
    }
  },
  'core-concepts/entanglement': {
    title: 'Quantum Entanglement',
    description: 'Understanding quantum entanglement and its applications in quantum computing',
    content: () => {
      const EntanglementPage = dynamic(() => import('@/app/docs/core-concepts/entanglement/page'), { ssr: true });
      return <EntanglementPage />;
    }
  },
  'core-concepts/quantum-gates': {
    title: 'Quantum Gates',
    description: 'Understanding quantum gates and their implementation in HaskQ',
    content: () => {
      const QuantumGatesPage = dynamic(() => import('@/app/docs/core-concepts/quantum-gates/page'), { ssr: true });
      return <QuantumGatesPage />;
    }
  },
  'core-concepts/measurement': {
    title: 'Quantum Measurement',
    description: 'Understanding quantum measurement and its implementation in HaskQ',
    content: () => {
      const MeasurementPage = dynamic(() => import('@/app/docs/core-concepts/measurement/page'), { ssr: true });
      return <MeasurementPage />;
    }
  },
  'core-concepts/quantum-circuits': {
    title: 'Quantum Circuits',
    description: 'Understanding quantum circuits and their implementation in HaskQ',
    content: () => {
      const QuantumCircuitsPage = dynamic(() => import('@/app/docs/core-concepts/quantum-circuits/page'), { ssr: true });
      return <QuantumCircuitsPage />;
    }
  },
  'core-concepts/quantum-algorithms': {
    title: 'Quantum Algorithms',
    description: 'Understanding key quantum algorithms and their implementation in HaskQ',
    content: () => {
      const QuantumAlgorithmsPage = dynamic(() => import('@/app/docs/core-concepts/quantum-algorithms/page'), { ssr: true });
      return <QuantumAlgorithmsPage />;
    }
  },
  'getting-started': {
    title: 'Getting Started with HaskQ',
    description: 'Learn how to install and set up HaskQ for your first quantum circuit',
    content: () => (
      <>
        <p className="mb-6">
          This guide will help you get started with HaskQ - from installation to creating your first quantum circuit.
          HaskQ is designed to make quantum programming accessible to Haskell developers while maintaining the
          benefits of Haskell's strong type system.
        </p>

        <InfoBox type="note">
          <p>HaskQ is currently in active development. APIs may change as we refine the language's design.</p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Prerequisites</h2>
        <p className="mb-4">Before you begin, ensure you have the following installed:</p>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>GHC (Glasgow Haskell Compiler) 9.2 or later</li>
          <li>Cabal 3.6 or later or Stack 2.7 or later</li>
          <li>Git</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Installation</h2>
        <p className="mb-4">You can install HaskQ from source using either Cabal or Stack:</p>

        <h3 className="text-xl font-bold mt-6 mb-3">Using Cabal</h3>
        <CodeBlock 
          language="bash" 
          code={`# Clone the repository
git clone https://github.com/haskq/haskq.git
cd haskq

# Install dependencies and build
cabal update
cabal install`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Using Stack</h3>
        <CodeBlock 
          language="bash" 
          code={`# Clone the repository
git clone https://github.com/haskq/haskq.git
cd haskq

# Install dependencies and build
stack setup
stack install`}
          className="my-6"
        />

        <InfoBox type="tip" title="Recommended Setup">
          <p>We recommend using Stack for a more consistent build experience across different environments. 
          Stack will automatically install the correct GHC version for you.</p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Your First Quantum Circuit</h2>
        <p className="mb-4">
          Let's create a simple quantum circuit that creates a Bell state - one of the simplest examples
          of quantum entanglement.
        </p>

        <p className="mb-4">Create a new file named <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">BellState.hs</code> with the following content:</p>

        <CodeBlock 
          language="haskell" 
          code={`module Main where

import HaskQ.Prelude
import HaskQ.Simulator.Circuit

-- Create a Bell state circuit
bellState :: Circ (Qubit, Qubit)
bellState = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- controlled not q1' q2
  pure (q1'', q2')

main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn "Bell State Simulation Results:"
  putStrLn $ "Measurements: " ++ show (measurements result)
  putStrLn $ "State Vector: " ++ show (stateVector result)`}
          className="my-6"
        />

        <p className="mb-4">Compile and run this program:</p>

        <CodeBlock 
          language="bash" 
          code={`ghc -o bell-state BellState.hs
./bell-state`}
          className="my-6"
        />

        <p className="mb-4">
          You should see output showing that the measurement will always result in either both qubits being <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Zero</code> or both being <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">One</code>, demonstrating quantum entanglement.
        </p>

        <InfoBox type="note" title="Understanding Bell States">
          <p className="mb-3">
            A Bell state is a maximally entangled quantum state of two qubits. After creating a Bell state, 
            measuring one qubit immediately determines the state of the other, regardless of the distance between them.
          </p>
          <p>
            This phenomenon, known as quantum entanglement, is one of the most fascinating aspects of quantum mechanics 
            and has no classical equivalent.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Visualization</h2>
        <p className="mb-4">
          HaskQ also includes tools for visualizing quantum circuits. To generate a visualization of your Bell state circuit,
          modify the main function as follows:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`import qualified HaskQ.Visualization as Viz

main :: IO ()
main = do
  -- Simulate the circuit
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn "Bell State Simulation Results:"
  putStrLn $ "Measurements: " ++ show (measurements result)
  
  -- Visualize the circuit
  let circuit = bellState >>= \\(q1, q2) -> do
        (_, _) <- measure q1
        (_, _) <- measure q2
        pure ()
        
  Viz.drawCircuit "bell-state-circuit.svg" circuit`}
          className="my-6"
        />

        <InfoBox type="warning">
          <p>The visualization API requires additional dependencies, including Cairo and Pango.
          If you encounter errors, check the installation guide for platform-specific requirements.</p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Next Steps</h2>
        <p className="mb-4">Now that you've created your first quantum circuit, you can:</p>
        <ul className="list-disc ml-6 space-y-2">
          <li>Learn more about <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">quantum computing basics</Link></li>
          <li>Explore <Link href="/docs/core-concepts/simulation" className="text-indigo-600 dark:text-indigo-400 hover:underline">HaskQ's simulator</Link> in more depth</li>
          <li>Try implementing more advanced <Link href="/docs/tutorials/algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">quantum algorithms</Link></li>
        </ul>
      </>
    )
  },
  'installation': {
    title: 'Installation',
    description: 'How to install and set up HaskQ on your system',
    content: () => (
      <>
        <p className="mb-6">
          This guide covers how to install and set up HaskQ on your system.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Prerequisites</h2>
        <p className="mb-4">Before installing HaskQ, ensure you have the following tools installed:</p>
        
        <h3 className="text-xl font-bold mt-6 mb-3">For All Components</h3>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>Git</li>
          <li>Node.js (v18 or later)</li>
          <li>npm (v9 or later)</li>
        </ul>

        <h3 className="text-xl font-bold mt-6 mb-3">For Haskell Packages</h3>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>GHC (Glasgow Haskell Compiler) version 9.2 or later with LinearTypes extension</li>
          <li>Cabal 3.6 or later</li>
          <li>Stack (optional)</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Installation Methods</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Method 1: From Source (Recommended for Development)</h3>
        <p className="mb-4">Clone the repository and build from source:</p>
        
        <CodeBlock 
          language="bash" 
          code={`# Clone the repository
git clone https://github.com/ArsCodeAmatoria/HaskQ.git
cd HaskQ

# Install dependencies
npm install

# Build all packages
npm run build`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Method 2: Using Cabal (For Haskell Packages Only)</h3>
        
        <CodeBlock 
          language="bash" 
          code={`# Install core package
cabal install haskq-core

# Install simulator package
cabal install haskq-simulator`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Method 3: Using Stack (For Haskell Packages Only)</h3>
        <p className="mb-4">Create a <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">stack.yaml</code> file with the following content:</p>
        
        <CodeBlock 
          language="yaml" 
          code={`resolver: lts-20.11  # or a more recent resolver
packages:
- packages/haskq-core
- packages/haskq-simulator
extra-deps:
- linear-base-0.3.0`}
          className="my-6"
        />
        
        <p className="mb-4">Then run:</p>
        
        <CodeBlock 
          language="bash" 
          code={`stack build`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Verifying Installation</h2>
        <p className="mb-4">After installation, verify that everything is working correctly:</p>
        
        <CodeBlock 
          language="bash" 
          code={`# Run the simulator CLI
cabal run haskq-sim -- --circuit bell --output ascii

# Start the development servers
npm run dev`}
          className="my-6"
        />
        
        <p className="mb-4">You should see:</p>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>The simulator outputs a Bell state circuit representation</li>
          <li>The web applications start on their respective ports</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Directory Structure</h2>
        <p className="mb-4">After installation, your project structure should look like this:</p>
        
        <CodeBlock 
          language="bash" 
          code={`HaskQ/
├── apps/
│   ├── landing/     # Landing page (Next.js)
│   ├── docs/        # Documentation (Docusaurus)
│   └── playground/  # Interactive playground (Next.js)
├── packages/
│   ├── haskq-core/      # Core quantum DSL
│   └── haskq-simulator/ # Quantum simulator
└── ... (root configuration files)`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Common Issues</h2>
        
        <InfoBox type="warning" title="Missing LinearTypes Extension">
          <p className="mb-3">If you encounter an error about the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">LinearTypes</code> extension, ensure you're using GHC 9.2 or later:</p>
          <CodeBlock 
            language="bash" 
            code={`ghc --version`}
            className="my-3"
          />
          <p>If you have an older version, update your GHC installation.</p>
        </InfoBox>
        
        <InfoBox type="note" title="Dependency Issues">
          <p className="mb-3">If you have dependency issues, you might need to update your package index:</p>
          <CodeBlock 
            language="bash" 
            code={`cabal update`}
            className="my-3"
          />
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Next Steps</h2>
        <p className="mb-4">
          Now that you have HaskQ installed, proceed to the <Link href="/docs/getting-started" className="text-indigo-600 dark:text-indigo-400 hover:underline">Getting Started</Link> guide to create your first quantum circuit.
        </p>
      </>
    )
  },
  'project-structure': {
    title: 'Project Structure',
    description: 'Understanding the organization of the HaskQ codebase',
    content: () => (
      <>
        <p className="mb-6">
          Understanding the structure of the HaskQ project will help you navigate the codebase and contribute effectively.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Overview</h2>
        <p className="mb-4">
          HaskQ is organized as a monorepo using npm workspaces, containing both Haskell packages and JavaScript applications. 
          Here's a high-level view of the project structure:
        </p>
        
        <CodeBlock 
          language="bash" 
          code={`HaskQ/
├── apps/                  # Web applications
├── packages/              # Haskell libraries
├── package.json           # Root package.json with workspace configuration
├── .gitignore             # Git ignore file
├── .prettierrc            # Prettier configuration
└── README.md              # Project documentation`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Haskell Packages</h2>
        <p className="mb-4">
          The <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">packages/</code> directory contains 
          the Haskell libraries that form the core of HaskQ:
        </p>
        
        <CodeBlock 
          language="bash" 
          code={`packages/
├── haskq-core/            # Core quantum DSL
│   ├── src/
│   │   └── HaskQ/
│   │       └── Core/
│   │           ├── Types.hs        # Fundamental types (Qubit, Gate, etc.)
│   │           ├── Gates.hs        # Quantum gate implementations
│   │           ├── Circuit.hs      # Circuit composition utilities
│   │           ├── Measurement.hs  # Measurement operations
│   │           └── Examples.hs     # Example circuits
│   ├── haskq-core.cabal   # Cabal package configuration
│   └── LICENSE            # License file
└── haskq-simulator/       # Quantum simulator
    ├── src/
    │   └── HaskQ/
    │       └── Simulator/
    │           ├── StateVector.hs  # State vector representation
    │           ├── Gates.hs        # Matrix implementations of gates
    │           ├── Circuit.hs      # Circuit simulation engine
    │           └── Visualizer.hs   # Circuit visualization tools
    ├── app/
    │   └── Main.hs        # Command-line simulator application
    ├── haskq-simulator.cabal
    └── LICENSE`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Core Design Principles</h3>
        <p className="mb-4">The Haskell packages follow these design principles:</p>
        
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li><strong>Type Safety</strong>: Linear types enforce quantum mechanics principles at compile time</li>
          <li><strong>Composition</strong>: Monadic design allows for clean circuit composition</li>
          <li><strong>Separation of Concerns</strong>: Clear separation between the DSL and simulation implementation</li>
          <li><strong>Extensibility</strong>: Designed to be extended with new gates and algorithms</li>
        </ol>

        <h2 className="text-2xl font-bold mt-8 mb-4">Web Applications</h2>
        <p className="mb-4">
          The <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">apps/</code> directory contains 
          the web applications that provide user interfaces to HaskQ:
        </p>
        
        <CodeBlock 
          language="bash" 
          code={`apps/
├── landing/              # Landing page
│   ├── app/              # Next.js app directory
│   ├── public/           # Static assets
│   ├── package.json
│   └── tailwind.config.js
├── docs/                 # Documentation site
│   ├── docs/             # Markdown documentation
│   │   ├── intro.md
│   │   ├── getting-started.md
│   │   ├── core-concepts/
│   │   └── tutorials/
│   ├── src/
│   ├── docusaurus.config.js
│   └── package.json
└── playground/           # Interactive playground
    ├── app/              # Next.js app directory
    ├── public/           # Static assets
    ├── package.json
    └── tailwind.config.js`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Build System</h2>
        <p className="mb-4">HaskQ uses a combination of build systems:</p>
        
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li><strong>npm workspaces</strong>: Manages dependencies across the monorepo</li>
          <li><strong>Cabal/Stack</strong>: Builds the Haskell packages</li>
          <li><strong>Next.js/Docusaurus</strong>: Builds the web applications</li>
        </ol>
        
        <p className="mb-4">
          The build process is defined in the root <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">turbo.json</code> file 
          and the scripts in the root <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">package.json</code>.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Development Workflow</h2>
        <p className="mb-4">When developing HaskQ, you'll typically:</p>
        
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li>Make changes to the Haskell packages in <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">packages/</code></li>
          <li>Build the packages using Cabal or Stack</li>
          <li>Run the simulator to test your changes</li>
          <li>Update documentation in <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">apps/docs/docs/</code></li>
          <li>Run the web applications to test the UI</li>
        </ol>

        <InfoBox type="tip" title="Development Tip">
          <p>
            You can use <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">npm run dev:all</code> to start all web applications 
            simultaneously during development, or run them individually with <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">npm run dev:landing</code>, 
            <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">npm run dev:docs</code>, and 
            <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">npm run dev:playground</code>.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Configuration Files</h2>
        <p className="mb-4">Key configuration files in the project:</p>
        
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">turbo.json</code>: Defines the build pipeline for the monorepo</li>
          <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">package.json</code>: Root package configuration with workspace setup</li>
          <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">*.cabal</code>: Haskell package configuration</li>
          <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">docusaurus.config.js</code>: Documentation site configuration</li>
          <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">tailwind.config.js</code>: UI styling configuration for web apps</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Next Steps</h2>
        <p className="mb-4">Now that you understand the project structure, you can:</p>
        
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li>Explore the API Reference to learn about specific modules</li>
          <li>Check out the <Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Tutorials</Link> to see how to use HaskQ</li>
          <li>Start Contributing to the project</li>
        </ol>
      </>
    )
  },
  'tutorials': {
    title: 'HaskQ Tutorials',
    description: 'Step-by-step guides for implementing quantum algorithms and protocols in HaskQ',
    content: () => {
      const TutorialsIndexPage = dynamic(() => import('@/app/docs/tutorials/page'), { ssr: true });
      return <TutorialsIndexPage />;
    }
  },
  'tutorials/qft': {
    title: 'Quantum Fourier Transform (QFT)',
    description: 'Learn about the Quantum Fourier Transform and its implementation in HaskQ',
    content: () => {
      const QFTTutorialPage = dynamic(() => import('@/app/docs/tutorials/qft/page'), { ssr: true });
      return <QFTTutorialPage />;
    }
  },
  'tutorials/bell-states': {
    title: 'Bell States',
    description: 'Understanding quantum entanglement through Bell states',
    content: () => {
      const BellStatesTutorialPage = dynamic(() => import('@/app/docs/tutorials/bell-states/page'), { ssr: true });
      return <BellStatesTutorialPage />;
    }
  },
  'tutorials/grovers-algorithm': {
    title: 'Grover\'s Algorithm',
    description: 'Implementing the quantum search algorithm that provides quadratic speedup',
    content: () => {
      const GroversAlgorithmPage = dynamic(() => import('@/app/docs/tutorials/grovers-algorithm/page'), { ssr: true });
      return <GroversAlgorithmPage />;
    }
  },
  'tutorials/teleportation': {
    title: 'Quantum Teleportation',
    description: 'Learn how to transfer quantum states using entanglement and classical communication',
    content: () => {
      const TeleportationTutorialPage = dynamic(() => import('@/app/docs/tutorials/teleportation/page'), { ssr: true });
      return <TeleportationTutorialPage />;
    }
  },
  'tutorials/superdense-coding': {
    title: 'Superdense Coding',
    description: 'Learn how to transmit two classical bits using a single qubit with entanglement',
    content: () => {
      const SuperdenseCodingPage = dynamic(() => import('@/app/docs/tutorials/superdense-coding/page'), { ssr: true });
      return <SuperdenseCodingPage />;
    }
  },
  'core-concepts/theoretical-connections': {
    title: 'Theoretical Physics Connections',
    description: 'How HaskQ bridges quantum computing with consciousness, modified gravity, and hermetic principles',
    content: () => (
      <>
        <p className="mb-6">HaskQ serves as the <strong>computational backbone</strong> for a revolutionary theoretical physics research ecosystem, providing quantum mechanical implementations of cutting-edge theories developed across consciousness studies, modified gravity research, and hermetic physics.</p>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🧠 AGDEF Theory & Consciousness Research</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Integration with <a href="https://phantasius.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Phantasius</a></h3>
        <p className="mb-4">The <strong>Anti-Gravity Dark Energy Field (AGDEF)</strong> theory proposes that consciousness operates as a fundamental force through higher-dimensional geometric structures. HaskQ provides the first computational framework for modeling these phenomena.</p>
        
        <h4 className="text-lg font-bold mt-4 mb-2">8th Dimensional Consciousness Manifold</h4>
        <p className="mb-4">The AGDEF theory posits that consciousness emerges from an 8th-dimensional manifold that interfaces with our 3D reality through quantum field interactions:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- AGDEF Consciousness Field Implementation
module AGDEF.Consciousness where

-- 8D Consciousness Manifold as quantum state space
type ConsciousnessManifold = [Qubit] -> Circ [Qubit]

-- Model the 8 dimensions of consciousness
eightDimensionalField :: Circ [Qubit]
eightDimensionalField = withQubits 8 $ \\dims -> do
  -- Each dimension represents a different aspect of consciousness
  let [awareness, attention, memory, perception, emotion, intuition, will, identity] = dims
  
  -- Create superposition across all consciousness dimensions
  awareness'   <- hadamard awareness
  attention'   <- hadamard attention
  memory'      <- hadamard memory
  perception'  <- hadamard perception
  emotion'     <- hadamard emotion
  intuition'   <- hadamard intuition
  will'        <- hadamard will
  identity'    <- hadamard identity
  
  -- Consciousness emerges from specific entanglement patterns
  -- Awareness ↔ Attention (conscious focus)
  (awareness'', attention'') <- cnot awareness' attention'
  
  -- Memory ↔ Perception (conscious experience)
  (memory'', perception'') <- cnot memory' perception'
  
  -- Emotion ↔ Intuition (felt sense)
  (emotion'', intuition'') <- cnot emotion' intuition'
  
  -- Will ↔ Identity (self-directed action)
  (will'', identity'') <- cnot will' identity'
  
  -- Higher-order consciousness: meta-awareness
  (awareness''', memory''') <- cnot awareness'' memory''
  (attention''', emotion''') <- cnot attention'' emotion''
  
  pure [awareness''', attention''', memory''', perception'', 
        emotion''', intuition'', will'', identity'']`}
          className="my-6"
        />
        
        <h4 className="text-lg font-bold mt-6 mb-2">Dark Energy Field Interactions</h4>
        <p className="mb-4">AGDEF theory suggests that consciousness can manipulate dark energy through quantum field interactions:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- Dark Energy Field manipulation through consciousness
darkEnergyManipulation :: Double -> [Qubit] -> Circ [Qubit]
darkEnergyManipulation consciousnessLevel qubits = do
  -- Consciousness level affects dark energy field strength
  let fieldStrength = consciousnessLevel * pi / 4
  
  -- Apply consciousness-modulated dark energy field
  mapM (rotateZ fieldStrength) qubits

-- Anti-gravity effect through dark energy field reversal
antiGravityField :: [Qubit] -> Circ [Qubit]
antiGravityField qubits = do
  -- Reverse the phase of dark energy to create repulsive force
  mapM (rotateZ pi) qubits

-- Consciousness-gravity coupling
consciousnessGravityCoupling :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
consciousnessGravityCoupling consciousness gravity = do
  -- Entangle consciousness field with gravitational field
  let pairs = zip consciousness gravity
  results <- mapM (\\(c, g) -> cnot c g) pairs
  let (consciousness', gravity') = unzip results
  pure (consciousness', gravity')`}
          className="my-6"
        />
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🌌 Modified Gravity & Dark Matter Research</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Integration with <a href="https://romulus-rouge.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Romulus</a></h3>
        <p className="mb-4">The Romulus project explores alternatives to dark matter through modified gravity theories. HaskQ provides quantum simulations of these gravitational modifications.</p>
        
        <h4 className="text-lg font-bold mt-4 mb-2">Quantum MOND Implementation</h4>
        <p className="mb-4">Modified Newtonian Dynamics (MOND) suggests gravity behaves differently at low accelerations:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- Quantum MOND field simulation
module Romulus.MOND where

-- MOND transition function
mondTransition :: Double -> Double
mondTransition a = 
  let a0 = 1.2e-10  -- MOND acceleration constant
  in if a < a0 
     then sqrt (a / a0)  -- Deep MOND regime
     else a / a0         -- Newtonian regime

-- Quantum MOND field
quantumMOND :: Double -> Circ Qubit
quantumMOND acceleration = do
  q <- createQubit Zero
  let mondFactor = mondTransition acceleration
      rotationAngle = mondFactor * pi / 2
  
  -- Apply MOND-modified gravitational interaction
  q' <- rotateY rotationAngle q
  
  -- Add quantum uncertainty in gravitational field
  q'' <- hadamard q'
  
  pure q''`}
          className="my-6"
        />
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🔮 Hermetic Quantum Computing</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Integration with <a href="https://arcana-obscura.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Arcana Obscura</a></h3>
        <p className="mb-4">The hermetic tradition provides profound insights into the nature of reality that map remarkably well onto quantum mechanical principles. HaskQ implements these ancient wisdom teachings as quantum algorithms.</p>
        
        <h4 className="text-lg font-bold mt-4 mb-2">The Seven Hermetic Principles</h4>
        <p className="mb-4">Each hermetic principle corresponds to fundamental quantum operations:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- The Seven Hermetic Principles as Quantum Operations
module ArcanaMundo.Hermetic where

-- 1. The Principle of Mentalism: "The All is Mind"
mentalism :: Qubit -> Circ Qubit
mentalism q = do
  -- All reality as mental construct → superposition
  hadamard q

-- 2. The Principle of Correspondence: "As above, so below"
correspondence :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
correspondence (above, below) = do
  -- Perfect correlation between levels
  above' <- hadamard above
  cnot above' below

-- 3. The Principle of Vibration: "Nothing rests; everything moves"
vibration :: Double -> Qubit -> Circ Qubit
vibration frequency q = do
  -- Everything vibrates at different frequencies
  rotateY (frequency * pi) q

-- 4. The Principle of Polarity: "Everything has its pair of opposites"
polarity :: Qubit -> Circ Qubit
polarity q = do
  -- Flip between opposite states
  gateX q`}
          className="my-6"
        />
        
        <h4 className="text-lg font-bold mt-6 mb-2">Hermetic Quantum Circuits</h4>
        <p className="mb-4">Complex hermetic operations combining multiple principles:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- The Emerald Tablet as a quantum circuit
emeraldTablet :: Circ [Qubit]
emeraldTablet = withQubits 7 $ \\principles -> do
  let [mentalism_q, correspondence_q, vibration_q, polarity_q, 
       rhythm_q, causation_q, gender_q] = principles
  
  -- "That which is below is like that which is above"
  mentalism_q' <- mentalism mentalism_q
  (correspondence_q', vibration_q') <- correspondence (correspondence_q, vibration_q)
  
  -- "And that which is above is like that which is below"
  polarity_q' <- polarity polarity_q
  rhythm_q' <- rhythm (pi/3) rhythm_q
  
  -- "To accomplish the miracles of the one thing"
  (causation_q', gender_q') <- causeAndEffect causation_q gender_q
  
  -- Final hermetic synthesis
  synthesized <- hermeticSynthesis [mentalism_q', correspondence_q', vibration_q', 
                                   polarity_q', rhythm_q', causation_q', gender_q']
  
  pure synthesized`}
          className="my-6"
        />
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🌊 Unified Field Theory</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">The Convergence Point</h3>
        <p className="mb-4">All three research streams converge in HaskQ's unified framework:</p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- Unified field incorporating consciousness, gravity, and hermetic principles
unifiedField :: Double -> Double -> Double -> Circ [Qubit]
unifiedField consciousnessLevel gravitationalStrength hermeticPhase = 
  withQubits 12 $ \\field -> do
    let [c1, c2, c3, c4, g1, g2, g3, g4, h1, h2, h3, h4] = field
        consciousness = [c1, c2, c3, c4]
        gravity = [g1, g2, g3, g4]  
        hermetic = [h1, h2, h3, h4]
    
    -- Apply consciousness field (AGDEF)
    consciousness' <- darkEnergyManipulation consciousnessLevel consciousness
    
    -- Apply gravitational field (MOND/Emergent Gravity)
    gravity' <- mapM (quantumMOND gravitationalStrength) gravity
    
    -- Apply hermetic field (Sacred Geometry)
    hermetic' <- mapM (vibration hermeticPhase) hermetic
    
    -- Unify all fields through quantum entanglement
    unified <- unifyFields consciousness' gravity' hermetic'
    
    pure unified`}
          className="my-6"
        />
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🚀 Practical Applications</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Research Validation</h3>
        <p className="mb-4">HaskQ enables:</p>
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li><strong>Consciousness Studies</strong>: Testing IIT theories through quantum information measures</li>
          <li><strong>Gravity Research</strong>: Validating MOND predictions via quantum simulation</li>
          <li><strong>Hermetic Physics</strong>: Exploring ancient wisdom through modern quantum mechanics</li>
        </ol>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Technological Innovation</h3>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Consciousness-Based Computing</strong>: Algorithms inspired by awareness mechanisms</li>
          <li><strong>Gravitational Quantum Computing</strong>: Using gravity modifications for quantum operations</li>
          <li><strong>Sacred Geometry Processors</strong>: Hardware based on hermetic geometric principles</li>
        </ul>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">🌟 Future Directions</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Development Roadmap</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
          <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-4 border border-green-100 dark:border-green-800">
            <h4 className="font-bold text-green-800 dark:text-green-200 mb-2">✅ Phase I: Foundation (Current)</h4>
            <ul className="text-green-700 dark:text-green-300 text-sm space-y-1">
              <li>• Basic quantum implementations</li>
              <li>• Cross-project integration</li>
              <li>• Educational documentation</li>
            </ul>
          </div>
          <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-4 border border-blue-100 dark:border-blue-800">
            <h4 className="font-bold text-blue-800 dark:text-blue-200 mb-2">🔄 Phase II: Integration (Next 6 months)</h4>
            <ul className="text-blue-700 dark:text-blue-300 text-sm space-y-1">
              <li>• Hardware implementation</li>
              <li>• Real-time consciousness measurement</li>
              <li>• Gravitational wave connections</li>
            </ul>
          </div>
          <div className="bg-purple-50 dark:bg-purple-900/20 rounded-lg p-4 border border-purple-100 dark:border-purple-800">
            <h4 className="font-bold text-purple-800 dark:text-purple-200 mb-2">🔮 Phase III: Validation (Next 12 months)</h4>
            <ul className="text-purple-700 dark:text-purple-300 text-sm space-y-1">
              <li>• Consciousness-gravity coupling tests</li>
              <li>• AGDEF field detection</li>
              <li>• Hermetic principle verification</li>
            </ul>
          </div>
          <div className="bg-yellow-50 dark:bg-yellow-900/20 rounded-lg p-4 border border-yellow-100 dark:border-yellow-800">
            <h4 className="font-bold text-yellow-800 dark:text-yellow-200 mb-2">🌈 Phase IV: Application (Future)</h4>
            <ul className="text-yellow-700 dark:text-yellow-300 text-sm space-y-1">
              <li>• Consciousness-enhanced computers</li>
              <li>• Gravitational field manipulation</li>
              <li>• Sacred geometry technologies</li>
            </ul>
          </div>
        </div>
        
        <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800 mt-8">
          <h2 className="text-xl font-semibold mb-4">📚 Cross-References</h2>
          <p className="mb-4">This unified research is explored across our ecosystem:</p>
          <ul className="space-y-2">
            <li>
              <strong><a href="https://phantasius.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Phantasius</a></strong>: Deep dive into AGDEF theory and consciousness dynamics
            </li>
            <li>
              <strong><a href="https://romulus-rouge.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Romulus</a></strong>: Comprehensive modified gravity and dark matter research
            </li>
            <li>
              <strong><a href="https://arcana-obscura.vercel.app/" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">Arcana Obscura</a></strong>: Esoteric wisdom and hermetic principles
            </li>
            <li>
              <strong>HaskQ</strong>: Computational implementation and quantum verification
            </li>
          </ul>
          <p className="mt-4 text-sm italic">
            Together, these projects represent a revolutionary approach to understanding reality through the marriage of ancient wisdom and cutting-edge quantum science.
          </p>
        </div>
      </>
    )
  },
};

// Main page component
export default function DocsPage() {
  const pathname = usePathname();
  const paramsObj = useParams();
  const [content, setContent] = useState<React.ReactNode | null>(null);
  const [title, setTitle] = useState('HaskQ Documentation');
  const [description, setDescription] = useState('');
  
  const loadPage = useCallback(() => {
    // Safely unwrap the params object
    const slug = paramsObj.slug;
    
    // If no slug is provided, show the docs index page
    if (!slug || (Array.isArray(slug) && slug.length === 0)) {
      return setContent(<DocsIndexPage />);
    }

    const slugPath = Array.isArray(slug) ? slug.join('/') : slug;
    
    if (slugPath in pageMapping) {
      const page = pageMapping[slugPath as keyof typeof pageMapping];
      setTitle(page.title);
      setDescription(page.description);
      setContent(page.content());
    } else {
      // Return a placeholder for other pages
      const pageName = Array.isArray(slug) 
        ? slug[slug.length - 1]?.replace(/-/g, ' ') 
        : slug.replace(/-/g, ' ');
      
      setTitle(pageName || 'Documentation');
      setDescription(`Documentation for ${slugPath}`);
      setContent(
        <div>
          <p className="mb-4">This documentation page is under construction.</p>
          <p className="mb-4">Check back soon for content about {pageName}.</p>
        </div>
      );
    }
  }, [paramsObj]);

  useEffect(() => {
    loadPage();
  }, [loadPage, pathname]);

  if (!content) {
    return <div>Loading...</div>;
  }

  return (
    <DocLayout title={title} description={description}>
      {content}
    </DocLayout>
  );
} 