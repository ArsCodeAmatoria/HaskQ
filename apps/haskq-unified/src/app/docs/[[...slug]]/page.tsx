'use client';

import { useCallback, useEffect, useState } from 'react';
import { notFound, usePathname, useParams } from 'next/navigation';
import Link from 'next/link';
import { Book, Code, ArrowRight } from 'lucide-react';
import React from 'react';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

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
  // Add more pages as needed following the same pattern
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