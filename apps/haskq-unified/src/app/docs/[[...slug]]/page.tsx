'use client';

import { useCallback, useEffect, useState } from 'react';
import { notFound, usePathname } from 'next/navigation';
import Link from 'next/link';
import { Book, Code, ArrowRight } from 'lucide-react';

// Simplified docs index function component
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
      <h1 className="text-3xl font-bold mb-6">HaskQ Documentation</h1>
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

// Static mapping for documentation pages - easier than processing MDX
const pageMapping = {
  'intro': {
    title: 'Introduction to HaskQ',
    description: 'A functional quantum programming language',
    content: () => (
      <>
        <p>HaskQ is a quantum programming language that combines the elegance of Haskell with the power of quantum computing. It provides a type-safe, purely functional approach to quantum circuit design and simulation.</p>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Why HaskQ?</h2>
        <p>Quantum computing is a fundamentally different paradigm from classical computing, and traditional programming languages weren't designed with quantum mechanics in mind. HaskQ addresses this gap by:</p>
        <ul className="list-disc ml-6 my-4 space-y-2">
          <li><strong>Enforcing quantum mechanics laws at compile time</strong> through linear types</li>
          <li><strong>Providing clean, functional abstractions</strong> for quantum circuit design</li>
          <li><strong>Including a built-in simulator</strong> for testing and debugging quantum algorithms</li>
          <li><strong>Enabling seamless integration</strong> with classical Haskell code</li>
        </ul>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Key Features</h2>
        <h3 className="text-xl font-bold mt-6 mb-3">Type-Safe Quantum Programming</h3>
        <p>HaskQ uses Haskell's linear types to enforce the no-cloning theorem at compile time. This means the compiler prevents you from writing programs that violate the laws of quantum mechanics.</p>
        
        <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md my-4">
          <pre className="text-sm">
            <code>{`-- This will compile
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
  pure (q'', q''')`}</code>
          </pre>
        </div>
      </>
    )
  },
  'core-concepts/simulation': {
    title: 'Quantum Simulation',
    description: 'Understand how the HaskQ simulator works and how to use it effectively',
    content: () => (
      <>
        <p>HaskQ includes a powerful quantum circuit simulator that allows you to test and analyze quantum algorithms without a physical quantum computer. This document explains how simulation works in HaskQ and how to use it effectively.</p>
        
        <h2 className="text-2xl font-bold mt-8 mb-4">Getting Started with HaskQ Simulator</h2>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Installation</h3>
        <p>To use the HaskQ simulator, first ensure you have the HaskQ package installed:</p>
        <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md my-4">
          <pre className="text-sm">
            <code>{`# Install HaskQ from source
git clone https://github.com/haskq/haskq.git
cd haskq
cabal install

# Or using stack
stack install`}</code>
          </pre>
        </div>
        
        <p>Once installed, you'll have access to the command-line simulator <code>haskq-sim</code> and the simulator library for use in your Haskell programs.</p>
        
        <h3 className="text-xl font-bold mt-6 mb-3">Quick Start Example</h3>
        <p>Create a file named <code>BellState.hs</code> with the following content:</p>
        <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md my-4">
          <pre className="text-sm">
            <code>{`import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)

main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn $ "Measurement results: " ++ show (measurements result)`}</code>
          </pre>
        </div>
        
        <p>Compile and run:</p>
        <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md my-4">
          <pre className="text-sm">
            <code>{`ghc -o bell-state BellState.hs
./bell-state`}</code>
          </pre>
        </div>
        
        <p>You should see either <code>[Zero, Zero]</code> or <code>[One, One]</code> as the output, demonstrating quantum entanglement.</p>
      </>
    )
  },
  // Add more pages as needed following the same pattern
};

// Main page component
export default function DocsPage({ params }: { params: { slug?: string[] } }) {
  const pathname = usePathname();
  const [content, setContent] = useState<React.ReactNode | null>(null);
  const [title, setTitle] = useState('Documentation');
  const [description, setDescription] = useState('');
  
  const loadPage = useCallback(async () => {
    // If no slug is provided, show the docs index page
    if (!params.slug || params.slug.length === 0) {
      return setContent(<DocsIndexPage />);
    }

    const slug = params.slug.join('/');
    
    if (slug in pageMapping) {
      const page = pageMapping[slug as keyof typeof pageMapping];
      setTitle(page.title);
      setDescription(page.description);
      setContent(page.content());
    } else {
      // Return a placeholder for other pages
      setTitle(slug.split('/').pop()?.replace(/-/g, ' ') || 'Documentation');
      setDescription(`Documentation for ${slug}`);
      setContent(
        <div>
          <p>This documentation page is under construction.</p>
          <p>Check back soon for content about {slug.split('/').pop()?.replace(/-/g, ' ')}.</p>
        </div>
      );
    }
  }, [params.slug]);

  useEffect(() => {
    loadPage();
  }, [loadPage, pathname]);

  if (!content) {
    return <div>Loading...</div>;
  }

  return (
    <article className="prose dark:prose-invert max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">{title}</h1>
      {description && (
        <p className="text-lg text-gray-600 dark:text-gray-300 mb-8">
          {description}
        </p>
      )}
      
      <div className="markdown-content">
        {content}
      </div>
    </article>
  );
} 