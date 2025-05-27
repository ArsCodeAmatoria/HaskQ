'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function InstallationPage() {
  return (
    <DocLayout 
      title="Installation" 
      description="How to install and set up HaskQ on your system"
    >
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
    </DocLayout>
  );
} 