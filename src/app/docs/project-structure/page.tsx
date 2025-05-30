'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function ProjectStructurePage() {
  return (
    <DocLayout 
      title="Project Structure" 
      description="Understanding the organization of the HaskQ codebase"
    >
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
├── haskq-unified/         # Unified web application
│   ├── src/
│   │   ├── app/           # Next.js app directory
│   │   │   ├── api/       # API routes
│   │   │   ├── docs/      # Documentation pages
│   │   │   └── playground/ # Interactive playground
│   │   ├── components/    # Shared React components
│   │   ├── lib/           # Utility functions
│   │   └── content/       # Documentation content in MDX
│   ├── public/            # Static assets
│   ├── package.json
│   └── tailwind.config.js`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Build System</h2>
      <p className="mb-4">HaskQ uses a combination of build systems:</p>
      
      <ol className="list-decimal ml-6 mb-6 space-y-2">
        <li><strong>npm workspaces</strong>: Manages dependencies across the monorepo</li>
        <li><strong>Cabal/Stack</strong>: Builds the Haskell packages</li>
        <li><strong>Next.js</strong>: Builds the web application</li>
      </ol>
      
      <p className="mb-4">
        The build process is defined in the scripts in the root <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">package.json</code>.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Development Workflow</h2>
      <p className="mb-4">When developing HaskQ, you'll typically:</p>
      
      <ol className="list-decimal ml-6 mb-6 space-y-2">
        <li>Make changes to the Haskell packages in <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">packages/</code></li>
        <li>Build the packages using Cabal or Stack</li>
        <li>Run the simulator to test your changes</li>
        <li>Update documentation in <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">apps/haskq-unified/src/app/docs/</code></li>
        <li>Run the web application to test the UI</li>
      </ol>

      <InfoBox type="tip" title="Development Tip">
        <p>
          You can start the web application with <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">npm run dev</code> from the project root,
          which will serve both the documentation and the playground in one application.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Configuration Files</h2>
      <p className="mb-4">Key configuration files in the project:</p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">package.json</code>: Root package configuration with workspace setup</li>
        <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">*.cabal</code>: Haskell package configuration</li>
        <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">next.config.ts</code>: Next.js configuration</li>
        <li><code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">tailwind.config.js</code>: UI styling configuration</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Next Steps</h2>
      <p className="mb-4">Now that you understand the project structure, you can:</p>
      
      <ol className="list-decimal ml-6 mb-6 space-y-2">
        <li>Explore the API Reference to learn about specific modules</li>
        <li>Check out the <Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Tutorials</Link> to see how to use HaskQ</li>
        <li>Start Contributing to the project</li>
      </ol>
    </DocLayout>
  );
} 