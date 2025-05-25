'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { ChevronDown, ChevronRight } from 'lucide-react';
import { useState } from 'react';

// Define the sidebar structure
const sidebarItems = [
  {
    type: 'category',
    label: 'Introduction',
    items: [
      { id: 'intro', label: 'Introduction', href: '/docs/intro' },
      { id: 'getting-started', label: 'Getting Started', href: '/docs/getting-started' },
      { id: 'installation', label: 'Installation', href: '/docs/installation' },
      { id: 'project-structure', label: 'Project Structure', href: '/docs/project-structure' },
    ],
  },
  {
    type: 'category',
    label: 'Core Concepts',
    items: [
      { id: 'quantum-computing-basics', label: 'Quantum Computing Basics', href: '/docs/core-concepts/quantum-computing-basics' },
      { id: 'superposition', label: 'Superposition', href: '/docs/core-concepts/superposition' },
      { id: 'entanglement', label: 'Entanglement', href: '/docs/core-concepts/entanglement' },
      { id: 'quantum-gates', label: 'Quantum Gates', href: '/docs/core-concepts/quantum-gates' },
      { id: 'measurement', label: 'Measurement', href: '/docs/core-concepts/measurement' },
      { id: 'quantum-circuits', label: 'Quantum Circuits', href: '/docs/core-concepts/quantum-circuits' },
      { id: 'quantum-algorithms', label: 'Quantum Algorithms', href: '/docs/core-concepts/quantum-algorithms' },
      { id: 'linear-types', label: 'Linear Types', href: '/docs/core-concepts/linear-types' },
      { id: 'circuit-composition', label: 'Circuit Composition', href: '/docs/core-concepts/circuit-composition' },
      { id: 'simulation', label: 'Simulation', href: '/docs/core-concepts/simulation' },
      { id: 'error-correction', label: 'Error Correction', href: '/docs/core-concepts/error-correction' },
      { id: 'theoretical-connections', label: 'Theoretical Connections', href: '/docs/core-concepts/theoretical-connections' },
    ],
  },
  {
    type: 'category',
    label: 'Tutorials',
    items: [
      { id: 'bell-states', label: 'Bell States', href: '/docs/tutorials/bell-states' },
      { id: 'teleportation', label: 'Quantum Teleportation', href: '/docs/tutorials/teleportation' },
      { id: 'superdense-coding', label: 'Superdense Coding', href: '/docs/tutorials/superdense-coding' },
      { id: 'grovers-algorithm', label: 'Grover\'s Algorithm', href: '/docs/tutorials/grovers-algorithm' },
      { id: 'qft', label: 'Quantum Fourier Transform', href: '/docs/tutorials/qft' },
    ],
  },
];

type CategoryProps = {
  category: {
    type: string;
    label: string;
    items: Array<{
      id: string;
      label: string;
      href: string;
    }>;
  };
};

function CategoryItem({ category }: CategoryProps) {
  const pathname = usePathname();
  const isActive = category.items.some(item => pathname === item.href);
  const [isOpen, setIsOpen] = useState(isActive);

  return (
    <div className="mb-4">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="flex items-center w-full text-left font-medium text-gray-800 dark:text-gray-200 hover:text-indigo-600 dark:hover:text-indigo-400"
      >
        {isOpen ? (
          <ChevronDown className="h-4 w-4 mr-2" />
        ) : (
          <ChevronRight className="h-4 w-4 mr-2" />
        )}
        {category.label}
      </button>
      {isOpen && (
        <ul className="mt-2 ml-4 space-y-2">
          {category.items.map((item) => (
            <li key={item.id}>
              <Link
                href={item.href}
                className={`text-sm ${
                  pathname === item.href
                    ? 'text-indigo-600 dark:text-indigo-400 font-medium'
                    : 'text-gray-600 dark:text-gray-400 hover:text-indigo-600 dark:hover:text-indigo-400'
                }`}
              >
                {item.label}
              </Link>
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

export default function DocsSidebar() {
  return (
    <aside className="w-64 pr-8 hidden md:block">
      <nav className="sticky top-28 mt-4">
        <div className="mb-8">
          <h3 className="text-lg font-bold mb-4 text-gray-900 dark:text-white">Documentation</h3>
        </div>
        <div className="space-y-4">
          {sidebarItems.map((category, index) => (
            <CategoryItem key={index} category={category} />
          ))}
        </div>
      </nav>
    </aside>
  );
} 