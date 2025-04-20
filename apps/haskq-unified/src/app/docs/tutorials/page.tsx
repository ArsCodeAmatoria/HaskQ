'use client';

import React from 'react';
import Link from 'next/link';
import { BookOpen, Zap, Layers, Cpu, Workflow, GitBranch } from 'lucide-react';

export default function TutorialsIndexPage() {
  const tutorialCategories = [
    {
      title: 'Fundamental Quantum Protocols',
      description: 'Tutorials on basic quantum information protocols',
      tutorials: [
        {
          title: 'Bell States',
          description: 'Create and work with maximally entangled qubit pairs',
          href: '/docs/tutorials/bell-states',
          icon: <Zap className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Quantum Teleportation',
          description: 'Transfer quantum states using entanglement and classical communication',
          href: '/docs/tutorials/teleportation',
          icon: <Workflow className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Superdense Coding',
          description: 'Transmit two classical bits using a single qubit',
          href: '/docs/tutorials/superdense-coding',
          icon: <Cpu className="h-5 w-5 text-indigo-500" />,
        },
      ],
    },
    {
      title: 'Quantum Algorithms',
      description: 'Implement powerful quantum algorithms',
      tutorials: [
        {
          title: 'Quantum Fourier Transform (QFT)',
          description: 'A key subroutine in many quantum algorithms',
          href: '/docs/tutorials/qft',
          icon: <GitBranch className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Grover\'s Algorithm',
          description: 'Quadratic speedup for unstructured search problems',
          href: '/docs/tutorials/grovers-algorithm',
          icon: <Layers className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Shor\'s Algorithm',
          description: 'Factorize large integers exponentially faster than classical algorithms',
          href: '/docs/tutorials/shor-algorithm',
          icon: <BookOpen className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Quantum Phase Estimation',
          description: 'Estimate the eigenvalues of a unitary operator',
          href: '/docs/tutorials/phase-estimation',
          icon: <Cpu className="h-5 w-5 text-indigo-500" />,
        },
      ],
    },
    {
      title: 'Practical Applications',
      description: 'Apply quantum computing to real-world problems',
      tutorials: [
        {
          title: 'Quantum Chemistry Simulation',
          description: 'Simulate molecular energy levels using quantum computers',
          href: '/docs/tutorials/quantum-chemistry',
          icon: <Workflow className="h-5 w-5 text-indigo-500" />,
        },
        {
          title: 'Quantum Machine Learning',
          description: 'Implement quantum versions of machine learning algorithms',
          href: '/docs/tutorials/quantum-ml',
          icon: <GitBranch className="h-5 w-5 text-indigo-500" />,
        },
      ],
    },
  ];

  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">HaskQ Tutorials</h1>
      
      <p className="text-lg mb-8">
        Welcome to the HaskQ tutorials section. These step-by-step guides will help you implement quantum 
        circuits and algorithms using HaskQ. Start with the fundamental tutorials if you're new to 
        quantum computing, or dive into the advanced algorithms if you're ready for a challenge.
      </p>
      
      {tutorialCategories.map((category, index) => (
        <div key={index} className="mb-12">
          <h2 className="text-2xl font-bold mb-4">{category.title}</h2>
          <p className="mb-6">{category.description}</p>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {category.tutorials.map((tutorial, tutorialIndex) => (
              <Link 
                key={tutorialIndex} 
                href={tutorial.href}
                className="flex items-start p-4 rounded-lg border border-gray-200 dark:border-gray-700 hover:bg-gray-50 dark:hover:bg-gray-800 transition-colors"
              >
                <div className="mr-4 mt-1">{tutorial.icon}</div>
                <div>
                  <h3 className="text-lg font-semibold text-indigo-600 dark:text-indigo-400 mb-1">{tutorial.title}</h3>
                  <p className="text-gray-600 dark:text-gray-300">{tutorial.description}</p>
                </div>
              </Link>
            ))}
          </div>
        </div>
      ))}
      
      <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800 mt-8">
        <h2 className="text-xl font-semibold mb-4">Need Help with a Tutorial?</h2>
        <p className="mb-4">
          If you have questions or run into issues while following these tutorials, try:
        </p>
        <ul className="list-disc list-inside space-y-2 text-gray-700 dark:text-gray-300">
          <li>Checking the <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Core Concepts</Link> documentation for a refresher on fundamentals</li>
          <li>Looking at the <Link href="/docs/installation" className="text-indigo-600 dark:text-indigo-400 hover:underline">Installation Guide</Link> if you're having setup issues</li>
          <li>Visiting our <a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">GitHub repository</a> to file an issue</li>
        </ul>
      </div>
    </div>
  );
} 