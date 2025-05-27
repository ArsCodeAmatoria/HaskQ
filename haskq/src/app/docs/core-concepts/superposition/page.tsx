'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function SuperpositionPage() {
  return (
    <DocLayout 
      title="Quantum Superposition" 
      description="Understanding quantum superposition and its role in quantum computing"
    >
      <p className="mb-6">
        Quantum superposition is a fundamental principle of quantum mechanics that enables quantum computers 
        to process information in ways that classical computers cannot. This document explains the concept 
        and its applications in quantum computing.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">What is Superposition?</h2>
      <p className="mb-4">
        In quantum mechanics, superposition refers to the ability of a quantum system to exist in multiple states simultaneously. 
        While a classical bit can be either 0 or 1, a qubit can exist in a state that is a combination of both 0 and 1 at the same time.
      </p>
      
      <div className="flex justify-center my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <p className="text-center font-mono">|ψ⟩ = α|0⟩ + β|1⟩</p>
        </div>
      </div>
      
      <p className="mb-4">
        In this representation:
      </p>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>|ψ⟩ is the quantum state</li>
        <li>α and β are complex numbers called amplitudes</li>
        <li>|α|² is the probability of measuring 0</li>
        <li>|β|² is the probability of measuring 1</li>
        <li>|α|² + |β|² = 1 (probabilities sum to 1)</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Creating Superposition</h2>
      <p className="mb-4">
        The most common way to create a superposition is by applying a Hadamard gate (H) to a qubit in the |0⟩ state:
      </p>
      
      <div className="flex justify-center my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <p className="text-center font-mono">H|0⟩ = (|0⟩ + |1⟩)/√2</p>
        </div>
      </div>
      
      <p className="mb-4">
        This creates a superposition where the qubit has equal probability (50%) of being measured as either 0 or 1.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Multi-qubit Superposition</h2>
      <p className="mb-4">
        When multiple qubits are in superposition, the system can represent many states simultaneously. 
        With n qubits, a quantum computer can represent 2ⁿ states at once:
      </p>
      
      <CodeBlock 
        language="text" 
        code={`n qubits → 2ⁿ states in superposition

1 qubit  → 2 states:   |0⟩, |1⟩
2 qubits → 4 states:   |00⟩, |01⟩, |10⟩, |11⟩
3 qubits → 8 states:   |000⟩, |001⟩, |010⟩, ... |111⟩
...
50 qubits → 2⁵⁰ states: More states than atoms in Earth!`}
        className="my-6"
      />
      
      <p className="mb-4">
        This exponential scaling is a key source of quantum computing's potential power.
      </p>

      <InfoBox type="note" title="Quantum Parallelism">
        <p>
          Superposition enables quantum parallelism, where a quantum computer can evaluate a function for many 
          inputs simultaneously. However, extracting this information requires careful algorithm design,
          as measurement collapses the superposition to just one result.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Superposition vs. Classical Probability</h2>
      <p className="mb-4">
        It's important to distinguish quantum superposition from classical probability:
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h3 className="font-bold mb-2">Classical Probability</h3>
          <ul className="list-disc ml-4 space-y-1">
            <li>A coin flip has 50% chance of heads or tails</li>
            <li>The coin is definitely in one state, we just don't know which</li>
            <li>No interference between possibilities</li>
          </ul>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h3 className="font-bold mb-2">Quantum Superposition</h3>
          <ul className="list-disc ml-4 space-y-1">
            <li>A qubit can be 50% |0⟩ and 50% |1⟩</li>
            <li>The qubit is genuinely in both states until measured</li>
            <li>States can interfere with each other (constructively or destructively)</li>
          </ul>
        </div>
      </div>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement</h2>
      <p className="mb-4">
        When a qubit in superposition is measured, the superposition collapses to either |0⟩ or |1⟩. 
        The probability of each outcome is determined by the amplitudes of the superposition state.
      </p>
      
      <p className="mb-4">
        After measurement, the qubit remains in the state that was measured, and the superposition is lost.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Superposition in HaskQ</h2>
      <p className="mb-4">
        In HaskQ, you can create and manipulate superpositions using quantum gates. Here's an example:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Create a superposition of |0⟩ and |1⟩
createSuperposition :: Circ Qubit
createSuperposition = do
  q <- createQubit Zero  -- Initialize qubit to |0⟩
  q <- hadamard q        -- Apply Hadamard gate to create superposition
  return q

-- Measure a qubit in superposition
measureSuperposition :: Circ Bit
measureSuperposition = do
  q <- createSuperposition
  measure q  -- 50% chance of 0, 50% chance of 1`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Applications in Quantum Algorithms</h2>
      <p className="mb-4">
        Superposition is a key ingredient in most quantum algorithms:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Deutsch-Jozsa Algorithm</strong>: Uses superposition to determine if a function is constant or balanced with a single query</li>
        <li><strong>Grover's Search</strong>: Uses superposition to search an unsorted database in O(√N) time instead of O(N)</li>
        <li><strong>Quantum Fourier Transform</strong>: Uses superposition to perform a Fourier transform exponentially faster than classical FFT</li>
        <li><strong>Shor's Algorithm</strong>: Uses superposition to factor large numbers efficiently</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Entanglement</Link></li>
        <li><Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link></li>
        <li><Link href="/docs/tutorials/superposition-demo" className="text-indigo-600 dark:text-indigo-400 hover:underline">Superposition Tutorial</Link></li>
      </ul>
    </DocLayout>
  );
} 