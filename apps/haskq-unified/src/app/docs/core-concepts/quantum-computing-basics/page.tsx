'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function QuantumComputingBasicsPage() {
  return (
    <DocLayout 
      title="Quantum Computing Basics" 
      description="A brief introduction to the fundamental concepts of quantum computing"
    >
      <p className="mb-6">
        This document provides a brief introduction to the fundamental concepts of quantum computing.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Qubits</h2>
      <p className="mb-4">
        Unlike classical bits, which can be either 0 or 1, quantum bits (qubits) can exist in a superposition of both states:
      </p>
      
      <div className="flex justify-center my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <p className="text-center font-mono">|ψ⟩ = α|0⟩ + β|1⟩</p>
        </div>
      </div>
      
      <p className="mb-4">
        where α and β are complex numbers satisfying |α|² + |β|² = 1.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Gates</h2>
      <p className="mb-4">
        In quantum computing, operations are performed using quantum gates, which are unitary operations that act on qubits. 
        Some fundamental gates include:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Hadamard (H)</strong>: Creates a superposition</li>
        <li><strong>Pauli-X</strong>: Quantum equivalent of the NOT gate</li>
        <li><strong>Pauli-Z</strong>: Phase flip gate</li>
        <li><strong>CNOT</strong>: Two-qubit controlled-NOT gate</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement</h2>
      <p className="mb-4">
        When a qubit is measured, it collapses to either 0 or 1 with probabilities determined by |α|² and |β|². 
        This collapse is irreversible.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Entanglement</h2>
      <p className="mb-4">
        Entanglement is a quantum phenomenon where the states of multiple qubits become correlated in such a way 
        that the state of one qubit cannot be described independently of the others.
      </p>

      <InfoBox type="note" title="A Key Quantum Resource">
        <p>
          Entanglement is a central resource in quantum computing, enabling many quantum algorithms to achieve 
          speedups over classical algorithms.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Algorithms</h2>
      <p className="mb-4">
        Quantum algorithms leverage superposition, entanglement, and interference to solve certain problems more 
        efficiently than classical algorithms:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Deutsch-Jozsa</strong>: Determines if a function is constant or balanced</li>
        <li><strong>Grover's Search</strong>: Searches an unsorted database in O(√N) time</li>
        <li><strong>Shor's Algorithm</strong>: Factors integers in polynomial time</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">No-Cloning Theorem</h2>
      <p className="mb-4">
        The no-cloning theorem states that it is impossible to create an identical copy of an unknown quantum state. 
        This theorem has profound implications for quantum computing and is enforced in HaskQ through linear types.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Circuits</h2>
      <p className="mb-4">
        Quantum algorithms are typically expressed as quantum circuits, which are sequences of quantum gates applied to qubits. 
        In HaskQ, circuits are expressed through the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ</code> monad, 
        which provides a clean and type-safe way to compose quantum operations.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      <p className="mb-4">To deepen your understanding of quantum computing, consider exploring:</p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>
          <a 
            href="https://www.cambridge.org/core/books/quantum-computation-and-quantum-information/01E10196D0A682A6AEFFEA52D53BE9AE" 
            target="_blank" 
            rel="noopener noreferrer"
            className="text-indigo-600 dark:text-indigo-400 hover:underline"
          >
            Nielsen and Chuang's "Quantum Computation and Quantum Information"
          </a>
        </li>
        <li>
          <a 
            href="https://quantum.country/" 
            target="_blank" 
            rel="noopener noreferrer"
            className="text-indigo-600 dark:text-indigo-400 hover:underline"
          >
            Quantum Country
          </a>
        </li>
        <li>
          <a 
            href="https://qiskit.org/textbook" 
            target="_blank" 
            rel="noopener noreferrer"
            className="text-indigo-600 dark:text-indigo-400 hover:underline"
          >
            Qiskit Textbook
          </a>
        </li>
      </ul>
    </DocLayout>
  );
} 