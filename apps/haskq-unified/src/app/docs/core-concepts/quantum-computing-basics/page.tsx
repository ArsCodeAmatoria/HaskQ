'use client';

import React from 'react';
import Link from 'next/link';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function QuantumComputingBasicsPage() {
  return (
    <DocLayout 
      title="Quantum Computing Basics" 
      description="A brief introduction to the fundamental concepts of quantum computing"
    >
      <div className="max-w-none prose dark:prose-invert">
        <p className="mb-6">
          Quantum computing is a revolutionary paradigm that leverages quantum mechanical phenomena to perform computations. 
          This document provides an introduction to the key concepts and principles that form the foundation of quantum computing.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Qubits: The Fundamental Unit</h2>
        <p className="mb-4">
          While classical computers use bits (0 or 1) as their basic unit of information, quantum computers use quantum bits or <strong>qubits</strong>.
          Unlike a classical bit, a qubit can exist in a superposition of both 0 and 1 states simultaneously.
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
          <li>|ψ⟩ (psi) represents the quantum state</li>
          <li>α and β are complex amplitudes</li>
          <li>|α|² is the probability of measuring the qubit as 0</li>
          <li>|β|² is the probability of measuring the qubit as 1</li>
          <li>|α|² + |β|² = 1 (probabilities must sum to 1)</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Gates</h2>
        <p className="mb-4">
          Quantum computation is performed by applying quantum gates to qubits. These gates are represented mathematically
          as unitary matrices that transform the state of qubits. Some fundamental quantum gates include:
        </p>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <h3 className="font-bold mb-2">Single-Qubit Gates</h3>
            <ul className="list-disc ml-4 space-y-1">
              <li><strong>Hadamard (H)</strong>: Creates superposition</li>
              <li><strong>Pauli-X</strong>: Quantum NOT gate (bit flip)</li>
              <li><strong>Pauli-Z</strong>: Phase flip</li>
              <li><strong>Pauli-Y</strong>: Combined X and Z rotations</li>
              <li><strong>S, T</strong>: Phase rotations</li>
            </ul>
          </div>
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <h3 className="font-bold mb-2">Multi-Qubit Gates</h3>
            <ul className="list-disc ml-4 space-y-1">
              <li><strong>CNOT</strong>: Controlled-NOT</li>
              <li><strong>CZ</strong>: Controlled-Z</li>
              <li><strong>SWAP</strong>: Exchanges qubit states</li>
              <li><strong>Toffoli</strong>: Controlled-controlled-NOT</li>
              <li><strong>Fredkin</strong>: Controlled-SWAP</li>
            </ul>
          </div>
        </div>

        <p className="mb-4">
          In HaskQ, these gates are implemented as functions that transform qubits:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Example usage of quantum gates in HaskQ
circuit :: Circ (Qubit, Qubit)
circuit = do
  q1 <- createQubit Zero     -- Initialize q1 to |0⟩
  q2 <- createQubit Zero     -- Initialize q2 to |0⟩
  
  q1' <- hadamard q1         -- Apply H gate to q1
  q1'' <- pauliX q1'         -- Apply X gate to q1
  
  -- Apply CNOT gate with q1 as control and q2 as target
  (q1''', q2') <- controlled not q1'' q2
  
  pure (q1''', q2')          -- Return the final state`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Measurement</h2>
        <p className="mb-4">
          Measurement is the process of extracting classical information from a quantum system. When a qubit is measured,
          its quantum state collapses to either |0⟩ or |1⟩, with probabilities determined by the amplitudes α and β.
        </p>
        
        <p className="mb-4">
          In HaskQ, measurement is performed using the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">measure</code> function:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`measureQubit :: Circ Bit
measureQubit = do
  q <- createQubit Zero    -- Initialize qubit to |0⟩
  q' <- hadamard q         -- Put qubit in superposition
  (result, _) <- measure q'  -- Measure the qubit
  pure result              -- Return the measurement result (Zero or One)`}
          className="my-6"
        />

        <InfoBox type="note" title="Measurement Collapse">
          <p>
            Measurement irreversibly collapses the quantum state. After measurement, the qubit is no longer in superposition
            and will remain in the measured state (|0⟩ or |1⟩). This is why quantum algorithms must carefully manage when
            and how measurements are performed.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Entanglement</h2>
        <p className="mb-4">
          Entanglement is a quantum phenomenon where two or more qubits become correlated such that the state of one qubit
          cannot be described independently of the others, regardless of the distance separating them.
        </p>
        
        <p className="mb-4">
          The Bell state is the simplest example of an entangled state:
        </p>

        <div className="flex justify-center my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <p className="text-center font-mono">|Φ⁺⟩ = (|00⟩ + |11⟩)/√2</p>
          </div>
        </div>

        <p className="mb-4">
          In this state, measuring one qubit instantly determines the state of the other qubit, even if they are separated by large distances.
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Creating a Bell state in HaskQ
bellState :: Circ (Qubit, Qubit)
bellState = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- controlled not q1' q2
  pure (q1'', q2')`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Algorithms</h2>
        <p className="mb-4">
          Quantum algorithms leverage the principles of superposition, entanglement, and quantum interference to solve
          certain problems more efficiently than classical algorithms. Some notable quantum algorithms include:
        </p>

        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>
            <strong>Grover's Algorithm</strong>: Provides a quadratic speedup for searching unsorted databases, 
            finding an item in O(√N) steps instead of O(N).
          </li>
          <li>
            <strong>Shor's Algorithm</strong>: Efficiently factors large integers in polynomial time, threatening 
            much of modern cryptography.
          </li>
          <li>
            <strong>Quantum Fourier Transform</strong>: The quantum version of the discrete Fourier transform, 
            which is a key component in many quantum algorithms.
          </li>
          <li>
            <strong>Quantum Phase Estimation</strong>: Estimates the eigenvalues of a unitary operator, used in 
            many quantum algorithms including Shor's.
          </li>
          <li>
            <strong>VQE (Variational Quantum Eigensolver)</strong>: A hybrid quantum-classical algorithm used 
            for finding the ground state energy of molecules.
          </li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">No-Cloning Theorem</h2>
        <p className="mb-4">
          The no-cloning theorem is a fundamental result in quantum mechanics that states it is impossible to create
          an identical copy of an unknown quantum state. This has profound implications for quantum information processing
          and is one of the key differences between quantum and classical information.
        </p>
        
        <p className="mb-4">
          In HaskQ, the no-cloning theorem is enforced at the type level through linear types, ensuring that quantum
          resources are used exactly once.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Circuits</h2>
        <p className="mb-4">
          Quantum circuits are a visual and mathematical representation of quantum computations. They consist of:
        </p>

        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Qubits</strong>: Represented as horizontal lines</li>
          <li><strong>Gates</strong>: Operations applied to qubits, represented as boxes on the lines</li>
          <li><strong>Measurements</strong>: Converting quantum information to classical information</li>
          <li><strong>Time</strong>: Flowing from left to right in the circuit diagram</li>
        </ul>

        <p className="mb-4">
          In HaskQ, circuits are represented using the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ</code> monad, 
          which provides a clean, composable way to express quantum computations:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- A simple quantum teleportation circuit
teleport :: Qubit ⊸ Circ Qubit
teleport qubitToTeleport = do
  -- Create Bell pair
  alice <- createQubit Zero
  bob <- createQubit Zero
  alice' <- hadamard alice
  (alice'', bob') <- controlled not alice' bob
  
  -- Alice entangles her qubit with the Bell pair
  (qubitToTeleport', alice''') <- controlled not qubitToTeleport alice''
  qubitToTeleport'' <- hadamard qubitToTeleport'
  
  -- Alice measures her qubits
  (m1, _) <- measure qubitToTeleport''
  (m2, _) <- measure alice'''
  
  -- Bob applies corrections based on Alice's measurements
  bob'' <- if m2 == One then pauliX bob' else pure bob'
  bob''' <- if m1 == One then pauliZ bob'' else pure bob''
  
  pure bob'''  -- Bob now has the teleported qubit`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li>
            <Link href="/docs/core-concepts/superposition" className="text-indigo-600 dark:text-indigo-400 hover:underline">
              Quantum Superposition
            </Link>
          </li>
          <li>
            <Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">
              Quantum Entanglement
            </Link>
          </li>
          <li>
            <Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">
              Quantum Gates
            </Link>
          </li>
          <li>
            <Link href="/docs/core-concepts/measurement" className="text-indigo-600 dark:text-indigo-400 hover:underline">
              Quantum Measurement
            </Link>
          </li>
          <li>
            <Link href="/docs/core-concepts/quantum-algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">
              Quantum Algorithms
            </Link>
          </li>
          <li>
            <a 
              href="https://quantum.country/qcvc" 
              target="_blank" 
              rel="noopener noreferrer"
              className="text-indigo-600 dark:text-indigo-400 hover:underline"
            >
              Quantum Country: Quantum Computing for the Very Curious
            </a>
          </li>
        </ul>
      </div>
    </DocLayout>
  );
} 