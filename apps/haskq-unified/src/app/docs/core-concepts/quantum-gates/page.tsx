'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function QuantumGatesPage() {
  return (
    <DocLayout 
      title="Quantum Gates" 
      description="Understanding quantum gates and their implementation in HaskQ"
    >
      <p className="mb-6">
        Quantum gates are the fundamental building blocks of quantum circuits. They perform operations on qubits, 
        similar to how classical logic gates operate on classical bits. This document explains the most common 
        quantum gates and how they are implemented in HaskQ.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">What are Quantum Gates?</h2>
      <p className="mb-4">
        Quantum gates are unitary operators that act on qubits. They have several important properties:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>They are <strong>reversible</strong> - for every quantum gate, there exists an inverse gate</li>
        <li>They preserve the <strong>normalization</strong> of quantum states</li>
        <li>They can create <strong>superpositions</strong> and <strong>entanglement</strong></li>
        <li>They are represented mathematically as <strong>unitary matrices</strong></li>
      </ul>

      <InfoBox type="note" title="Unitary Matrices">
        <p>
          A unitary matrix U has the property that its conjugate transpose U† is also its inverse: U†U = UU† = I.
          This property ensures that quantum operations conserve probability.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Single-Qubit Gates</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Pauli Gates</h3>
      <p className="mb-4">
        The Pauli gates are fundamental single-qubit operations:
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4 my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-X (NOT Gate)</h4>
          <p className="font-mono text-center mb-2">X = |0⟩⟨1| + |1⟩⟨0|</p>
          <p>Flips the state of a qubit (bit-flip):</p>
          <p className="font-mono">X|0⟩ = |1⟩ and X|1⟩ = |0⟩</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-Y</h4>
          <p className="font-mono text-center mb-2">Y = -i|0⟩⟨1| + i|1⟩⟨0|</p>
          <p>Rotates the state around Y-axis:</p>
          <p className="font-mono">Y|0⟩ = i|1⟩ and Y|1⟩ = -i|0⟩</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-Z</h4>
          <p className="font-mono text-center mb-2">Z = |0⟩⟨0| - |1⟩⟨1|</p>
          <p>Flips the phase (phase-flip):</p>
          <p className="font-mono">Z|0⟩ = |0⟩ and Z|1⟩ = -|1⟩</p>
        </div>
      </div>

      <h3 className="text-xl font-bold mt-6 mb-3">Hadamard Gate (H)</h3>
      <p className="mb-4">
        The Hadamard gate creates a superposition by putting a qubit into an equal superposition of |0⟩ and |1⟩:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <p className="font-mono text-center mb-2">H = 1/√2 ( |0⟩⟨0| + |0⟩⟨1| + |1⟩⟨0| - |1⟩⟨1| )</p>
        <p className="font-mono">H|0⟩ = (|0⟩ + |1⟩)/√2</p>
        <p className="font-mono">H|1⟩ = (|0⟩ - |1⟩)/√2</p>
      </div>

      <h3 className="text-xl font-bold mt-6 mb-3">Phase Gates</h3>
      <p className="mb-4">
        Phase gates add a phase to the |1⟩ component of a qubit:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>S Gate (Phase)</strong>: S|0⟩ = |0⟩, S|1⟩ = i|1⟩</li>
        <li><strong>T Gate (π/8)</strong>: T|0⟩ = |0⟩, T|1⟩ = e^(iπ/4)|1⟩</li>
        <li><strong>R<sub>φ</sub> (Rotation)</strong>: Arbitrary phase rotation by angle φ</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Multi-Qubit Gates</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">CNOT (Controlled-NOT)</h3>
      <p className="mb-4">
        The CNOT gate is a two-qubit gate that flips the target qubit if the control qubit is |1⟩:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <p className="mb-2">Effects on basis states:</p>
        <p className="font-mono">CNOT|00⟩ = |00⟩</p>
        <p className="font-mono">CNOT|01⟩ = |01⟩</p>
        <p className="font-mono">CNOT|10⟩ = |11⟩</p>
        <p className="font-mono">CNOT|11⟩ = |10⟩</p>
      </div>
      
      <p className="mb-4">
        The CNOT gate is used to create entanglement between qubits.
      </p>

      <h3 className="text-xl font-bold mt-6 mb-3">SWAP Gate</h3>
      <p className="mb-4">
        The SWAP gate exchanges the states of two qubits:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <p className="mb-2">Effects on basis states:</p>
        <p className="font-mono">SWAP|00⟩ = |00⟩</p>
        <p className="font-mono">SWAP|01⟩ = |10⟩</p>
        <p className="font-mono">SWAP|10⟩ = |01⟩</p>
        <p className="font-mono">SWAP|11⟩ = |11⟩</p>
      </div>

      <h3 className="text-xl font-bold mt-6 mb-3">Toffoli (CCNOT) Gate</h3>
      <p className="mb-4">
        The Toffoli gate is a three-qubit gate that flips the target qubit if both control qubits are |1⟩. 
        It is essential for implementing classical logic in quantum circuits.
      </p>

      <h3 className="text-xl font-bold mt-6 mb-3">Fredkin (CSWAP) Gate</h3>
      <p className="mb-4">
        The Fredkin gate swaps two target qubits if the control qubit is |1⟩.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Gate Implementation in HaskQ</h2>
      <p className="mb-4">
        In HaskQ, quantum gates are implemented as functions that transform qubits. Here are examples of how to use various gates:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Single-qubit gates
applySingleQubitGates :: Circ Qubit
applySingleQubitGates = do
  q <- createQubit Zero
  
  -- Apply Hadamard gate
  q <- hadamard q
  
  -- Apply Pauli gates
  q <- pauliX q  -- NOT gate
  q <- pauliY q
  q <- pauliZ q  -- Phase flip
  
  -- Apply phase gates
  q <- phase q   -- S gate
  q <- tGate q   -- T gate
  
  return q

-- Two-qubit gates
applyTwoQubitGates :: Circ (Qubit, Qubit)
applyTwoQubitGates = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply CNOT gate (control: q1, target: q2)
  (q1, q2) <- cnot q1 q2
  
  -- Apply SWAP gate
  (q1, q2) <- swap q1 q2
  
  return (q1, q2)`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Universal Gate Sets</h2>
      <p className="mb-4">
        A set of gates is said to be universal if any unitary operation can be approximated to arbitrary precision 
        using only gates from that set. In HaskQ, the following gates form a universal set:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>Hadamard (H)</li>
        <li>Phase (S)</li>
        <li>T gate (π/8)</li>
        <li>CNOT</li>
      </ul>
      
      <InfoBox type="tip" title="Gate Combinations">
        <p>
          Complex quantum operations can be built by combining simple gates. For example, the SWAP gate can be 
          implemented using three CNOT gates. Similarly, rotation gates can be approximated using H, T, and S gates.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Gate Matrices</h2>
      <p className="mb-4">
        For those interested in the mathematical representation, here are the matrix forms of some common gates:
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-X</h4>
          <p className="font-mono mb-2">X = [0 1]</p>
          <p className="font-mono">    [1 0]</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-Y</h4>
          <p className="font-mono mb-2">Y = [0 -i]</p>
          <p className="font-mono">    [i  0]</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Pauli-Z</h4>
          <p className="font-mono mb-2">Z = [1  0]</p>
          <p className="font-mono">    [0 -1]</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h4 className="font-bold mb-2">Hadamard</h4>
          <p className="font-mono mb-2">H = 1/√2 [1  1]</p>
          <p className="font-mono">        [1 -1]</p>
        </div>
      </div>

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/core-concepts/superposition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Superposition</Link></li>
        <li><Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Entanglement</Link></li>
        <li><Link href="/docs/tutorials/gates-demo" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates Tutorial</Link></li>
      </ul>
    </DocLayout>
  );
} 