'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function EntanglementPage() {
  return (
    <DocLayout 
      title="Quantum Entanglement" 
      description="Understanding quantum entanglement and its applications in quantum computing"
    >
      <p className="mb-6">
        Quantum entanglement is one of the most fascinating and counterintuitive phenomena in quantum mechanics. 
        It plays a crucial role in quantum computing and quantum information theory.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">What is Entanglement?</h2>
      <p className="mb-4">
        Entanglement occurs when two or more quantum systems (like qubits) become correlated in such a way that the 
        quantum state of each particle cannot be described independently of the others, regardless of the distance separating them.
      </p>
      
      <p className="mb-4">
        When qubits are entangled, measuring one immediately affects the state of the other, seemingly instantaneously, 
        regardless of the distance between them. Einstein famously referred to this as "spooky action at a distance."
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Bell States</h2>
      <p className="mb-4">
        The simplest examples of entangled states are the Bell states (also called EPR pairs), 
        which are maximally entangled two-qubit states:
      </p>
      
      <div className="flex flex-col space-y-3 my-6 bg-gray-50 dark:bg-gray-800 p-4 rounded-lg">
        <p className="font-mono">|Φ⁺⟩ = (|00⟩ + |11⟩)/√2</p>
        <p className="font-mono">|Φ⁻⟩ = (|00⟩ - |11⟩)/√2</p>
        <p className="font-mono">|Ψ⁺⟩ = (|01⟩ + |10⟩)/√2</p>
        <p className="font-mono">|Ψ⁻⟩ = (|01⟩ - |10⟩)/√2</p>
      </div>

      <h2 className="text-2xl font-bold mt-8 mb-4">Creating Entanglement</h2>
      <p className="mb-4">
        In quantum circuits, entanglement is typically created by applying a Hadamard gate to one qubit, 
        followed by a CNOT gate between two qubits. This is the standard circuit for creating a Bell state:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Creating a Bell state
bellCircuit :: Circ (Qubit, Qubit)
bellCircuit = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1 <- hadamard q1
  (q1, q2) <- cnot q1 q2
  return (q1, q2)`}
        className="my-6"
      />

      <p className="mb-4">This circuit produces the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2.</p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Properties of Entanglement</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">1. Non-locality</h3>
      <p className="mb-4">
        Entangled qubits maintain their correlation regardless of the distance between them. 
        This property has been experimentally verified over distances exceeding 1,200 kilometers.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">2. No-cloning Theorem</h3>
      <p className="mb-4">
        It is impossible to create an identical copy of an unknown quantum state. This theorem is 
        closely related to entanglement and has profound implications for quantum information processing.
      </p>
      
      <InfoBox type="warning" title="Bell's Inequality">
        <p>
          Bell's Inequality provides a mathematical framework for testing whether quantum mechanics 
          is a complete theory or if there are "hidden variables" that explain quantum phenomena. 
          Experimental violations of Bell's Inequality confirm that entanglement is a real phenomenon 
          that cannot be explained by classical physics.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Applications in Quantum Computing</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Quantum Teleportation</h3>
      <p className="mb-4">
        Quantum teleportation uses entanglement to transfer the quantum state of one qubit to another 
        distant qubit without physically moving the qubit itself.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Superdense Coding</h3>
      <p className="mb-4">
        With superdense coding, two classical bits of information can be transmitted by sending just 
        one qubit, provided the sender and receiver share an entangled pair.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Quantum Algorithms</h3>
      <p className="mb-4">
        Many quantum algorithms, including Shor's factoring algorithm and Grover's search algorithm, 
        leverage entanglement to achieve computational advantages over classical algorithms.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Entanglement in HaskQ</h2>
      <p className="mb-4">
        In HaskQ, entanglement is created through the application of multi-qubit gates such as CNOT. 
        Here's an example of creating and measuring an entangled state:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Create and measure a Bell state
measureBellState :: Circ (Bit, Bit)
measureBellState = do
  -- Create Bell state
  (q1, q2) <- bellCircuit
  
  -- Measure both qubits
  b1 <- measure q1
  b2 <- measure q2
  
  return (b1, b2)`}
        className="my-6"
      />
      
      <p className="mb-4">
        When you run this circuit and simulate it many times, you'll notice that the measurement 
        outcomes are always correlated: either both qubits measure to 0, or both measure to 1, 
        each with 50% probability.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell States Tutorial</Link></li>
        <li><Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link></li>
        <li><Link href="/docs/core-concepts/superposition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Superposition</Link></li>
      </ul>
    </DocLayout>
  );
} 