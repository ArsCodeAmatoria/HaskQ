'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function QuantumCircuitsPage() {
  return (
    <DocLayout 
      title="Quantum Circuits" 
      description="Understanding quantum circuits and their implementation in HaskQ"
    >
      <p className="mb-6">
        Quantum circuits are the standard way to represent quantum algorithms. They provide a visual 
        and mathematical framework for describing sequences of quantum operations. This document explains 
        quantum circuits and how they are implemented in HaskQ.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">What is a Quantum Circuit?</h2>
      <p className="mb-4">
        A quantum circuit is a computational model that describes a sequence of quantum operations (gates) 
        applied to a set of qubits. Key characteristics of quantum circuits include:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>They consist of qubits (quantum registers) and quantum gates</li>
        <li>Operations are applied from left to right (or top to bottom in visual representations)</li>
        <li>They include initialization, quantum gates, and measurement operations</li>
        <li>They may include classical control based on measurement outcomes</li>
        <li>They are reversible until measurement occurs</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Components</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Qubits</h3>
      <p className="mb-4">
        Qubits are the fundamental units of quantum information. In circuit diagrams, qubits are represented 
        as horizontal lines. Each qubit is typically initialized to the |0⟩ state at the beginning of a circuit.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Gates</h3>
      <p className="mb-4">
        Quantum gates are operations that transform qubit states. They are represented as boxes or symbols 
        placed on qubit lines. Common gates include:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>Single-qubit gates (H, X, Y, Z, S, T)</li>
        <li>Two-qubit gates (CNOT, SWAP)</li>
        <li>Multi-qubit gates (Toffoli, Fredkin)</li>
      </ul>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Measurements</h3>
      <p className="mb-4">
        Measurements convert quantum information to classical information. They are usually represented at the 
        end of a circuit, though mid-circuit measurements are also possible.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Representation</h2>
      <p className="mb-4">
        Quantum circuits can be represented in several ways:
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Visual Diagram</h3>
      <p className="mb-4">
        The standard visual representation shows qubits as horizontal lines and gates as symbols:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <pre className="font-mono text-sm">
{`q0: ─────────────[H]─────────[●]───────────[M]───
                            │
q1: ─────────────────────[X]───────────[M]───`}
        </pre>
        <p className="mt-2 text-sm">A Bell circuit: Hadamard on q0, CNOT with q0 as control and q1 as target, then measure both qubits</p>
      </div>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Mathematical Representation</h3>
      <p className="mb-4">
        Circuits can also be represented as a sequence of matrix operations applied to a state vector:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <p className="font-mono">|ψ_final⟩ = U_n ... U_2 · U_1 · |ψ_initial⟩</p>
        <p className="mt-2">Where each U_i is a unitary matrix representing a quantum gate</p>
      </div>

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Depth and Width</h2>
      <p className="mb-4">
        Two important metrics for quantum circuits are:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Circuit width</strong>: The number of qubits used in the circuit</li>
        <li><strong>Circuit depth</strong>: The maximum number of sequential gate operations that must be performed</li>
      </ul>
      
      <p className="mb-4">
        These metrics are important for assessing the computational resources required by a quantum algorithm.
      </p>
      
      <InfoBox type="note" title="NISQ Era Constraints">
        <p>
          In the current Noisy Intermediate-Scale Quantum (NISQ) era, quantum computers have limited coherence times. 
          This makes circuit depth a critical constraint, as deeper circuits are more susceptible to errors.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Circuits in HaskQ</h2>
      <p className="mb-4">
        In HaskQ, quantum circuits are represented using the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ</code> monad, 
        which provides a clean and type-safe way to compose quantum operations:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- A simple Bell circuit
bellCircuit :: Circ (Bit, Bit)
bellCircuit = do
  -- Initialize qubits
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply Hadamard to first qubit
  q1 <- hadamard q1
  
  -- Apply CNOT with q1 as control and q2 as target
  (q1, q2) <- cnot q1 q2
  
  -- Measure both qubits
  b1 <- measure q1
  b2 <- measure q2
  
  return (b1, b2)`}
        className="my-6"
      />
      
      <p className="mb-4">
        The monadic style allows for natural sequencing of operations and clear handling of qubit transformations.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Composition</h2>
      <p className="mb-4">
        One of the key advantages of the circuit model is the ability to compose circuits. 
        HaskQ makes this easy through function composition and the monadic interface:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- A circuit component that creates a superposition
createSuperposition :: Qubit -> Circ Qubit
createSuperposition q = hadamard q

-- A circuit component that entangles two qubits
entanglePair :: Qubit -> Qubit -> Circ (Qubit, Qubit)
entanglePair q1 q2 = cnot q1 q2

-- Composing the components into a larger circuit
compositeBellCircuit :: Circ (Bit, Bit)
compositeBellCircuit = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Use the component circuits
  q1 <- createSuperposition q1
  (q1, q2) <- entanglePair q1 q2
  
  b1 <- measure q1
  b2 <- measure q2
  
  return (b1, b2)`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Conditional Operations</h2>
      <p className="mb-4">
        Quantum circuits can include classical control, where quantum operations are conditionally applied 
        based on measurement results:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Quantum teleportation circuit
teleportCircuit :: Qubit -> Circ Qubit
teleportCircuit state = do
  -- Create entangled pair
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1 <- hadamard q1
  (q1, q2) <- cnot q1 q2
  
  -- Entangle with state to teleport
  (state, q1) <- cnot state q1
  state <- hadamard state
  
  -- Measure control qubits
  b1 <- measure state
  b2 <- measure q1
  
  -- Apply conditional corrections
  q2 <- if b1 == One then pauliX q2 else return q2
  q2 <- if b2 == One then pauliZ q2 else return q2
  
  return q2  -- Teleported state`}
        className="my-6"
      />
      
      <p className="mb-4">
        This example shows how measurement results can be used to control subsequent quantum operations.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Visualization</h2>
      <p className="mb-4">
        HaskQ provides tools to visualize quantum circuits, which is helpful for understanding and debugging:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Visualize a Bell circuit
main :: IO ()
main = do
  let circuit = bellCircuit
  drawCircuit circuit   -- Renders an ASCII or Unicode circuit diagram
  printCircuit circuit  -- Prints a textual representation of the circuit`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Optimization</h2>
      <p className="mb-4">
        Quantum circuits often benefit from optimization to reduce depth and gate count. Common techniques include:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>Gate cancellation (e.g., H·H = I)</li>
        <li>Gate commutation (reordering gates that operate on different qubits)</li>
        <li>Gate decomposition (breaking complex gates into simpler ones)</li>
        <li>Circuit identities (using known equivalent circuits)</li>
      </ul>
      
      <InfoBox type="tip" title="Circuit Optimization">
        <p>
          HaskQ provides optimization functions that can automatically simplify circuits by applying these techniques.
          Using optimized circuits can significantly improve simulation speed and reduce errors in real quantum hardware.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Example Circuits</h2>
      <p className="mb-4">
        HaskQ includes several example circuits that demonstrate key quantum algorithms:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Deutsch-Jozsa Algorithm</strong>: Determines if a function is constant or balanced</li>
        <li><strong>Grover's Search</strong>: Finds an element in an unsorted database</li>
        <li><strong>Quantum Fourier Transform</strong>: The quantum version of the discrete Fourier transform</li>
        <li><strong>Quantum Phase Estimation</strong>: Estimates the eigenvalues of a unitary operator</li>
      </ul>
      
      <p className="mb-4">
        These can be found in the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">HaskQ.Core.Algorithms</code> module.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates</Link></li>
        <li><Link href="/docs/core-concepts/measurement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Measurement</Link></li>
        <li><Link href="/docs/tutorials/bell-circuit" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell Circuit Tutorial</Link></li>
        <li><Link href="/docs/tutorials/quantum-teleportation" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Teleportation Tutorial</Link></li>
      </ul>
    </DocLayout>
  );
} 