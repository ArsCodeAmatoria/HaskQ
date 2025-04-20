'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function MeasurementPage() {
  return (
    <DocLayout 
      title="Quantum Measurement" 
      description="Understanding quantum measurement and its implementation in HaskQ"
    >
      <p className="mb-6">
        Measurement is a crucial operation in quantum computing that allows us to extract classical information 
        from quantum states. This document explains the principles of quantum measurement and how it is 
        implemented in HaskQ.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">What is Quantum Measurement?</h2>
      <p className="mb-4">
        Quantum measurement is the process of observing a quantum system, which causes its superposition state 
        to collapse to one of its basis states. This process has several key characteristics:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Probabilistic</strong>: The outcome is inherently random, governed by probability amplitudes</li>
        <li><strong>Irreversible</strong>: Measurement collapses the quantum state and cannot be undone</li>
        <li><strong>Basis-dependent</strong>: The choice of measurement basis affects possible outcomes</li>
        <li><strong>State-changing</strong>: The quantum state changes upon measurement</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement in the Computational Basis</h2>
      <p className="mb-4">
        The most common type of measurement is in the computational basis (|0⟩ and |1⟩). For a qubit in state:
      </p>
      
      <div className="flex justify-center my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <p className="text-center font-mono">|ψ⟩ = α|0⟩ + β|1⟩</p>
        </div>
      </div>
      
      <p className="mb-4">
        A measurement will result in:
      </p>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>Outcome "0" with probability |α|² - the state collapses to |0⟩</li>
        <li>Outcome "1" with probability |β|² - the state collapses to |1⟩</li>
      </ul>

      <InfoBox type="warning" title="Born Rule">
        <p>
          The Born rule, formulated by Max Born, states that the probability of measuring a particular outcome 
          is equal to the squared magnitude of the corresponding amplitude. This fundamental rule connects the 
          mathematical formalism of quantum mechanics to experimental observations.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement in Different Bases</h2>
      <p className="mb-4">
        Qubits can be measured in different bases, not just the standard computational basis:
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6">
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h3 className="font-bold mb-2">Z-basis (Computational)</h3>
          <p className="font-mono">|0⟩ and |1⟩</p>
          <p>Standard measurement determining if qubit is in state 0 or 1</p>
        </div>
        <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
          <h3 className="font-bold mb-2">X-basis (Hadamard)</h3>
          <p className="font-mono">|+⟩ = (|0⟩+|1⟩)/√2 and |-⟩ = (|0⟩-|1⟩)/√2</p>
          <p>Measures along the X-axis of the Bloch sphere</p>
        </div>
      </div>
      
      <p className="mb-4">
        To measure in a different basis, we typically rotate the state before measuring in the computational basis. 
        For example, to measure in the X-basis, we apply a Hadamard gate before measuring.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement and Entanglement</h2>
      <p className="mb-4">
        When measuring one qubit of an entangled pair, the measurement outcome affects the state of the other qubit instantaneously:
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <p className="mb-2">For the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2:</p>
        <p className="font-mono">- If the first qubit is measured as |0⟩, the second qubit will also be in state |0⟩</p>
        <p className="font-mono">- If the first qubit is measured as |1⟩, the second qubit will also be in state |1⟩</p>
      </div>
      
      <p className="mb-4">
        This correlation persists regardless of the distance between the qubits, a phenomenon Einstein famously called "spooky action at a distance."
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum State Tomography</h2>
      <p className="mb-4">
        Since a single measurement collapses a quantum state, how can we determine what state a qubit was in? 
        Quantum state tomography is a technique that involves preparing many identical copies of a quantum state 
        and performing measurements in different bases to reconstruct the original state.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Measurement in HaskQ</h2>
      <p className="mb-4">
        In HaskQ, measurement is performed using the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">measure</code> function, 
        which takes a qubit and returns a classical bit:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Basic measurement
basicMeasurement :: Circ Bit
basicMeasurement = do
  q <- createQubit Zero  -- Initialize qubit to |0⟩
  q <- hadamard q        -- Apply Hadamard to create superposition
  b <- measure q         -- Measure the qubit (50% chance of 0, 50% chance of 1)
  return b

-- Measuring in the X-basis
xBasisMeasurement :: Circ Bit
xBasisMeasurement = do
  q <- createQubit Zero
  q <- pauliX q          -- Prepare |1⟩ state
  q <- hadamard q        -- Apply Hadamard to get |-⟩ state
  
  -- To measure in X-basis, apply Hadamard before measurement
  q <- hadamard q
  b <- measure q
  return b

-- Measuring an entangled pair
measureEntangledPair :: Circ (Bit, Bit)
measureEntangledPair = do
  -- Create Bell state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1 <- hadamard q1
  (q1, q2) <- cnot q1 q2
  
  -- Measure both qubits
  b1 <- measure q1
  b2 <- measure q2
  
  return (b1, b2)  -- Results will be correlated`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Non-demolition Measurements</h2>
      <p className="mb-4">
        In some quantum systems, it's possible to perform quantum non-demolition (QND) measurements that 
        preserve certain aspects of the quantum state. These are particularly important in quantum error 
        correction, where we need to detect errors without disturbing the encoded quantum information.
      </p>
      
      <p className="mb-4">
        HaskQ supports simulating these types of measurements in quantum error correction codes.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Deferred Measurement Principle</h2>
      <p className="mb-4">
        The deferred measurement principle states that measurements can be postponed until the end of a 
        quantum computation without affecting the result. This principle is used in many quantum algorithms 
        to simplify circuit design.
      </p>
      
      <InfoBox type="tip" title="Optimizing Circuits">
        <p>
          Using the deferred measurement principle, you can often simplify quantum circuits by replacing 
          mid-circuit measurements and classical controls with controlled quantum operations.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Statistics and Sampling</h2>
      <p className="mb-4">
        Since quantum measurements are probabilistic, it's often necessary to run a quantum circuit multiple 
        times and gather statistics. In HaskQ, you can run simulations multiple times to sample from the 
        probability distribution:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Run a circuit multiple times and count the outcomes
sampleCircuit :: Int -> Circ a -> IO (Map a Int)
sampleCircuit shots circuit = do
  results <- replicateM shots (runCircuit circuit)
  return $ fromListWith (+) [(result, 1) | result <- results]

-- Example usage
main :: IO ()
main = do
  counts <- sampleCircuit 1000 basicMeasurement
  putStrLn $ "Measurement statistics: " ++ show counts`}
        className="my-6"
      />

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/core-concepts/superposition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Superposition</Link></li>
        <li><Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Entanglement</Link></li>
        <li><Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates</Link></li>
        <li><Link href="/docs/tutorials/measurement-demo" className="text-indigo-600 dark:text-indigo-400 hover:underline">Measurement Tutorial</Link></li>
      </ul>
    </DocLayout>
  );
} 