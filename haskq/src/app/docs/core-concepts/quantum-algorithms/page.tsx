'use client';

import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Link from 'next/link';

export default function QuantumAlgorithmsPage() {
  return (
    <DocLayout 
      title="Quantum Algorithms" 
      description="Understanding key quantum algorithms and their implementation in HaskQ"
    >
      <p className="mb-6">
        Quantum algorithms are procedures that run on quantum computers to solve computational problems, 
        often with advantages over classical algorithms. This document introduces important quantum algorithms 
        and how they are implemented in HaskQ.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Algorithm Advantages</h2>
      <p className="mb-4">
        Quantum algorithms can offer several types of advantages over classical algorithms:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Polynomial speedup</strong>: Solving problems with fewer steps than classical algorithms</li>
        <li><strong>Exponential speedup</strong>: Solving problems that would take exponential time classically</li>
        <li><strong>Quantum sampling</strong>: Generating samples from distributions that classical computers cannot efficiently produce</li>
        <li><strong>Quantum simulation</strong>: Efficiently simulating quantum systems that classical computers struggle with</li>
      </ul>
      
      <InfoBox type="note" title="Quantum Supremacy">
        <p>
          Quantum advantage (or supremacy) refers to the milestone when a quantum computer can solve a problem 
          that no classical computer can solve in a reasonable amount of time. This has been claimed for specific 
          sampling problems, though the advantage often applies to artificial problems designed for this purpose.
        </p>
      </InfoBox>

      <h2 className="text-2xl font-bold mt-8 mb-4">Key Quantum Algorithms</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Deutsch-Jozsa Algorithm</h3>
      <p className="mb-4">
        The Deutsch-Jozsa algorithm determines whether a function is constant or balanced with a single query, 
        something that would require multiple queries classically.
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <h4 className="font-bold mb-2">Problem:</h4>
        <p>Given a black-box function f(x) that returns either 0 or 1, determine if the function is:</p>
        <ul className="list-disc ml-6 mt-2 mb-2">
          <li><strong>Constant</strong>: Always returns the same value (all 0s or all 1s)</li>
          <li><strong>Balanced</strong>: Returns 0 for half the inputs and 1 for the other half</li>
        </ul>
        <h4 className="font-bold mb-2">Quantum Solution:</h4>
        <p>Uses superposition to evaluate the function on all inputs simultaneously and quantum interference to determine the result.</p>
      </div>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Deutsch-Jozsa Algorithm
deutschJozsa :: (Qubit -> Qubit -> Circ (Qubit, Qubit)) -> Circ Bool
deutschJozsa oracle = do
  -- Create n+1 qubits in |0⟩ state
  x <- createQubit Zero  -- Query register
  y <- createQubit One   -- Answer register (initialized to |1⟩)
  
  -- Apply Hadamard to all qubits
  x <- hadamard x
  y <- hadamard y
  
  -- Apply oracle
  (x, y) <- oracle x y
  
  -- Apply Hadamard to query register
  x <- hadamard x
  
  -- Measure query register
  result <- measure x
  
  -- If result is |0⟩, function is constant; otherwise, it's balanced
  return (result == Zero)`}
        className="my-6"
      />

      <h3 className="text-xl font-bold mt-6 mb-3">Grover's Search Algorithm</h3>
      <p className="mb-4">
        Grover's algorithm provides a quadratic speedup for searching an unstructured database, finding an element 
        in O(√N) time instead of O(N) required classically.
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <h4 className="font-bold mb-2">Problem:</h4>
        <p>Find a specific item x in an unstructured database of N items.</p>
        <h4 className="font-bold mb-2">Quantum Solution:</h4>
        <p>Uses amplitude amplification to increase the probability of measuring the target state:</p>
        <ol className="list-decimal ml-6 mt-2">
          <li>Prepare a uniform superposition of all states</li>
          <li>Apply the Grover iteration O(√N) times:
            <ul className="list-disc ml-6">
              <li>Apply the oracle (marks the target state)</li>
              <li>Apply the diffusion operator (amplifies the marked state)</li>
            </ul>
          </li>
          <li>Measure to obtain the target state with high probability</li>
        </ol>
      </div>

      <CodeBlock 
        language="haskell" 
        code={`-- Grover's Search Algorithm
groverSearch :: Int -> (QState -> QState) -> Circ [Bit]
groverSearch n oracle = do
  -- Create n qubits and apply H to get superposition of all states
  qubits <- replicateM n (createQubit Zero >>= hadamard)
  
  -- Apply Grover iteration O(√N) times
  let iterations = floor (pi/4 * sqrt (2^n))
  qubits <- iterate groverIteration qubits !! iterations
  
  -- Measure all qubits
  mapM measure qubits
  where
    -- Single Grover iteration
    groverIteration qs = do
      -- Apply oracle
      qs' <- oracle qs
      
      -- Apply diffusion operator (reflection about the average)
      qs'' <- diffusion qs'
      
      return qs''
      
    -- Diffusion operator
    diffusion qs = do
      -- Apply H to all qubits
      qs' <- mapM hadamard qs
      
      -- Apply phase flip to all states except |00...0⟩
      qs'' <- applyPhaseFlip qs'
      
      -- Apply H to all qubits
      mapM hadamard qs''`}
        className="my-6"
      />

      <h3 className="text-xl font-bold mt-6 mb-3">Quantum Fourier Transform</h3>
      <p className="mb-4">
        The Quantum Fourier Transform (QFT) is a quantum version of the discrete Fourier transform, 
        a key component in many quantum algorithms including Shor's algorithm. It can be performed 
        in O(log² N) steps, compared to O(N log N) for the classical Fast Fourier Transform.
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Quantum Fourier Transform
qft :: [Qubit] -> Circ [Qubit]
qft [] = return []
qft (q:qs) = do
  -- Apply Hadamard to the first qubit
  q' <- hadamard q
  
  -- Apply controlled rotations
  q'' <- applyControlledRotations q' qs (length qs)
  
  -- Recursively apply QFT to remaining qubits
  qs' <- qft qs
  
  -- Return qubits in reversed order
  return (qs' ++ [q''])
  
  where
    -- Apply controlled rotations for one qubit
    applyControlledRotations :: Qubit -> [Qubit] -> Int -> Circ Qubit
    applyControlledRotations q [] _ = return q
    applyControlledRotations q (control:controls) m = do
      -- Apply controlled rotation
      (control, q) <- controlledPhaseShift control q m
      
      -- Continue with remaining controls
      applyControlledRotations q controls (m-1)`}
        className="my-6"
      />

      <h3 className="text-xl font-bold mt-6 mb-3">Shor's Algorithm</h3>
      <p className="mb-4">
        Shor's algorithm is a quantum algorithm for integer factorization, offering an exponential speedup over 
        the best-known classical algorithms. It's particularly significant because it could break widely used public key 
        cryptography systems like RSA.
      </p>
      
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm my-6">
        <h4 className="font-bold mb-2">Problem:</h4>
        <p>Factor a large integer N into its prime factors.</p>
        <h4 className="font-bold mb-2">Quantum Solution:</h4>
        <p>The algorithm has classical and quantum parts:</p>
        <ol className="list-decimal ml-6 mt-2">
          <li><strong>Classical part</strong>: Reduce factoring to finding the period of a function</li>
          <li><strong>Quantum part</strong>: Find the period using quantum phase estimation, which includes:
            <ul className="list-disc ml-6">
              <li>Creating a superposition</li>
              <li>Computing modular exponentiation</li>
              <li>Applying the quantum Fourier transform</li>
              <li>Measuring and using continued fractions to find the period</li>
            </ul>
          </li>
        </ol>
      </div>

      <InfoBox type="warning" title="Resource Requirements">
        <p>
          While Shor's algorithm is theoretically efficient, factoring large numbers (e.g., 2048-bit RSA keys) 
          would require thousands of error-corrected qubits, which are not yet available. Current implementations 
          are limited to factoring small numbers.
        </p>
      </InfoBox>

      <h3 className="text-xl font-bold mt-6 mb-3">Quantum Phase Estimation</h3>
      <p className="mb-4">
        Quantum Phase Estimation (QPE) estimates the eigenvalues of a unitary operator. It's a key subroutine in 
        many quantum algorithms, including Shor's algorithm and quantum chemistry simulations.
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Quantum Phase Estimation
quantumPhaseEstimation :: Int -> (Qubit -> Circ Qubit) -> Qubit -> Circ ([Bit], Qubit)
quantumPhaseEstimation precision unitary eigenstate = do
  -- Create precision qubits for phase estimation
  phaseQubits <- replicateM precision (createQubit Zero >>= hadamard)
  
  -- Apply controlled-U operations
  controlledOps <- foldM (applyControlledU unitary eigenstate) phaseQubits [0..(precision-1)]
  
  -- Apply inverse QFT to phase qubits
  phaseQubitsTransformed <- inversQFT controlledOps
  
  -- Measure phase qubits
  measureResults <- mapM measure phaseQubitsTransformed
  
  return (measureResults, eigenstate)
  
  where
    applyControlledU u target control k = do
      -- Apply controlled-U^(2^k) operation
      (control, target) <- controlledOp control target (u^(2^k))
      return control`}
        className="my-6"
      />

      <h3 className="text-xl font-bold mt-6 mb-3">Variational Quantum Algorithms</h3>
      <p className="mb-4">
        Variational quantum algorithms are hybrid classical-quantum algorithms that use a quantum computer to 
        evaluate a cost function and a classical computer to optimize variational parameters. Examples include:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Variational Quantum Eigensolver (VQE)</strong>: Finds ground state energies of molecules</li>
        <li><strong>Quantum Approximate Optimization Algorithm (QAOA)</strong>: Solves combinatorial optimization problems</li>
      </ul>
      
      <p className="mb-4">
        These algorithms are particularly promising for near-term quantum computers with limited qubit counts and coherence times.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Algorithm Implementation in HaskQ</h2>
      <p className="mb-4">
        HaskQ provides implementations of these quantum algorithms in the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">HaskQ.Core.Algorithms</code> module:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Example usage of Deutsch-Jozsa algorithm
import HaskQ.Core.Algorithms.DeutschJozsa

-- Define a constant oracle
constantOracle :: Qubit -> Qubit -> Circ (Qubit, Qubit)
constantOracle x y = return (x, y)  -- Always returns 0

-- Run the algorithm
main :: IO ()
main = do
  result <- runCircuit (deutschJozsa constantOracle)
  putStrLn $ "Is the function constant? " ++ show result`}
        className="my-6"
      />
      
      <p className="mb-4">
        HaskQ also provides various utilities to make implementing and testing quantum algorithms easier:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li>Functions for creating and manipulating quantum oracles</li>
        <li>Circuit visualization tools to understand algorithm behavior</li>
        <li>Simulation capabilities to test algorithms on classical hardware</li>
        <li>Performance analysis functions to evaluate algorithm efficiency</li>
      </ul>

      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Algorithm Complexity</h2>
      <p className="mb-4">
        Quantum algorithms are analyzed in terms of their:
      </p>
      
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><strong>Time complexity</strong>: Number of quantum gates required</li>
        <li><strong>Space complexity</strong>: Number of qubits required</li>
        <li><strong>Circuit depth</strong>: Length of the critical path in the circuit</li>
        <li><strong>Query complexity</strong>: Number of oracle queries needed</li>
      </ul>
      
      <p className="mb-4">
        It's important to note that quantum algorithms don't provide universal speedups for all problems. 
        The quantum advantage is specific to certain problem structures and computational tasks.
      </p>

      <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
      <ul className="list-disc ml-6 mb-6 space-y-2">
        <li><Link href="/docs/core-concepts/quantum-circuits" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Circuits</Link></li>
        <li><Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates</Link></li>
        <li><Link href="/docs/tutorials/grovers-algorithm" className="text-indigo-600 dark:text-indigo-400 hover:underline">Grover's Algorithm Tutorial</Link></li>
        <li><Link href="/docs/tutorials/qft-implementation" className="text-indigo-600 dark:text-indigo-400 hover:underline">QFT Implementation</Link></li>
      </ul>
    </DocLayout>
  );
} 