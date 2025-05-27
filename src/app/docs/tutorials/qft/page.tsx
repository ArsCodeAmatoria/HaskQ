'use client';

import React from 'react';
import Link from 'next/link';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';
import Image from 'next/image';

export default function QFTTutorialPage() {
  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">Quantum Fourier Transform (QFT) Tutorial</h1>
      
      <p className="text-lg mb-6">
        The Quantum Fourier Transform (QFT) is a fundamental quantum algorithm that serves as the building block for many important quantum algorithms, including Shor's factoring algorithm and quantum phase estimation.
      </p>
      
      <InfoBox type="note">
        <p>This tutorial assumes basic familiarity with quantum computing concepts such as qubits, quantum gates, and circuit composition. If you're new to these concepts, check out our <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link> guide first.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">What is the Quantum Fourier Transform?</h2>
      
      <p className="mb-4">
        The Quantum Fourier Transform is the quantum analog of the classical Discrete Fourier Transform (DFT), but with exponential speedup. While a classical DFT on N points requires O(N log N) operations, the QFT can be implemented with only O((log N)²) quantum gates.
      </p>
      
      <p className="mb-4">
        Mathematically, the QFT transforms a quantum state |x⟩ into a superposition of states with amplitudes related to the Fourier transform of the original state:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <p className="font-mono">
          QFT|x⟩ = (1/√N) Σ<sub>y=0</sub><sup>N-1</sup> e<sup>2πixy/N</sup>|y⟩
        </p>
      </div>
      
      <p className="mb-4">
        Where N = 2ⁿ for an n-qubit system.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">QFT Circuit Implementation</h2>
      
      <p className="mb-4">
        The QFT circuit for an n-qubit register can be constructed using Hadamard gates and controlled rotation gates:
      </p>
      
      <div className="mb-6">
        <p className="mb-2">For an n-qubit system, the circuit includes:</p>
        <ol className="list-decimal ml-8 space-y-2">
          <li>Hadamard gate on each qubit</li>
          <li>Controlled rotation gates (CR<sub>k</sub>) between pairs of qubits</li>
          <li>A final bit reversal (swap gates between symmetric qubits)</li>
        </ol>
      </div>
      
      <p className="mb-4">
        Here's how to implement the QFT in HaskQ:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit

-- Controlled rotation gate
controlledR :: Int -> Qubit -> Qubit -> Circ (Qubit, Qubit)
controlledR k control target = do
  -- Apply controlled rotation by 2π/2^k
  controlled (phaseShift (2 * pi / (2^k))) control target

-- QFT on a single qubit is just a Hadamard gate
qft1 :: Qubit -> Circ Qubit
qft1 q = hadamard q

-- QFT on n qubits
qftN :: [Qubit] -> Circ [Qubit]
qftN [] = pure []
qftN (q:qs) = do
  -- Apply Hadamard to the first qubit
  q' <- hadamard q
  
  -- Apply controlled rotations from this qubit to all others
  (q'', qs') <- applyControlledRotations q' qs 1
  
  -- Recursively apply QFT to remaining qubits
  qs'' <- qftN qs'
  
  -- Return the transformed qubits
  pure (q'' : qs'')
  where
    applyControlledRotations :: Qubit -> [Qubit] -> Int -> Circ (Qubit, [Qubit])
    applyControlledRotations control [] _ = pure (control, [])
    applyControlledRotations control (target:rest) k = do
      (control', target') <- controlledR k control target
      (control'', rest') <- applyControlledRotations control' rest (k+1)
      pure (control'', target' : rest')

-- QFT with bit reversal
qft :: [Qubit] -> Circ [Qubit]
qft qs = do
  qs' <- qftN qs
  -- Reverse the order of qubits
  pure (reverse qs')

-- Example usage
qftExample :: IO ()
qftExample = do
  let n = 3  -- Number of qubits
      result = simulateCircuit n $ do
        -- Initialize qubits in state |001⟩ (or another state of your choice)
        qs <- mapM (\\i -> if i == 0 then createQubit One else createQubit Zero) [0..n-1]
        
        -- Apply QFT
        qs' <- qft qs
        
        -- Measure all qubits
        measurementResults <- mapM (\\q -> do
                                (m, _) <- measure q
                                pure m) qs'
        
        pure measurementResults
  
  putStrLn "QFT Example Results:"
  putStrLn $ "Measurements: " ++ show (measurements result)
  putStrLn $ "State Vector: " ++ show (stateVector result)`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Understanding the QFT Implementation</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">The Core Functions</h3>
      
      <p className="mb-4">Let's break down the key components of our QFT implementation:</p>
      
      <ol className="list-decimal ml-8 space-y-4">
        <li>
          <strong>controlledR</strong>: This function implements a controlled rotation gate, where the target qubit is rotated by an angle of 2π/2ᵏ if the control qubit is in state |1⟩.
        </li>
        <li>
          <strong>qft1</strong>: For a single qubit, the QFT is simply a Hadamard gate, transforming |0⟩ to (|0⟩ + |1⟩)/√2 and |1⟩ to (|0⟩ - |1⟩)/√2.
        </li>
        <li>
          <strong>qftN</strong>: This recursive function applies the QFT to n qubits:
          <ul className="list-disc ml-8 mt-2">
            <li>Apply a Hadamard gate to the first qubit</li>
            <li>Apply controlled rotations from this qubit to all others</li>
            <li>Recursively apply QFT to the remaining qubits</li>
          </ul>
        </li>
        <li>
          <strong>qft</strong>: The complete QFT with bit reversal (swapping qubits to match the conventional output order).
        </li>
      </ol>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Visualization of QFT</h3>
      
      <p className="mb-4">
        For a 3-qubit system, the QFT circuit looks approximately like this:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg my-6 overflow-x-auto">
        <pre className="whitespace-pre">
{`q0: ──H──•──────•──────×──
         │      │      │
q1: ─────┼──•───R2─────×──
         │  │      
q2: ─────R3─R2───────────

H: Hadamard gate
R2: π/2 phase rotation (controlled)
R3: π/4 phase rotation (controlled)
×: Swap gates (bit reversal)`}
        </pre>
      </div>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Applications of QFT</h2>
      
      <p className="mb-4">
        The QFT is a key component in several important quantum algorithms:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><strong>Shor's Algorithm</strong>: Uses QFT to find the period of a function, which can be used to factor large numbers exponentially faster than the best-known classical algorithms.</li>
        <li><strong>Quantum Phase Estimation</strong>: Uses QFT to estimate the eigenvalues of a unitary operator, a crucial subroutine in many quantum algorithms.</li>
        <li><strong>Quantum Counting</strong>: Uses QFT to estimate the number of solutions to a search problem.</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Using QFT with HaskQ's Circuit Visualizer</h2>
      
      <p className="mb-4">
        You can visualize the QFT circuit using HaskQ's visualization tools:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import qualified HaskQ.Visualization as Viz

visualizeQFT :: IO ()
visualizeQFT = do
  let n = 3  -- Number of qubits
      circuit = do
        -- Initialize qubits
        qs <- mapM (\\_ -> createQubit Zero) [1..n]
        
        -- Apply QFT
        qs' <- qft qs
        
        -- Return the final qubits
        pure qs'
  
  -- Draw the circuit to an SVG file
  Viz.drawCircuit "qft_circuit.svg" circuit`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Optimizations and Considerations</h2>
      
      <InfoBox type="tip" title="Performance Tip">
        <p>For large numbers of qubits, you can optimize the QFT implementation by approximating it. Research has shown that controlled rotation gates with very small rotation angles (less than some threshold) can be omitted with minimal impact on accuracy.</p>
      </InfoBox>
      
      <p className="mb-4">
        In practice, an optimized version of QFT might look like this:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Approximate QFT with a threshold for rotation angles
approxQFT :: Double -> [Qubit] -> Circ [Qubit]
approxQFT threshold qs = do
  qs' <- approxQFTN threshold qs
  pure (reverse qs')
  where
    approxQFTN :: Double -> [Qubit] -> Circ [Qubit]
    approxQFTN _ [] = pure []
    approxQFTN threshold (q:qs) = do
      q' <- hadamard q
      (q'', qs') <- applyControlledRotations threshold q' qs 1
      qs'' <- approxQFTN threshold qs'
      pure (q'' : qs'')
    
    applyControlledRotations :: Double -> Qubit -> [Qubit] -> Int -> Circ (Qubit, [Qubit])
    applyControlledRotations _ control [] _ = pure (control, [])
    applyControlledRotations threshold control (target:rest) k = do
      -- Skip rotations smaller than the threshold
      (control', target') <- if 2*pi/(2^k) >= threshold
                             then controlledR k control target
                             else pure (control, target)
      (control'', rest') <- applyControlledRotations threshold control' rest (k+1)
      pure (control'', target' : rest')`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Practical Exercise: Implementing Phase Estimation</h2>
      
      <p className="mb-4">
        As a practical exercise, you can use the QFT to implement the Quantum Phase Estimation algorithm, which is used to estimate the eigenvalue of a unitary operator:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Phase estimation circuit
phaseEstimation :: Int -> (Qubit -> Circ Qubit) -> Qubit -> Circ ([Qubit], Qubit)
phaseEstimation precision unitary targetQubit = do
  -- Create precision qubits in superposition
  controlQubits <- mapM (\\_ -> createQubit Zero) [1..precision]
  controlQubits' <- mapM hadamard controlQubits
  
  -- Apply controlled-U operations
  (controlQubits'', targetQubit') <- applyControlledU controlQubits' targetQubit
  
  -- Apply inverse QFT to control qubits
  controlQubits''' <- inverseQFT controlQubits''
  
  pure (controlQubits''', targetQubit')
  where
    -- Apply controlled-U^(2^j) operations
    applyControlledU :: [Qubit] -> Qubit -> Circ ([Qubit], Qubit)
    applyControlledU [] target = pure ([], target)
    applyControlledU (control:controls) target = do
      -- Apply controlled-U^(2^j)
      (control', target') <- controlledPower control target (length controls)
      (controls', target'') <- applyControlledU controls target'
      pure (control' : controls', target'')
    
    -- Apply U^(2^power) controlled by control qubit
    controlledPower :: Qubit -> Qubit -> Int -> Circ (Qubit, Qubit)
    controlledPower control target power = do
      -- In a real implementation, you would apply the unitary operation 2^power times
      -- This is a simplified version
      (control', target') <- controlled unitary control target
      pure (control', target')
    
    -- Inverse QFT is the same as QFT but with conjugate rotation angles
    inverseQFT :: [Qubit] -> Circ [Qubit]
    inverseQFT qs = do
      -- Apply QFT in reverse order with conjugate phases
      -- For brevity, this is not fully implemented here
      -- A complete implementation would reverse the rotations in the QFT
      reverse <$> qft (reverse qs)`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Conclusion</h2>
      
      <p className="mb-4">
        The Quantum Fourier Transform is a powerful quantum algorithm that provides exponential speedup over its classical counterpart. In this tutorial, we've covered:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li>The mathematical foundation of QFT</li>
        <li>Implementation of QFT in HaskQ</li>
        <li>Applications of QFT in quantum computing</li>
        <li>Optimizations and practical considerations</li>
        <li>A starting point for implementing the Quantum Phase Estimation algorithm</li>
      </ul>
      
      <p className="mb-4">
        By mastering the QFT, you've taken a significant step toward understanding many of the most important quantum algorithms.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><Link href="/docs/core-concepts/quantum-algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Algorithms Overview</Link></li>
        <li><Link href="/docs/tutorials/shor-algorithm" className="text-indigo-600 dark:text-indigo-400 hover:underline">Shor's Algorithm Tutorial</Link></li>
        <li><Link href="/docs/tutorials/phase-estimation" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Phase Estimation</Link></li>
      </ul>
    </div>
  );
} 