'use client';

import React from 'react';
import Link from 'next/link';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function ErrorCorrectionPage() {
  return (
    <DocLayout 
      title="Quantum Error Correction" 
      description="Understanding and implementing quantum error correction codes in HaskQ"
    >
      <div className="max-w-none prose dark:prose-invert">
        <p className="mb-6">
          Quantum computers are highly sensitive to environmental noise, which can cause errors in quantum calculations.
          Quantum Error Correction (QEC) is essential for building reliable quantum computers that can perform complex
          computations. This document explains the principles of QEC and how to implement error correction codes in HaskQ.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Why Quantum Error Correction is Necessary</h2>
        <p className="mb-4">
          Unlike classical computers, quantum computers face unique challenges with errors:
        </p>
        
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>No-cloning theorem</strong>: We cannot simply create backup copies of quantum states</li>
          <li><strong>Continuous error space</strong>: Quantum errors can be any arbitrary rotation, not just bit flips</li>
          <li><strong>Measurement destroys states</strong>: We cannot directly measure qubits to check for errors</li>
          <li><strong>Decoherence</strong>: Quantum states naturally deteriorate over time due to interaction with the environment</li>
        </ul>
        
        <p className="mb-4">
          These challenges make quantum error correction significantly more complex than classical error correction,
          requiring specialized techniques and codes.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Types of Quantum Errors</h2>
        <p className="mb-4">
          Quantum errors generally fall into two categories:
        </p>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <h3 className="font-bold mb-2">Bit Flip Errors</h3>
            <p>
              Equivalent to Pauli-X errors, these flip a qubit's state from |0⟩ to |1⟩ or vice versa.
              Mathematically, this corresponds to applying an X gate to the qubit.
            </p>
            <div className="mt-2 text-center font-mono">
              |0⟩ → |1⟩ and |1⟩ → |0⟩
            </div>
          </div>
          
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <h3 className="font-bold mb-2">Phase Flip Errors</h3>
            <p>
              Equivalent to Pauli-Z errors, these introduce a relative phase of -1 between |0⟩ and |1⟩.
              Mathematically, this corresponds to applying a Z gate to the qubit.
            </p>
            <div className="mt-2 text-center font-mono">
              |0⟩ → |0⟩ and |1⟩ → -|1⟩
            </div>
          </div>
        </div>
        
        <p className="mb-4">
          More general errors can be represented as combinations of these basic errors.
          The Pauli-Y error, for instance, applies both a bit flip and a phase flip.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Basic Principles of Quantum Error Correction</h2>
        <p className="mb-4">
          Quantum error correction relies on several key principles:
        </p>
        
        <ol className="list-decimal ml-6 mb-6 space-y-2">
          <li><strong>Redundant encoding</strong>: Encode logical qubits using multiple physical qubits</li>
          <li><strong>Error syndromes</strong>: Measure properties that reveal errors without measuring the actual state</li>
          <li><strong>Quantum measurements</strong>: Project erroneous states into a correctable subspace</li>
          <li><strong>Recovery operations</strong>: Apply corrections based on syndrome measurements</li>
        </ol>

        <InfoBox type="note" title="The Quantum Error Correction Threshold Theorem">
          <p>
            One of the most important results in quantum computing is the threshold theorem, which states that
            if the error rate per physical gate is below a certain threshold, then arbitrarily long quantum
            computations can be performed reliably using quantum error correction. Current estimates place this
            threshold around 1% error rate per gate.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Common Quantum Error Correction Codes</h2>
        <p className="mb-4">
          Several error correction codes have been developed for quantum computing:
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Bit-Flip Code (3-Qubit Repetition)</h3>
        <p className="mb-4">
          The bit-flip code protects against X (bit-flip) errors by encoding a logical qubit using three physical qubits:
        </p>
        
        <div className="flex justify-center my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <p className="text-center font-mono">
              |0⟩<sub>L</sub> = |000⟩<br />
              |1⟩<sub>L</sub> = |111⟩
            </p>
          </div>
        </div>
        
        <p className="mb-4">
          If one qubit experiences a bit-flip error, majority voting can identify and correct the error.
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Phase-Flip Code</h3>
        <p className="mb-4">
          The phase-flip code protects against Z (phase-flip) errors using a similar approach, but in the Hadamard basis:
        </p>
        
        <div className="flex justify-center my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <p className="text-center font-mono">
              |0⟩<sub>L</sub> = |+++⟩<br />
              |1⟩<sub>L</sub> = |---⟩
            </p>
          </div>
        </div>
        
        <p className="mb-4">
          Where |+⟩ = (|0⟩ + |1⟩)/√2 and |-⟩ = (|0⟩ - |1⟩)/√2.
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Shor's 9-Qubit Code</h3>
        <p className="mb-4">
          The first complete quantum error correction code, Shor's code protects against both bit-flip and phase-flip errors:
        </p>
        
        <div className="flex justify-center my-6">
          <div className="bg-white dark:bg-gray-800 p-4 rounded-lg shadow-sm">
            <p className="text-center font-mono">
              |0⟩<sub>L</sub> = (|000⟩ + |111⟩)(|000⟩ + |111⟩)(|000⟩ + |111⟩)/2√2<br />
              |1⟩<sub>L</sub> = (|000⟩ - |111⟩)(|000⟩ - |111⟩)(|000⟩ - |111⟩)/2√2
            </p>
          </div>
        </div>
        
        <p className="mb-4">
          Shor's code can correct any arbitrary error on a single qubit.
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Steane's 7-Qubit Code</h3>
        <p className="mb-4">
          A more efficient code that uses 7 qubits to protect against any single-qubit error, based on classical
          Hamming codes.
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Surface Codes</h3>
        <p className="mb-4">
          Surface codes are the most promising for practical quantum computing due to their high threshold and
          2D local structure. They use a grid of physical qubits to encode information and can be scaled to
          arbitrarily large sizes.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Implementing Error Correction in HaskQ</h2>
        <p className="mb-4">
          HaskQ provides tools to implement quantum error correction codes. Let's look at some examples:
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">3-Qubit Bit-Flip Code</h3>
        <p className="mb-4">
          Here's how to implement the 3-qubit bit-flip code in HaskQ:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.ErrorCorrection.BitFlip

-- Encode a logical qubit into three physical qubits
encodeBitFlip :: Qubit %1-> Circ [Qubit]
encodeBitFlip q = do
  -- Create two ancilla qubits in |0⟩ state
  a1 <- createQubit Zero
  a2 <- createQubit Zero
  
  -- Apply CNOT gates to entangle them with the input qubit
  (q', a1') <- controlled not q a1
  (q'', a2') <- controlled not q' a2
  
  -- Return the three qubits that now encode the logical state
  pure [q'', a1', a2']

-- Detect and correct a bit-flip error
correctBitFlip :: [Qubit] %1-> Circ (Qubit, [Qubit])
correctBitFlip qubits = do
  -- Create two ancilla qubits for syndrome measurement
  s1 <- createQubit Zero
  s2 <- createQubit Zero
  
  -- Measure error syndromes using CNOT gates
  -- q0 ─•─ q0
  --     │
  -- q1 ─•─ q1
  --     │
  -- s1 ─⊕─ s1 (syndrome 1)
  --
  -- q1 ─•─ q1
  --     │
  -- q2 ─•─ q2
  --     │
  -- s2 ─⊕─ s2 (syndrome 2)
  case qubits of
    [q0, q1, q2] -> do
      (q0', q1', s1') <- controlledCNOT q0 q1 s1
      (q1'', q2', s2') <- controlledCNOT q1' q2 s2
      
      -- Measure syndrome qubits
      (m1, _) <- measure s1'
      (m2, _) <- measure s2'
      
      -- Apply correction based on syndrome
      (correctedQ, remainingQs) <- case (m1, m2) of
        (Zero, Zero) -> pure (q0', [q1'', q2'])   -- No error
        (One, Zero)  -> do
                         q0'' <- pauliX q0'      -- Error on q0
                         pure (q0'', [q1'', q2'])
        (One, One)   -> do
                         q1''' <- pauliX q1''    -- Error on q1
                         pure (q0', [q1''', q2'])
        (Zero, One)  -> do
                         q2'' <- pauliX q2'      -- Error on q2
                         pure (q0', [q1'', q2''])
      
      pure (correctedQ, remainingQs)
    _ -> error "Bit-flip code requires exactly 3 qubits"

-- Decode the logical qubit back to a single physical qubit
decodeBitFlip :: [Qubit] %1-> Circ Qubit
decodeBitFlip qubits = case qubits of
  [q0, q1, q2] -> do
    -- Apply CNOTs to disentangle the qubits
    (q0', q1') <- controlled not q0 q1
    (q0'', q2') <- controlled not q0' q2
    
    -- Measure and discard the ancilla qubits
    (_, _) <- measure q1'
    (_, _) <- measure q2'
    
    -- Return the original logical qubit
    pure q0''
  _ -> error "Bit-flip code requires exactly 3 qubits"

-- Complete error correction example
bitFlipCorrection :: Qubit %1-> Circ Qubit
bitFlipCorrection q = do
  -- Encode the qubit using the bit-flip code
  encoded <- encodeBitFlip q
  
  -- Simulate a bit-flip error on one of the qubits
  encoded' <- simulateError encoded
  
  -- Detect and correct the error
  (corrected, remaining) <- correctBitFlip encoded'
  
  -- Decode back to a single qubit
  decoded <- decodeBitFlip (corrected:remaining)
  
  pure decoded

-- Helper function to simulate a bit-flip error on the first qubit
simulateError :: [Qubit] %1-> Circ [Qubit]
simulateError qubits = case qubits of
  (q:qs) -> do
    q' <- pauliX q  -- Apply an X error
    pure (q':qs)
  [] -> pure []`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Phase-Flip Code</h3>
        <p className="mb-4">
          The phase-flip code is similar but works in the Hadamard basis:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Encode a logical qubit to protect against phase flips
encodePhaseFlip :: Qubit %1-> Circ [Qubit]
encodePhaseFlip q = do
  -- Create two ancilla qubits
  a1 <- createQubit Zero
  a2 <- createQubit Zero
  
  -- Apply Hadamard gates to all qubits
  q' <- hadamard q
  a1' <- hadamard a1
  a2' <- hadamard a2
  
  -- Apply CNOT gates to entangle them
  (q'', a1'') <- controlled not q' a1'
  (q''', a2'') <- controlled not q'' a2'
  
  -- Apply Hadamard gates again
  q'''' <- hadamard q'''
  a1''' <- hadamard a1''
  a2''' <- hadamard a2''
  
  pure [q'''', a1''', a2''']

-- Detect and correct a phase-flip error
correctPhaseFlip :: [Qubit] %1-> Circ (Qubit, [Qubit])
correctPhaseFlip qubits = do
  -- First convert to Hadamard basis
  qubits' <- mapM hadamard qubits
  
  -- Perform bit-flip correction in this basis
  (corrected, remaining) <- correctBitFlip qubits'
  
  -- Convert back from Hadamard basis
  corrected' <- hadamard corrected
  remaining' <- mapM hadamard remaining
  
  pure (corrected', remaining')`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Shor's 9-Qubit Code</h3>
        <p className="mb-4">
          Shor's code combines bit-flip and phase-flip protection:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Encode a logical qubit using Shor's 9-qubit code
encodeShor :: Qubit %1-> Circ [Qubit]
encodeShor q = do
  -- First apply phase-flip encoding
  phaseEncoded <- encodePhaseFlip q
  
  -- Then apply bit-flip encoding to each of the 3 qubits
  bitFlipEncoded1 <- encodeBitFlip (phaseEncoded !! 0)
  bitFlipEncoded2 <- encodeBitFlip (phaseEncoded !! 1)
  bitFlipEncoded3 <- encodeBitFlip (phaseEncoded !! 2)
  
  -- Combine all 9 qubits
  pure (bitFlipEncoded1 ++ bitFlipEncoded2 ++ bitFlipEncoded3)

-- Error correction for Shor's code follows a similar nested approach
correctShor :: [Qubit] %1-> Circ Qubit
correctShor qubits = do
  -- First correct bit-flip errors within each block of 3 qubits
  -- Then correct phase-flip errors across blocks
  -- Implementation omitted for brevity
  ...`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Error Detection vs. Error Correction</h2>
        <p className="mb-4">
          It's important to distinguish between:
        </p>
        
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Error detection</strong>: Identifies when an error has occurred</li>
          <li><strong>Error correction</strong>: Identifies and fixes the error</li>
        </ul>
        
        <p className="mb-4">
          Some quantum codes only detect errors but don't correct them. In such cases, if an error is detected,
          the computation might need to be restarted.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Fault-Tolerant Quantum Computing</h2>
        <p className="mb-4">
          Error correction is just one component of fault-tolerant quantum computing. Full fault tolerance requires:
        </p>
        
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Error-correcting codes</strong>: To protect quantum information</li>
          <li><strong>Fault-tolerant gate implementations</strong>: So errors don't propagate uncontrollably</li>
          <li><strong>Error syndrome extraction</strong>: To detect errors without disrupting the computation</li>
          <li><strong>Logical operations</strong>: To manipulate encoded qubits without decoding them</li>
        </ul>
        
        <p className="mb-4">
          HaskQ provides tools to experiment with these concepts, though full fault-tolerance requires
          sophisticated hardware and software integration.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Simulating Noisy Quantum Systems</h2>
        <p className="mb-4">
          HaskQ allows you to simulate noisy quantum systems to test error correction codes:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`import HaskQ.Simulator.Noise

-- Simulate bit-flip noise with a given error probability
simulateNoisyCircuit :: Double -> Circ a -> IO (SimulationResult a)
simulateNoisyCircuit errorProb circuit = do
  -- Create a noise model with bit-flip errors
  let noiseModel = BitFlipNoise errorProb
  
  -- Run the simulation with the noise model
  runNoisySimulation noiseModel circuit

-- Test error correction with different error rates
testErrorCorrection :: IO ()
testErrorCorrection = do
  let errorRates = [0.01, 0.05, 0.1, 0.15]
  
  -- Test each error rate
  forM_ errorRates $ \\rate -> do
    -- Create a qubit in superposition
    let circuit = do
          q <- createQubit Zero
          q' <- hadamard q
          
          -- With error correction
          encoded <- encodeBitFlip q'
          -- Simulate operations...
          (corrected, remaining) <- correctBitFlip encoded
          decoded <- decodeBitFlip (corrected:remaining)
          pure decoded
          
          -- Without error correction (for comparison)
          -- pure q'
    
    -- Run with noise
    result <- simulateNoisyCircuit rate circuit
    
    -- Analyze fidelity
    let fidelity = calculateFidelity result (1/sqrt 2, 1/sqrt 2)
    putStrLn $ "Error rate: " ++ show rate ++ ", Fidelity: " ++ show fidelity`}
          className="my-6"
        />

        <InfoBox type="tip" title="Error Correction in Practice">
          <p>
            When implementing error correction in real quantum systems, it's important to consider the overhead.
            Current quantum error correction codes require many physical qubits per logical qubit (ranging from 7 to
            thousands depending on the code). The trade-off between resource requirements and error protection
            is a critical consideration.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Current Challenges and Research</h2>
        <p className="mb-4">
          Quantum error correction is an active research area with several ongoing challenges:
        </p>
        
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Reducing overhead</strong>: Developing codes that require fewer physical qubits</li>
          <li><strong>Improving thresholds</strong>: Creating codes that can tolerate higher error rates</li>
          <li><strong>Hardware-aware encoding</strong>: Tailoring codes to specific quantum hardware</li>
          <li><strong>Implementing logical operations</strong>: Performing fault-tolerant gates on encoded qubits</li>
          <li><strong>Decoding algorithms</strong>: Efficiently determining the correct recovery operation</li>
        </ul>
        
        <p className="mb-4">
          HaskQ provides a platform for experimenting with new error correction techniques and
          contributing to this evolving field.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates</Link></li>
          <li><Link href="/docs/core-concepts/circuit-composition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Circuit Composition</Link></li>
          <li><Link href="/docs/tutorials/error-correction" className="text-indigo-600 dark:text-indigo-400 hover:underline">Error Correction Tutorial</Link></li>
        </ul>
      </div>
    </DocLayout>
  );
} 