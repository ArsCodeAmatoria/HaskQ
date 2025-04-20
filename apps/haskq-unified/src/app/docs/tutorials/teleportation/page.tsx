'use client';

import React from 'react';
import Link from 'next/link';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function TeleportationTutorialPage() {
  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">Quantum Teleportation Tutorial</h1>
      
      <p className="text-lg mb-6">
        Quantum teleportation is a protocol that allows the transfer of a quantum state from one location to another
        using previously shared quantum entanglement and classical communication. Despite its name, teleportation
        doesn't involve the physical transfer of matter or energy - only the quantum information is "teleported".
      </p>
      
      <InfoBox type="note">
        <p>This tutorial assumes familiarity with quantum entanglement and Bell states. If you're new to these concepts, 
        we recommend first reading our <Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell States Tutorial</Link>.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">The Quantum Teleportation Protocol</h2>
      
      <p className="mb-4">
        Quantum teleportation allows us to transmit the state of a qubit from one location (Alice) to another (Bob) without physically 
        sending the qubit itself. Here's how it works:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2 mb-6">
        <li>Alice and Bob share an entangled pair of qubits (a Bell state)</li>
        <li>Alice has a third qubit in some state |ψ⟩ that she wants to teleport to Bob</li>
        <li>Alice performs a joint measurement on her qubit from the entangled pair and the qubit she wants to teleport</li>
        <li>Alice sends the classical measurement results to Bob (two classical bits)</li>
        <li>Based on these two bits, Bob performs one of four possible operations on his qubit</li>
        <li>After Bob's operation, his qubit is now in the same state |ψ⟩ that Alice's original qubit was in</li>
      </ol>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Quantum Circuit for Teleportation</h2>
      
      <p className="mb-4">
        The teleportation protocol can be represented as the following quantum circuit:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <pre className="whitespace-pre">
{`|ψ⟩ ─────•─────H─────M──────────────────
         │             │
|0⟩ ──H──X───────────M─┼────────────────
                        │
|0⟩ ──────────────────Z─X───── |ψ⟩`}
        </pre>
      </div>
      
      <p className="mb-4">
        The top line is Alice's qubit to be teleported (in state |ψ⟩). The middle line is Alice's half of the entangled pair, 
        and the bottom line is Bob's half. The 'M' boxes represent measurements, and the Z and X operations on Bob's qubit 
        are conditional on Alice's measurement results.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Implementing Quantum Teleportation in HaskQ</h2>
      
      <p className="mb-4">
        Let's implement the quantum teleportation protocol step by step using HaskQ:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit

-- Function to create a Bell state (|00⟩ + |11⟩)/√2
createBellState :: Circ (Qubit, Qubit)
createBellState = do
  -- Create two qubits in the |0⟩ state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply Hadamard to the first qubit
  q1' <- hadamard q1
  
  -- Apply CNOT with q1 as control and q2 as target
  (q1'', q2') <- controlled not q1' q2
  
  -- Return the entangled qubits
  pure (q1'', q2')

-- Function to prepare a qubit in a custom state
prepareQubit :: Double -> Double -> Double -> Circ Qubit
prepareQubit theta phi lambda = do
  -- Create a qubit in |0⟩ state
  q <- createQubit Zero
  
  -- Apply U3 gate to create any single-qubit state
  -- U3(theta, phi, lambda) = Rz(phi) Ry(theta) Rz(lambda)
  q' <- rotateZ lambda q
  q'' <- rotateY theta q'
  q''' <- rotateZ phi q''
  
  pure q'''

-- Teleportation circuit
teleport :: Qubit -> Circ Qubit
teleport stateToTeleport = do
  -- Alice and Bob share a Bell state
  (aliceEntangled, bobEntangled) <- createBellState
  
  -- Alice entangles her qubit with her half of the Bell state
  (stateToTeleport', aliceEntangled') <- controlled not stateToTeleport aliceEntangled
  
  -- Alice applies Hadamard to her qubit
  stateToTeleport'' <- hadamard stateToTeleport'
  
  -- Alice measures her qubits
  (m1, _) <- measure stateToTeleport''
  (m2, _) <- measure aliceEntangled'
  
  -- Bob applies corrections based on Alice's measurement results
  bobFinal <- case (m1, m2) of
    (Zero, Zero) -> pure bobEntangled                 -- No correction needed
    (Zero, One)  -> pauliX bobEntangled               -- Apply X gate
    (One, Zero)  -> pauliZ bobEntangled               -- Apply Z gate
    (One, One)   -> do                                -- Apply both Z and X gates
                     bobTemp <- pauliZ bobEntangled
                     pauliX bobTemp
  
  -- Bob's qubit is now in the state that Alice wanted to teleport
  pure bobFinal

-- Main function to demonstrate teleportation
main :: IO ()
main = do
  let -- Define a state to teleport
      theta = pi/3  -- Some arbitrary angles to create an interesting state
      phi = pi/4
      lambda = pi/6
      
      -- Simulate teleportation
      result = simulateCircuit 3 $ do
        -- Create the state to teleport
        stateToTeleport <- prepareQubit theta phi lambda
        
        -- For reference, measure what the state was (in a separate simulation branch)
        (originalMeasurement, _) <- measure stateToTeleport
        
        -- Teleport the state
        teleportedQubit <- teleport stateToTeleport
        
        -- Measure the teleported state
        (teleportedMeasurement, _) <- measure teleportedQubit
        
        pure [originalMeasurement, teleportedMeasurement]
  
  putStrLn "Quantum Teleportation Results:"
  putStrLn $ "Original state measurement: " ++ show (head (measurements result))
  putStrLn $ "Teleported state measurement: " ++ show (measurements result !! 1)
  
  -- In reality, we would need to run this many times to verify the statistics match`}
        className="my-6"
      />
      
      <InfoBox type="warning">
        <p>The code above measures the state at the end, which collapses the quantum state. For verification purposes, we would 
        need to run this multiple times and compare the statistics of measurements on both the original and teleported qubits.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Detailed Explanation of the Protocol</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Initial Setup</h3>
      
      <p className="mb-4">
        At the start of the protocol, we have three qubits:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li>Alice's qubit |ψ⟩ = α|0⟩ + β|1⟩ (the state to be teleported)</li>
        <li>A shared entangled pair in the Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2</li>
      </ul>
      
      <p className="mb-4">
        The overall state of the three-qubit system can be written as:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <p className="font-mono">
          |ψ⟩ ⊗ |Φ⁺⟩ = (α|0⟩ + β|1⟩) ⊗ (|00⟩ + |11⟩)/√2
        </p>
      </div>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Alice's Operations</h3>
      
      <p className="mb-4">
        Alice performs two operations:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2">
        <li>A CNOT gate between her qubit |ψ⟩ and her half of the Bell pair</li>
        <li>A Hadamard gate on her qubit |ψ⟩</li>
      </ol>
      
      <p className="mb-4">
        After these operations, the system is in a superposition of four possible states, each corresponding to one of the four possible measurement outcomes.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Measurement and Classical Communication</h3>
      
      <p className="mb-4">
        Alice measures her two qubits, obtaining one of four possible outcomes: 00, 01, 10, or 11. 
        This measurement collapses the quantum state and determines which operation Bob needs to perform.
      </p>
      
      <p className="mb-4">
        Alice then sends these two classical bits to Bob through a classical communication channel.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Bob's Corrections</h3>
      
      <p className="mb-4">
        Based on the two classical bits he receives, Bob applies one of four possible corrections to his qubit:
      </p>
      
      <div className="overflow-x-auto">
        <table className="min-w-full bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 my-6">
          <thead>
            <tr>
              <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Alice's Measurement</th>
              <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Bob's Operation</th>
              <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Resulting State</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">00</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">I (Identity, do nothing)</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">α|0⟩ + β|1⟩</td>
            </tr>
            <tr>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">01</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">X (Bit-flip)</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">α|0⟩ + β|1⟩</td>
            </tr>
            <tr>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">10</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Z (Phase-flip)</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">α|0⟩ + β|1⟩</td>
            </tr>
            <tr>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">11</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">ZX (Both phase and bit-flip)</td>
              <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">α|0⟩ + β|1⟩</td>
            </tr>
          </tbody>
        </table>
      </div>
      
      <p className="mb-4">
        After applying the appropriate correction, Bob's qubit will be in the state |ψ⟩ = α|0⟩ + β|1⟩, identical to the state 
        Alice wanted to teleport.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Teleporting More Complex States</h2>
      
      <p className="mb-4">
        The teleportation protocol works for any single-qubit state, regardless of how complex it is. Let's demonstrate 
        this by teleporting a superposition state with a specific phase:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Teleport a specific state and analyze the result
teleportSpecificState :: IO ()
teleportSpecificState = do
  let -- Create state |ψ⟩ = (|0⟩ + i|1⟩)/√2
      result = simulateCircuit 3 $ do
        -- Create the state to teleport
        stateToTeleport <- createQubit Zero
        stateToTeleport' <- hadamard stateToTeleport
        stateToTeleport'' <- phaseShift (pi/2) stateToTeleport'  -- Apply S gate to add the 'i' phase
        
        -- Teleport the state
        teleportedQubit <- teleport stateToTeleport''
        
        -- Return the qubits for state vector analysis
        pure [teleportedQubit]
  
  -- Print the state vector to see the full quantum state
  putStrLn "Teleportation of state |ψ⟩ = (|0⟩ + i|1⟩)/√2:"
  putStrLn $ "State vector of teleported qubit: " ++ show (stateVector result)
  putStrLn $ "Probabilities: " ++ show (probabilities result)`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Verifying the Teleportation</h2>
      
      <p className="mb-4">
        To verify that the teleportation protocol works correctly, we need to check that statistical measurements 
        on the teleported qubit match those on the original qubit. Let's implement a function to do this:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Function to verify teleportation by statistical measurements
verifyTeleportation :: Int -> IO ()
verifyTeleportation numTrials = do
  -- Create a specific state to teleport
  let theta = pi/3
      phi = pi/4
      lambda = pi/6
  
  -- Run many trials and collect statistics
  originalResults <- replicateM numTrials $ do
    let result = simulateCircuit 1 $ do
          q <- prepareQubit theta phi lambda
          (m, _) <- measure q
          pure m
    pure (head (measurements result))
  
  teleportedResults <- replicateM numTrials $ do
    let result = simulateCircuit 3 $ do
          q <- prepareQubit theta phi lambda
          q' <- teleport q
          (m, _) <- measure q'
          pure m
    pure (head (measurements result))
  
  -- Calculate statistics
  let originalZeros = length $ filter (== Zero) originalResults
      originalOnes = length $ filter (== One) originalResults
      teleportedZeros = length $ filter (== Zero) teleportedResults
      teleportedOnes = length $ filter (== One) teleportedResults
      
      originalZeroProb = fromIntegral originalZeros / fromIntegral numTrials * 100
      originalOneProb = fromIntegral originalOnes / fromIntegral numTrials * 100
      teleportedZeroProb = fromIntegral teleportedZeros / fromIntegral numTrials * 100
      teleportedOneProb = fromIntegral teleportedOnes / fromIntegral numTrials * 100
  
  -- Print results
  putStrLn $ "Statistics over " ++ show numTrials ++ " trials:"
  putStrLn $ "Original state |0⟩ probability: " ++ show originalZeroProb ++ "%"
  putStrLn $ "Original state |1⟩ probability: " ++ show originalOneProb ++ "%"
  putStrLn $ "Teleported state |0⟩ probability: " ++ show teleportedZeroProb ++ "%"
  putStrLn $ "Teleported state |1⟩ probability: " ++ show teleportedOneProb ++ "%"
  
  -- Check if probabilities are close
  let tolerance = 2.0  -- Allow for some statistical variation
      zerosDiff = abs (originalZeroProb - teleportedZeroProb)
      onesDiff = abs (originalOneProb - teleportedOneProb)
  
  if zerosDiff <= tolerance && onesDiff <= tolerance
    then putStrLn "Verification SUCCESSFUL: Teleportation preserves quantum state statistics."
    else putStrLn "Verification FAILED: Teleportation does not preserve quantum state statistics."

-- Example usage
main :: IO ()
main = do
  verifyTeleportation 1000  -- Run 1000 trials`}
        className="my-6"
      />
      
      <InfoBox type="note">
        <p>In a real quantum computer, each qubit measurement is a destructive operation. Therefore, we need to prepare 
        identical states and perform measurements multiple times to verify the statistical behavior.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Key Properties of Quantum Teleportation</h2>
      
      <p className="mb-4">
        The quantum teleportation protocol has several important properties:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><strong>No-cloning theorem compliance</strong>: The original qubit state is destroyed during the process, 
        ensuring the no-cloning theorem isn't violated</li>
        <li><strong>Entanglement as a resource</strong>: Teleportation requires pre-shared entanglement between the parties</li>
        <li><strong>Classical communication requirement</strong>: Two classical bits must be transmitted from Alice to Bob</li>
        <li><strong>No faster-than-light information transfer</strong>: Because classical bits are required, information cannot 
        travel faster than light</li>
        <li><strong>Works for any qubit state</strong>: The protocol can teleport any arbitrary quantum state |ψ⟩</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Practical Applications</h2>
      
      <p className="mb-4">
        Quantum teleportation has several important applications in quantum information processing:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><strong>Quantum networks</strong>: Enables the transfer of quantum states between remote quantum processors</li>
        <li><strong>Quantum repeaters</strong>: Helps overcome distance limitations in quantum communication</li>
        <li><strong>Distributed quantum computing</strong>: Facilitates computation across physically separated quantum processors</li>
        <li><strong>Quantum error correction</strong>: Serves as a primitive operation in some quantum error correction schemes</li>
        <li><strong>Quantum cryptography</strong>: Enhances the security and capabilities of quantum key distribution</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Extensions to Multiple Qubits</h2>
      
      <p className="mb-4">
        The teleportation protocol can be extended to teleport multiple qubits or even quantum registers:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Teleport multiple qubits
teleportMultipleQubits :: [Qubit] -> Circ [Qubit]
teleportMultipleQubits qubits = do
  -- Teleport each qubit individually
  mapM teleport qubits`}
        className="my-6"
      />
      
      <p className="mb-4">
        Note that teleporting n qubits requires n shared Bell pairs and the transmission of 2n classical bits.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Conclusion</h2>
      
      <p className="mb-4">
        Quantum teleportation is a fundamental protocol in quantum information science that demonstrates 
        the power of quantum entanglement as a resource. While it doesn't allow for faster-than-light communication, 
        it provides a way to transmit quantum states using entanglement and classical communication.
      </p>
      
      <p className="mb-4">
        In this tutorial, we've:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li>Explored the quantum teleportation protocol step by step</li>
        <li>Implemented teleportation in HaskQ</li>
        <li>Verified that the protocol correctly preserves quantum states</li>
        <li>Discussed key properties and applications of quantum teleportation</li>
      </ul>
      
      <p className="mb-4">
        Understanding quantum teleportation provides insight into the nature of quantum information and 
        forms a foundation for more advanced quantum communication protocols.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><Link href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell States Tutorial</Link></li>
        <li><Link href="/docs/tutorials/superdense-coding" className="text-indigo-600 dark:text-indigo-400 hover:underline">Superdense Coding Tutorial</Link></li>
        <li><Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Entanglement</Link></li>
      </ul>
    </div>
  );
} 