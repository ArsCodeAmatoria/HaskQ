'use client';

import React from 'react';
import Link from 'next/link';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function BellStatesTutorialPage() {
  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">Bell States Tutorial</h1>
      
      <p className="text-lg mb-6">
        Bell states are the simplest examples of maximally entangled quantum states and form the foundation for many quantum protocols. 
        In this tutorial, we'll learn how to create and manipulate Bell states using HaskQ.
      </p>
      
      <InfoBox type="note">
        <p>This tutorial assumes basic familiarity with quantum computing concepts. If you're new to quantum computing, 
        check out our <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link> guide first.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">What are Bell States?</h2>
      
      <p className="mb-4">
        Bell states (also known as EPR pairs) are maximally entangled quantum states of two qubits. They exhibit a fascinating property: 
        when one qubit is measured, the state of the other qubit is instantly determined, regardless of the distance between them.
      </p>
      
      <p className="mb-4">
        There are four Bell states, often denoted as:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg my-6">
        <p className="font-mono mb-2">|Φ⁺⟩ = (|00⟩ + |11⟩)/√2</p>
        <p className="font-mono mb-2">|Φ⁻⟩ = (|00⟩ - |11⟩)/√2</p>
        <p className="font-mono mb-2">|Ψ⁺⟩ = (|01⟩ + |10⟩)/√2</p>
        <p className="font-mono">|Ψ⁻⟩ = (|01⟩ - |10⟩)/√2</p>
      </div>
      
      <p className="mb-4">
        These states form a basis for the 2-qubit Hilbert space and have important applications in quantum teleportation, 
        superdense coding, and quantum key distribution.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Creating a Bell State</h2>
      
      <p className="mb-4">
        The most common Bell state, |Φ⁺⟩ = (|00⟩ + |11⟩)/√2, can be created using the following circuit:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <pre className="whitespace-pre">
{`q1: |0⟩ ──H────•──
            │
q2: |0⟩ ─────X──`}
        </pre>
      </div>
      
      <p className="mb-4">
        The circuit applies a Hadamard gate to the first qubit, creating a superposition state (|0⟩ + |1⟩)/√2. 
        Then, a CNOT gate is applied with the first qubit as the control and the second qubit as the target. 
        This entangles the two qubits, creating the Bell state.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Implementing Bell States in HaskQ</h2>
      
      <p className="mb-4">
        Let's implement the Bell state creation circuit in HaskQ:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit

-- Function to create a Bell state (|00⟩ + |11⟩)/√2
bellState :: Circ (Qubit, Qubit)
bellState = do
  -- Create two qubits in the |0⟩ state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply Hadamard to the first qubit
  q1' <- hadamard q1
  
  -- Apply CNOT with q1 as control and q2 as target
  (q1'', q2') <- controlled not q1' q2
  
  -- Return the entangled qubits
  pure (q1'', q2')

-- Main function to run the simulation
main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        -- Create the Bell state
        (q1, q2) <- bellState
        
        -- Measure both qubits
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        
        pure [m1, m2]
  
  putStrLn "Bell State Results:"
  putStrLn $ "Measurements: " ++ show (measurements result)
  putStrLn $ "State Vector: " ++ show (stateVector result)
  putStrLn $ "Probabilities: " ++ show (probabilities result)`}
        className="my-6"
      />
      
      <p className="mb-4">
        When you run this code, you'll observe that the measurement results will always be either [Zero, Zero] or [One, One], with equal probability (approximately 50% each).
        This demonstrates the entanglement property: measuring the first qubit instantly determines the second qubit's state.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Creating All Four Bell States</h2>
      
      <p className="mb-4">
        We can modify our function to create all four Bell states:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`data BellState = PhiPlus | PhiMinus | PsiPlus | PsiMinus
  deriving (Show, Eq)

-- Function to create any of the four Bell states
createBellState :: BellState -> Circ (Qubit, Qubit)
createBellState bellType = do
  -- Create two qubits in the |0⟩ state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- For Psi states, flip the second qubit to |1⟩
  q2' <- case bellType of
    PhiPlus  -> pure q2
    PhiMinus -> pure q2
    PsiPlus  -> not q2
    PsiMinus -> not q2
  
  -- Apply Hadamard to the first qubit
  q1' <- hadamard q1
  
  -- Apply CNOT
  (q1'', q2'') <- controlled not q1' q2'
  
  -- For "minus" states, apply a Z gate to the first qubit
  q1''' <- case bellType of
    PhiPlus  -> pure q1''
    PhiMinus -> pauliZ q1''
    PsiPlus  -> pure q1''
    PsiMinus -> pauliZ q1''
  
  -- Return the entangled qubits
  pure (q1''', q2'')

-- Simulating each Bell state
simulateBellState :: BellState -> IO ()
simulateBellState bellType = do
  let result = simulateCircuit 2 $ do
        -- Create the specified Bell state
        (q1, q2) <- createBellState bellType
        
        -- Return the state without measurement for analysis
        pure [q1, q2]
  
  putStrLn $ "Bell State: " ++ show bellType
  putStrLn $ "State Vector: " ++ show (stateVector result)
  putStrLn $ "Probabilities: " ++ show (probabilities result)
  putStrLn ""

-- Main function to test all Bell states
main :: IO ()
main = do
  putStrLn "Simulating all Bell states:\n"
  simulateBellState PhiPlus
  simulateBellState PhiMinus
  simulateBellState PsiPlus
  simulateBellState PsiMinus`}
        className="my-6"
      />
      
      <InfoBox type="tip" title="Understanding the Output">
        <p>The state vector for |Φ⁺⟩ should show approximately equal amplitudes for |00⟩ and |11⟩, with zero amplitudes for |01⟩ and |10⟩. 
        The probabilities should show approximately 50% each for measuring |00⟩ and |11⟩.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Verifying Bell State Properties</h2>
      
      <p className="mb-4">
        Let's verify the entanglement property by measuring only one qubit and observing the effect on the second qubit:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Function to demonstrate entanglement
demonstrateEntanglement :: IO ()
demonstrateEntanglement = do
  -- Run multiple times to collect statistics
  results <- sequence $ replicate 10 $ simulateCircuit 2 $ do
    -- Create a Bell state
    (q1, q2) <- createBellState PhiPlus
    
    -- Measure only the first qubit
    (m1, q1') <- measure q1
    
    -- Now measure the second qubit
    (m2, _) <- measure q2
    
    pure [m1, m2]
  
  -- Print the results
  putStrLn "Entanglement Demonstration:"
  putStrLn "Each row shows measurements [first qubit, second qubit]"
  mapM_ (\\r -> putStrLn $ "  " ++ show (measurements r)) results`}
        className="my-6"
      />
      
      <p className="mb-4">
        You'll notice that when the first qubit is measured as Zero, the second qubit is always Zero, and when the first qubit is measured as One, the second qubit is always One. 
        This perfect correlation, despite the randomness of each individual measurement, is a hallmark of quantum entanglement.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Applications of Bell States</h2>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Quantum Teleportation</h3>
      
      <p className="mb-4">
        Bell states are the key resource in quantum teleportation, which allows the state of a qubit to be transmitted using only classical communication and shared entanglement:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Simple quantum teleportation circuit
teleport :: Qubit -> Circ Qubit
teleport qubitToTeleport = do
  -- Create an entangled pair (Bell state)
  (alice, bob) <- createBellState PhiPlus
  
  -- Alice entangles her qubit with her entangled qubit
  (qubitToTeleport', alice') <- controlled not qubitToTeleport alice
  qubitToTeleport'' <- hadamard qubitToTeleport'
  
  -- Alice measures her qubits
  (aliceMeasurement1, _) <- measure qubitToTeleport''
  (aliceMeasurement2, _) <- measure alice'
  
  -- Bob applies corrections based on Alice's measurements
  bobFinal <- case (aliceMeasurement1, aliceMeasurement2) of
    (Zero, Zero) -> pure bob
    (Zero, One)  -> pauliX bob
    (One, Zero)  -> pauliZ bob
    (One, One)   -> bob' <- pauliZ bob
                    pauliX bob'
  
  -- Return Bob's qubit, which now has the state of the original qubit
  pure bobFinal`}
        className="my-6"
      />
      
      <h3 className="text-xl font-bold mt-6 mb-3">Superdense Coding</h3>
      
      <p className="mb-4">
        Superdense coding is another protocol that uses Bell states to transmit two classical bits using just one qubit:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Superdense coding example
superDenseCoding :: (Bool, Bool) -> Circ (Bool, Bool)
superDenseCoding (bit1, bit2) = do
  -- Create a Bell state
  (alice, bob) <- createBellState PhiPlus
  
  -- Alice encodes two bits by applying operations to her qubit
  alice' <- case (bit1, bit2) of
    (False, False) -> pure alice               -- 00: Do nothing (|Φ⁺⟩)
    (False, True)  -> pauliX alice             -- 01: Apply X (|Ψ⁺⟩)
    (True, False)  -> pauliZ alice             -- 10: Apply Z (|Φ⁻⟩)
    (True, True)   -> alice' <- pauliX alice   -- 11: Apply XZ (|Ψ⁻⟩)
                      pauliZ alice'
  
  -- Alice sends her qubit to Bob (in this simulation, Bob now has both qubits)
  
  -- Bob decodes by first applying a CNOT
  (alice'', bob') <- controlled not alice' bob
  
  -- Then a Hadamard to Alice's qubit
  alice''' <- hadamard alice''
  
  -- Bob measures both qubits to recover the two bits
  (m1, _) <- measure alice'''
  (m2, _) <- measure bob'
  
  -- Convert measurement results to booleans
  let bit1' = case m1 of Zero -> False; One -> True
      bit2' = case m2 of Zero -> False; One -> True
  
  pure (bit1', bit2')`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Bell's Inequality and Non-locality</h2>
      
      <p className="mb-4">
        Bell states exhibit non-local correlations that cannot be explained by classical physics. These correlations were the subject of Bell's inequality, which provides a way to experimentally distinguish quantum mechanics from classical theories with hidden variables.
      </p>
      
      <p className="mb-4">
        In HaskQ, we can demonstrate these correlations by measuring entangled qubits in different bases:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Measure a qubit in different bases
measureInBasis :: Basis -> Qubit -> Circ (Measurement, Qubit)
measureInBasis basis qubit = case basis of
  -- Z basis (computational basis)
  ZBasis -> measure qubit
  
  -- X basis
  XBasis -> do
    qubit' <- hadamard qubit
    measure qubit'
  
  -- Y basis
  YBasis -> do
    qubit' <- rotateY (pi/2) qubit
    measure qubit'

-- Data type for measurement bases
data Basis = ZBasis | XBasis | YBasis
  deriving (Show, Eq)

-- Demonstrate Bell's inequality
bellTest :: IO ()
bellTest = do
  let -- Define measurement settings (bases)
      aliceBases = [ZBasis, XBasis]
      bobBases = [ZBasis, XBasis]
      
      -- Run the experiment
      runExperiment alice bob = do
        result <- simulateCircuit 2 $ do
          -- Create Bell state
          (q1, q2) <- createBellState PhiPlus
          
          -- Alice and Bob measure in their chosen bases
          (m1, _) <- measureInBasis alice q1
          (m2, _) <- measureInBasis bob q2
          
          pure [m1, m2]
        
        -- Return the results
        pure (measurements result)
  
  -- Collect results for all combinations of measurement settings
  results <- sequence [runExperiment a b | a <- aliceBases, b <- bobBases]
  
  -- Display results
  putStrLn "Bell Test Results:"
  sequence_ $ zipWith (\\(a, b) r -> do
    putStrLn $ "Alice: " ++ show a ++ ", Bob: " ++ show b
    putStrLn $ "  Result: " ++ show r
    ) [(a, b) | a <- aliceBases, b <- bobBases] results`}
        className="my-6"
      />
      
      <p className="mb-4">
        The results of this test will show correlations that violate Bell's inequality, demonstrating the non-local nature of quantum entanglement.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Visualizing Bell States</h2>
      
      <p className="mb-4">
        You can use HaskQ's visualization tools to generate circuit diagrams for Bell states:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import qualified HaskQ.Visualization as Viz

-- Visualize a Bell state circuit
visualizeBellState :: IO ()
visualizeBellState = do
  -- Define the circuit (without measurements)
  let circuit = do
        q1 <- createQubit Zero
        q2 <- createQubit Zero
        q1' <- hadamard q1
        (q1'', q2') <- controlled not q1' q2
        pure (q1'', q2')
  
  -- Generate an SVG circuit diagram
  Viz.drawCircuit "bell_state_circuit.svg" circuit`}
        className="my-6"
      />
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Conclusion</h2>
      
      <p className="mb-4">
        Bell states are fundamental building blocks in quantum information theory and demonstrate the uniquely quantum property of entanglement. In this tutorial, we've:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li>Learned what Bell states are and their mathematical representation</li>
        <li>Implemented circuits to create all four Bell states</li>
        <li>Verified their entanglement properties</li>
        <li>Explored applications like quantum teleportation and superdense coding</li>
        <li>Touched on Bell's inequality and the non-local nature of entanglement</li>
      </ul>
      
      <p className="mb-4">
        Understanding Bell states provides a foundation for more advanced quantum protocols and algorithms.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><Link href="/docs/core-concepts/entanglement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Entanglement</Link></li>
        <li><Link href="/docs/tutorials/teleportation" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Teleportation Tutorial</Link></li>
        <li><Link href="/docs/tutorials/superdense-coding" className="text-indigo-600 dark:text-indigo-400 hover:underline">Superdense Coding</Link></li>
      </ul>
    </div>
  );
} 