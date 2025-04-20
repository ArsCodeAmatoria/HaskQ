'use client';

import React from 'react';
import Link from 'next/link';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function CircuitCompositionPage() {
  return (
    <DocLayout 
      title="Circuit Composition" 
      description="Building complex quantum circuits through composition in HaskQ"
    >
      <div className="max-w-none prose dark:prose-invert">
        <p className="mb-6">
          Circuit composition is one of the most powerful features in HaskQ. It allows you to build complex quantum
          algorithms by combining smaller circuit components in a modular and reusable way. This document explores
          various techniques for composing quantum circuits in HaskQ.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">Fundamentals of Circuit Composition</h2>
        <p className="mb-4">
          In HaskQ, quantum circuits are values of type <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ a</code>, 
          where <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">a</code> is the return type of the circuit.
          The <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ</code> type is a monad, which enables
          sequential composition of quantum operations through the familiar <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">do</code> notation.
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Simple sequential composition
applyXThenH :: Qubit %1-> Circ Qubit
applyXThenH q = do
  q' <- pauliX q     -- Apply X gate
  q'' <- hadamard q' -- Apply H gate
  pure q''           -- Return the final qubit

-- Equivalent to
applyXThenH' :: Qubit %1-> Circ Qubit
applyXThenH' q = pauliX q >>= hadamard`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Building Reusable Circuit Components</h2>
        <p className="mb-4">
          One of the key advantages of HaskQ's functional approach is the ability to define reusable circuit components
          as Haskell functions. These components can be combined to build more complex circuits.
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Reusable component: Create a Bell state
bellState :: Circ (Qubit, Qubit)
bellState = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- controlled not q1' q2
  pure (q1'', q2')

-- Use the Bell state component in a teleportation circuit
teleport :: Qubit %1-> Circ Qubit
teleport qToTeleport = do
  -- Create entangled pair using our reusable component
  (q1, q2) <- bellState
  
  -- Rest of the teleportation circuit
  (qToTeleport', q1') <- controlled not qToTeleport q1
  qToTeleport'' <- hadamard qToTeleport'
  
  -- Measurements and corrections...
  -- ... (implementation omitted for brevity)
  
  pure q2 -- Return Bob's qubit containing the teleported state`}
          className="my-6"
        />

        <InfoBox type="tip" title="Component Design">
          <p>
            When designing circuit components, focus on making them composable by ensuring they:
          </p>
          <ul className="list-disc ml-6 mt-2">
            <li>Have clear input and output types</li>
            <li>Perform one coherent quantum operation or algorithm</li>
            <li>Handle linear types correctly</li>
            <li>Are well-documented with their purpose and requirements</li>
          </ul>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Patterns of Circuit Composition</h2>
        <p className="mb-4">
          HaskQ supports several patterns for composing quantum circuits, each suited to different scenarios:
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Sequential Composition</h3>
        <p className="mb-4">
          Apply quantum operations one after another, where each operation uses the output of the previous one:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Sequential composition using do notation
sequentialCircuit :: Qubit %1-> Circ Qubit
sequentialCircuit q = do
  q1 <- hadamard q      -- First operation
  q2 <- pauliT q1       -- Second operation
  q3 <- phaseShift 0.5 q2 -- Third operation
  pure q3               -- Return final state`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Parallel Composition</h3>
        <p className="mb-4">
          Apply different operations to different qubits independently:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Helper function to apply operations in parallel
parallel :: (Qubit %1-> Circ Qubit) -> (Qubit %1-> Circ Qubit) 
         -> (Qubit, Qubit) %1-> Circ (Qubit, Qubit)
parallel op1 op2 (q1, q2) = do
  q1' <- op1 q1
  q2' <- op2 q2
  pure (q1', q2')

-- Using parallel composition
parallelHX :: (Qubit, Qubit) %1-> Circ (Qubit, Qubit)
parallelHX = parallel hadamard pauliX

-- Example usage
twoQubitCircuit :: Circ (Qubit, Qubit)
twoQubitCircuit = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  (q1', q2') <- parallelHX (q1, q2)  -- Apply H to q1 and X to q2 in parallel
  pure (q1', q2')`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Controlled Composition</h3>
        <p className="mb-4">
          Apply an operation conditionally based on the state of a control qubit:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- General controlled operation
controlled :: (Qubit %1-> Circ Qubit) -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlled operation control target = do
  -- Implementation that applies operation to target if control is |1⟩
  -- ... (implementation details omitted)
  pure (control', target')

-- Creating controlled versions of basic gates
controlledH :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlledH = controlled hadamard

controlledZ :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlledZ = controlled pauliZ`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Repetition and Iteration</h3>
        <p className="mb-4">
          Apply an operation multiple times to the same qubit:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Apply a gate n times to a qubit
repeatGate :: (Qubit %1-> Circ Qubit) -> Int -> Qubit %1-> Circ Qubit
repeatGate gate 0 q = pure q
repeatGate gate n q = do
  q' <- gate q
  repeatGate gate (n-1) q'

-- Example: Repeat Hadamard 3 times (equivalent to applying H once)
threeHadamards :: Qubit %1-> Circ Qubit
threeHadamards = repeatGate hadamard 3`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Higher-Order Circuit Functions</h2>
        <p className="mb-4">
          HaskQ allows you to write higher-order functions that take circuits as inputs and return new circuits,
          enabling powerful abstractions:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Create the adjoint (conjugate transpose) of a gate
adjoint :: (Qubit %1-> Circ Qubit) -> (Qubit %1-> Circ Qubit)
adjoint gate = ...  -- Implementation depends on the gate

-- Apply a gate and then its adjoint (creates identity operation)
selfInverting :: (Qubit %1-> Circ Qubit) -> Qubit %1-> Circ Qubit
selfInverting gate q = do
  q' <- gate q
  adjoint gate q'
  
-- Execute two operations in sequence
sequence :: (Qubit %1-> Circ Qubit) -> (Qubit %1-> Circ Qubit) 
         -> (Qubit %1-> Circ Qubit)
sequence gate1 gate2 q = do
  q' <- gate1 q
  gate2 q'`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Handling Multiple Qubits</h2>
        <p className="mb-4">
          Working with multiple qubits requires careful handling of linear types. HaskQ provides patterns
          for safely manipulating multi-qubit systems:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Apply an operation to each qubit in a list
mapQubits :: (Qubit %1-> Circ Qubit) -> [Qubit] %1-> Circ [Qubit]
mapQubits _ [] = pure []
mapQubits op (q:qs) = do
  q' <- op q
  qs' <- mapQubits op qs
  pure (q':qs')

-- Apply Hadamard to all qubits
allHadamard :: [Qubit] %1-> Circ [Qubit]
allHadamard = mapQubits hadamard

-- Create a GHZ state (generalized Bell state)
ghzState :: Int -> Circ [Qubit]
ghzState n = do
  -- Create n qubits in |0⟩ state
  qs <- replicateM n (createQubit Zero)
  
  -- Put the first qubit in superposition
  case qs of
    [] -> pure []
    (q:rest) -> do
      q' <- hadamard q
      
      -- Apply CNOT gates with the first qubit as control
      rest' <- foldM 
        (\\(control, accum) target -> do
          (control', target') <- controlled not control target
          pure (control', accum ++ [target'])
        ) 
        (q', []) 
        rest
      
      pure (fst rest' : snd rest')`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Parameterized Circuits</h2>
        <p className="mb-4">
          Creating parameterized quantum circuits allows maximum flexibility and reusability:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Rotation around the X-axis by a variable angle
rotateX :: Double -> Qubit %1-> Circ Qubit
rotateX theta q = ...  -- Implementation applies Rx(theta)

-- Parameterized quantum Fourier transform
qft :: Int -> [Qubit] %1-> Circ [Qubit]
qft n qs = ...  -- Implementation of n-qubit QFT

-- Create a custom ansatz for variational algorithms
variationalAnsatz :: [Double] -> [Qubit] %1-> Circ [Qubit]
variationalAnsatz params qs = do
  -- Apply rotations based on parameters
  -- ... (implementation omitted)
  pure qs'`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Example: Quantum Phase Estimation</h2>
        <p className="mb-4">
          Let's see how circuit composition enables implementation of a complex algorithm like Quantum Phase Estimation:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Quantum Phase Estimation
quantumPhaseEstimation :: (Qubit %1-> Circ Qubit)  -- Unitary U with eigenvalue e^{2πiφ}
                      -> Int                    -- Precision bits
                      -> Qubit %1-> Circ (Double, Qubit)
quantumPhaseEstimation unitary precision eigenstate = do
  -- Create precision register qubits in |0⟩
  register <- replicateM precision (createQubit Zero)
  
  -- Step 1: Apply Hadamard to all register qubits
  register' <- mapQubits hadamard register
  
  -- Step 2: Apply controlled-U^(2^j) operations
  (register'', eigenstate') <- applyControlledPowers unitary register' eigenstate
  
  -- Step 3: Apply inverse QFT to register
  register''' <- inverseQFT register''
  
  -- Step 4: Measure register to get phase estimate
  (measurements, _) <- measureAll register'''
  
  -- Convert binary measurements to phase estimate
  let phaseEstimate = binaryToPhase measurements
  
  pure (phaseEstimate, eigenstate')
  
-- Helper functions
applyControlledPowers :: (Qubit %1-> Circ Qubit) -> [Qubit] %1-> Qubit %1-> Circ ([Qubit], Qubit)
applyControlledPowers unitary registers eigenstate = ...

inverseQFT :: [Qubit] %1-> Circ [Qubit]
inverseQFT qs = ...

measureAll :: [Qubit] %1-> Circ ([Bit], [Qubit])
measureAll qs = ...

binaryToPhase :: [Bit] -> Double
binaryToPhase bits = ...`}
          className="my-6"
        />

        <InfoBox type="note" title="Composability is Key">
          <p>
            Notice how quantum phase estimation is built from smaller components like controlled operations,
            the inverse QFT, and measurement routines. This modularity makes the code easier to understand,
            test, and maintain.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Best Practices</h2>
        <p className="mb-4">
          Follow these practices to make your circuit compositions more effective:
        </p>

        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Leverage type signatures</strong>: Clear types make composition easier and prevent errors</li>
          <li><strong>Build a component library</strong>: Create reusable components for common patterns</li>
          <li><strong>Use higher-order functions</strong>: Abstract over circuit operations</li>
          <li><strong>Handle linear types consistently</strong>: Maintain a clear protocol for qubit ownership</li>
          <li><strong>Document component interfaces</strong>: Describe the purpose, inputs, and outputs</li>
          <li><strong>Test components in isolation</strong>: Verify each circuit component independently</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Circuit Visualization and Debugging</h2>
        <p className="mb-4">
          HaskQ provides tools to visualize circuit compositions, aiding in understanding and debugging:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`import HaskQ.Visualization

-- Generate a circuit diagram
circuitDiagram :: IO ()
circuitDiagram = drawCircuit "bell-state.svg" bellState

-- Print circuit statistics
circuitStats :: IO ()
circuitStats = do
  let stats = analyzeCircuit bellState
  putStrLn $ "Gate count: " ++ show (gateCount stats)
  putStrLn $ "Circuit depth: " ++ show (circuitDepth stats)
  putStrLn $ "T-gate count: " ++ show (tGateCount stats)`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><Link href="/docs/core-concepts/linear-types" className="text-indigo-600 dark:text-indigo-400 hover:underline">Linear Types in HaskQ</Link></li>
          <li><Link href="/docs/core-concepts/quantum-gates" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Gates</Link></li>
          <li><Link href="/docs/core-concepts/quantum-algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Algorithms</Link></li>
        </ul>
      </div>
    </DocLayout>
  );
} 