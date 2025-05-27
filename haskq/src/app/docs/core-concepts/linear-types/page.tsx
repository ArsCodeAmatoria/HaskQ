'use client';

import React from 'react';
import Link from 'next/link';
import { DocLayout } from '@/components/DocLayout';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function LinearTypesPage() {
  return (
    <DocLayout 
      title="Linear Types in HaskQ" 
      description="Understanding how linear types enforce quantum mechanics laws at compile time"
    >
      <div className="max-w-none prose dark:prose-invert">
        <p className="mb-6">
          Linear types are a powerful feature of HaskQ that enforce quantum mechanics laws at compile time, 
          ensuring your quantum programs are physically realizable. This document explores how linear types work
          and why they're essential for quantum programming.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">The No-Cloning Theorem</h2>
        <p className="mb-4">
          The no-cloning theorem is a fundamental principle of quantum mechanics: it is impossible to create an 
          identical copy of an arbitrary unknown quantum state. Unlike classical bits, qubits cannot be duplicated.
        </p>
        
        <p className="mb-4">
          In programming terms, this means that once a qubit is used in a quantum operation, the original qubit 
          is consumed and cannot be used again. This is fundamentally different from classical computing, where 
          variables can be used multiple times.
        </p>

        <h2 className="text-2xl font-bold mt-8 mb-4">What Are Linear Types?</h2>
        <p className="mb-4">
          Linear types are a type system feature that enforces a resource usage discipline: values with linear types
          must be used exactly once—no more, no less. They provide a perfect match for quantum computing's constraints.
        </p>
        
        <p className="mb-4">
          In HaskQ, linear types are denoted with the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">%1-{'>'}</code> 
          arrow in function types, indicating that the input parameter must be used exactly once in the function.
        </p>
        
        <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-sm my-6">
          <h3 className="font-bold mb-2">Key Properties of Linear Types</h3>
          <ul className="list-disc ml-6 space-y-2">
            <li><strong>Exactly-once usage</strong>: Linear values must be used exactly once</li>
            <li><strong>No duplication</strong>: Linear values cannot be copied or cloned</li>
            <li><strong>No discarding</strong>: Linear values cannot be ignored or discarded</li>
            <li><strong>Compile-time enforcement</strong>: Usage violations are caught during compilation</li>
          </ul>
        </div>

        <h2 className="text-2xl font-bold mt-8 mb-4">Linear Types in HaskQ</h2>
        <p className="mb-4">
          In HaskQ, qubits have linear types, which means they must be consumed exactly once. This directly enforces
          the no-cloning theorem at the type level, preventing many quantum programming errors at compile time.
        </p>
        
        <CodeBlock 
          language="haskell" 
          code={`-- The type of a quantum bit is linear
data Qubit  -- abstract type

-- The type of a quantum circuit returning a value of type a
newtype Circ a = ...

-- Quantum gates are linear functions, consuming input qubits
hadamard :: Qubit %1-> Circ Qubit
pauliX :: Qubit %1-> Circ Qubit
pauliZ :: Qubit %1-> Circ Qubit

-- Two-qubit gates consume both input qubits
cnot :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
swap :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Valid vs. Invalid Quantum Circuits</h3>
        <p className="mb-4">
          With linear types, the compiler will accept quantum circuits where each qubit is used exactly once,
          and reject circuits that try to use qubits multiple times or discard them:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- ✓ VALID: Each qubit is used exactly once
validCircuit :: Circ (Qubit, Qubit)
validCircuit = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')

-- ✗ INVALID: Attempts to use q1' twice
invalidCircuit1 :: Circ (Qubit, Qubit)
invalidCircuit1 = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  q1'' <- pauliX q1'  -- Uses q1'
  q1''' <- pauliZ q1'  -- ERROR! q1' already consumed
  pure (q1''', q2)

-- ✗ INVALID: Discards a qubit
invalidCircuit2 :: Circ Qubit
invalidCircuit2 = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero  -- Creates q2
  q1' <- hadamard q1
  pure q1'  -- ERROR! Discards q2 without using it`}
          className="my-6"
        />

        <InfoBox type="note" title="GHC Extension">
          <p>
            Linear types in HaskQ are enabled by the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">LinearTypes</code> extension in GHC,
            which was introduced in GHC 9.0. This extension adds the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">%1-{'>'}</code> function arrow for linear functions.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Consuming and Creating Qubits</h2>
        <p className="mb-4">
          In HaskQ, qubits can only be created through specific operations and must eventually be consumed.
          This mirrors the physical reality of quantum computing where qubits must be initialized, operated on,
          and eventually measured.
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Creating qubits
createQubit :: Bit -> Circ Qubit  -- Creates a new qubit in state |0⟩ or |1⟩

-- Measuring qubits (consumes the qubit)
measure :: Qubit %1-> Circ (Bit, Qubit)
-- Returns the measurement result and a post-measurement qubit`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Handling Linear Types with Monads</h2>
        <p className="mb-4">
          HaskQ combines linear types with monadic programming to make working with quantum circuits intuitive.
          The <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Circ</code> monad manages qubit lifetimes
          and enforces linear usage.
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Monadic operations allow sequencing of quantum operations
circuitWithMonad :: Circ Bit
circuitWithMonad = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Sequence of operations, each consuming and producing qubits
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  
  -- Measurement consumes qubits
  (result, _) <- measure q1''
  (_, _) <- measure q2'  -- Discard measurement result
  
  pure result  -- Return only the first measurement result`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Advanced Linear Type Patterns</h2>
        <p className="mb-4">
          HaskQ provides several patterns for working with linear types effectively:
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Multiplicity Polymorphism</h3>
        <p className="mb-4">
          Functions can be polymorphic in their usage requirements, allowing code reuse between linear and non-linear contexts:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- A function polymorphic in multiplicity
map :: (a %m-> b) -> [a] %m-> [b]

-- Can be used with both linear and non-linear functions
mapLinear :: [Qubit] %1-> [Qubit]
mapLinear = map hadamard

mapNonLinear :: [Int] -> [Int]
mapNonLinear = map (+1)`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Linear Let and Case Expressions</h3>
        <p className="mb-4">
          HaskQ allows binding and pattern matching on linear values, ensuring they're properly consumed:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`linearCase :: Qubit %1-> Bit -> Circ Qubit
linearCase q b = case b of
  Zero -> pauliX q    -- q is consumed here if b is Zero
  One  -> hadamard q  -- q is consumed here if b is One

linearLet :: Qubit %1-> Circ (Qubit, Qubit)
linearLet q = do
  q' <- hadamard q
  -- Binding using let
  let (q1, q2) = splitQubit q'  -- Hypothetical function
  -- Both q1 and q2 must be consumed
  q1' <- pauliX q1
  q2' <- pauliZ q2
  pure (q1', q2')`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Limitations and Workarounds</h2>
        <p className="mb-4">
          While linear types are powerful, they can sometimes make certain patterns challenging. HaskQ provides solutions:
        </p>

        <h3 className="text-xl font-bold mt-6 mb-3">Unrestricted Types</h3>
        <p className="mb-4">
          Classical (non-quantum) data can be wrapped in the <code className="bg-gray-100 dark:bg-gray-800 px-1 py-0.5 rounded">Unrestricted</code> type
          to allow unrestricted usage:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Classical data can be freely duplicated and discarded
classicalData :: Qubit %1-> Unrestricted Int -> Circ Qubit
classicalData q (Unrestricted n) = do
  -- Can use n multiple times
  let iterations = n * 2
  applyGateNTimes q iterations`}
          className="my-6"
        />

        <h3 className="text-xl font-bold mt-6 mb-3">Consuming Without Using</h3>
        <p className="mb-4">
          When you need to discard a qubit safely, you can measure and ignore the result:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Safely discard a qubit by measuring it
discardQubit :: Qubit %1-> Circ ()
discardQubit q = do
  (_, _) <- measure q
  pure ()  -- Qubit consumed, measurement result ignored`}
          className="my-6"
        />

        <h2 className="text-2xl font-bold mt-8 mb-4">Benefits of Linear Types</h2>
        <p className="mb-4">
          Linear types provide several advantages for quantum programming:
        </p>

        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><strong>Physical correctness</strong>: Programs respect quantum mechanics laws</li>
          <li><strong>Early error detection</strong>: Many errors are caught at compile time, not runtime</li>
          <li><strong>Resource tracking</strong>: The type system helps track quantum resources</li>
          <li><strong>Reasoning aid</strong>: Makes it easier to reason about quantum programs</li>
          <li><strong>Optimization opportunities</strong>: Compiler can optimize knowing resources are used exactly once</li>
        </ul>

        <h2 className="text-2xl font-bold mt-8 mb-4">Practical Examples</h2>
        <p className="mb-4">
          Let's examine a complete quantum teleportation circuit that demonstrates linear types in action:
        </p>

        <CodeBlock 
          language="haskell" 
          code={`-- Quantum teleportation circuit
teleport :: Qubit %1-> Circ Qubit
teleport qubitToTeleport = do
  -- Create an entangled Bell pair
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  
  -- Perform Bell measurement between qubit to teleport and q1''
  (qubitToTeleport', q1''') <- cnot qubitToTeleport q1''
  qubitToTeleport'' <- hadamard qubitToTeleport'
  
  -- Measure both qubits (consuming them)
  (m1, _) <- measure qubitToTeleport''
  (m2, _) <- measure q1'''
  
  -- Apply corrections to q2' based on measurement results
  q2'' <- case (m1, m2) of
    (Zero, Zero) -> pure q2'
    (Zero, One)  -> pauliX q2'
    (One, Zero)  -> pauliZ q2'
    (One, One)   -> do
                     q2'' <- pauliX q2'
                     pauliZ q2''
  
  -- Return the teleported qubit
  pure q2''`}
          className="my-6"
        />

        <p className="mb-4">
          Notice how the original qubit is consumed by the teleportation process, and a new qubit containing the quantum
          state is returned—perfectly aligning with the physics of quantum teleportation.
        </p>

        <InfoBox type="tip" title="Working with Linear Types">
          <p>
            When working with linear types in HaskQ, it helps to think of qubits as physical resources that must be 
            explicitly managed. Each quantum operation consumes qubits and produces new ones, forming a chain of 
            transformations until measurement.
          </p>
        </InfoBox>

        <h2 className="text-2xl font-bold mt-8 mb-4">Related Topics</h2>
        <ul className="list-disc ml-6 mb-6 space-y-2">
          <li><Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link></li>
          <li><Link href="/docs/core-concepts/circuit-composition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Circuit Composition</Link></li>
          <li><Link href="/docs/core-concepts/measurement" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Measurement</Link></li>
        </ul>
      </div>
    </DocLayout>
  );
} 