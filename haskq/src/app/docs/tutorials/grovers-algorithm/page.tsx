'use client';

import React from 'react';
import Link from 'next/link';
import { CodeBlock } from '@/components/CodeBlock';
import { InfoBox } from '@/components/InfoBox';

export default function GroversAlgorithmPage() {
  return (
    <div className="max-w-none prose dark:prose-invert">
      <h1 className="text-3xl font-bold mb-6">Grover's Algorithm Tutorial</h1>
      
      <p className="text-lg mb-6">
        Grover's algorithm is a quantum algorithm that provides a quadratic speedup for searching an unsorted database.
        It's one of the most important quantum algorithms with practical applications for a wide range of search problems.
      </p>
      
      <InfoBox type="note">
        <p>This tutorial assumes familiarity with basic quantum computing concepts. If you're new to these topics, 
        we recommend starting with the <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</Link> guide.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">The Search Problem</h2>
      
      <p className="mb-4">
        Consider the problem of searching an unsorted database with N items to find a specific item that satisfies a given condition. 
        Classically, we would need to check each item one by one until we find the answer, requiring O(N) operations on average.
      </p>
      
      <p className="mb-4">
        Grover's algorithm, on the other hand, can solve this problem in just O(√N) operations, providing a 
        quadratic speedup over the best possible classical algorithm.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">How Grover's Algorithm Works</h2>
      
      <p className="mb-4">
        Grover's algorithm works by amplifying the amplitude of the target state(s) through a series of operations:
      </p>
      
      <ol className="list-decimal ml-8 space-y-2 mb-6">
        <li>Initialize qubits in a uniform superposition of all possible states</li>
        <li>Apply the oracle function that marks the target state</li>
        <li>Apply the diffusion operator (also known as the Grover operator)</li>
        <li>Repeat steps 2 and 3 approximately √N times</li>
        <li>Measure the qubits to obtain the target state with high probability</li>
      </ol>
      
      <h3 className="text-xl font-bold mt-6 mb-3">The Oracle Function</h3>
      
      <p className="mb-4">
        The oracle function is a quantum black box that recognizes the target state and marks it by 
        flipping its phase. For a state |x⟩, the oracle performs:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <p className="font-mono">
          O|x⟩ = (-1)<sup>f(x)</sup>|x⟩
        </p>
      </div>
      
      <p className="mb-4">
        where f(x) = 1 if x is the target state, and f(x) = 0 otherwise.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">The Diffusion Operator</h3>
      
      <p className="mb-4">
        The diffusion operator performs a reflection about the average amplitude, amplifying the 
        amplitude of the marked state while reducing the amplitude of all other states. It can be 
        expressed as:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg text-center my-6">
        <p className="font-mono">
          D = 2|ψ⟩⟨ψ| - I
        </p>
      </div>
      
      <p className="mb-4">
        where |ψ⟩ is the uniform superposition of all states, and I is the identity operator.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Implementing Grover's Algorithm in HaskQ</h2>
      
      <p className="mb-4">
        Let's implement Grover's algorithm in HaskQ, starting with the necessary imports:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`import HaskQ.Prelude
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit
import Control.Monad (replicateM)`}
        className="my-6"
      />
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 1: Prepare the Oracle Function</h3>
      
      <p className="mb-4">
        First, we'll define the oracle function that marks our target state. In this example,
        we'll use a simple oracle that marks the state |101⟩ (or decimal 5):
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Oracle function that marks the state |101⟩
oracle :: [Qubit] -> Circ [Qubit]
oracle qubits = do
  -- This oracle marks the state |101⟩
  -- Apply Z gate when the state is |101⟩
  -- We can implement this using an ancilla qubit and multi-controlled NOT
  
  -- Create an ancilla qubit in |1⟩ state
  ancilla <- createQubit One
  
  -- Apply X gates to all qubits that should be 0 in our target state
  -- For |101⟩, we apply X gate to the middle qubit
  qubits' <- case qubits of
    [q0, q1, q2] -> do
      q1' <- pauliX q1  -- Invert the middle qubit
      pure [q0, q1', q2]
    _ -> pure qubits  -- Handle other cases gracefully
  
  -- Apply multi-controlled NOT with the ancilla as target
  (qubits'', ancilla') <- case qubits' of
    [q0, q1, q2] -> do
      -- 3-qubit Toffoli gate (CCNOT)
      (q0', q1', ancilla'') <- toffoli q0 q1 ancilla
      (q2', ancilla''') <- controlled not q2 ancilla''
      pure ([q0', q1', q2'], ancilla''')
    _ -> pure (qubits', ancilla)
  
  -- Undo the X gates
  qubits''' <- case qubits'' of
    [q0, q1, q2] -> do
      q1' <- pauliX q1  -- Invert the middle qubit again
      pure [q0, q1', q2]
    _ -> pure qubits''
  
  -- The phase kickback effect puts a negative phase on the |101⟩ state
  
  -- Measure and discard the ancilla (in an actual quantum computer, we'd reset it)
  (_, _) <- measure ancilla'
  
  pure qubits'''

-- Helper function for Toffoli gate (CCNOT)
toffoli :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)
toffoli control1 control2 target = do
  -- Implementation of Toffoli gate using basic gates
  target' <- hadamard target
  (control2', target'') <- controlled phaseShift control2 target'
  (control1', target''') <- controlled phaseShiftDagger control1 target''
  (control2'', target'''') <- controlled phaseShift control2' target'''
  (control1'', target''''') <- controlled phaseShiftDagger control1' target''''
  target'''''' <- hadamard target'''''
  pure (control1'', control2'', target'''''')`}
        className="my-6"
      />
      
      <p className="mb-4">
        In a real-world scenario, the oracle would be a specialized function based on your search problem.
      </p>
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 2: Implement the Diffusion Operator</h3>
      
      <p className="mb-4">
        Next, we'll implement the diffusion operator that amplifies the amplitude of the marked state:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Diffusion operator (Grover operator)
diffusion :: [Qubit] -> Circ [Qubit]
diffusion qubits = do
  -- Apply Hadamard to all qubits
  qubits' <- mapM hadamard qubits
  
  -- Apply X to all qubits
  qubits'' <- mapM pauliX qubits'
  
  -- Apply multi-controlled-Z 
  qubits''' <- applyControlledZ qubits''
  
  -- Apply X to all qubits
  qubits'''' <- mapM pauliX qubits'''
  
  -- Apply Hadamard to all qubits
  qubits''''' <- mapM hadamard qubits''''
  
  pure qubits'''''

-- Helper function to apply controlled-Z
applyControlledZ :: [Qubit] -> Circ [Qubit]
applyControlledZ [] = pure []
applyControlledZ [q] = do
  q' <- pauliZ q  -- Apply Z to a single qubit
  pure [q']
applyControlledZ qubits = do
  -- Create an ancilla qubit in |0⟩ state
  ancilla <- createQubit Zero
  
  -- Apply H to ancilla
  ancilla' <- hadamard ancilla
  
  -- For each qubit, apply a CNOT with the ancilla as target
  (qubits', ancilla'') <- foldM 
    (\\(qs, anc) q -> do
      (q', anc') <- controlled not q anc
      pure (qs ++ [q'], anc')
    ) 
    ([], ancilla') 
    qubits
  
  -- Apply H to ancilla
  ancilla''' <- hadamard ancilla''
  
  -- Measure and discard the ancilla
  (_, _) <- measure ancilla'''
  
  pure qubits'`}
        className="my-6"
      />
      
      <h3 className="text-xl font-bold mt-6 mb-3">Step 3: The Complete Grover's Algorithm</h3>
      
      <p className="mb-4">
        Now, let's put it all together and implement the complete Grover's algorithm:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Main Grover's algorithm function
groverSearch :: Int -> Int -> Circ [Qubit]
groverSearch numQubits iterations = do
  -- Create qubits all in |0⟩ state
  qubits <- replicateM numQubits (createQubit Zero)
  
  -- Apply Hadamard to all qubits to create a uniform superposition
  qubits' <- mapM hadamard qubits
  
  -- Apply Grover iteration multiple times
  qubits'' <- foldM 
    (\\qs _ -> do
      -- Apply oracle
      qs' <- oracle qs
      -- Apply diffusion
      diffusion qs'
    )
    qubits'
    [1..iterations]
  
  pure qubits''

-- Calculate optimal number of iterations
optimalIterations :: Int -> Int
optimalIterations n = floor (pi/4 * sqrt (fromIntegral (2^n)))

-- Run Grover's algorithm
runGrover :: IO ()
runGrover = do
  let numQubits = 3  -- Using 3 qubits
      targetState = 5  -- Searching for |101⟩ (decimal 5)
      iterations = optimalIterations numQubits
      
      result = simulateCircuit numQubits $ do
        -- Run Grover's algorithm
        qubits <- groverSearch numQubits iterations
        
        -- Measure all qubits
        measurements <- mapM (\\q -> do
                           (m, _) <- measure q
                           pure m) qubits
        
        pure measurements
  
  putStrLn $ "Searching for state: |" ++ showBinaryString targetState numQubits ++ "⟩ (decimal " ++ show targetState ++ ")"
  putStrLn $ "Number of qubits: " ++ show numQubits
  putStrLn $ "Total search space: " ++ show (2^numQubits) ++ " states"
  putStrLn $ "Optimal iterations: " ++ show iterations
  putStrLn $ "Measurements: " ++ show (measurements result)
  putStrLn $ "Measured state: " ++ showBinaryString (binaryToDecimal (measurements result)) numQubits
  putStrLn $ "Probability of success: " ++ show (probOfState targetState result) ++ "%"

-- Utility functions
showBinaryString :: Int -> Int -> String
showBinaryString n len = map (\\i -> if testBit n (len - 1 - i) then '1' else '0') [0..len-1]

binaryToDecimal :: [Measurement] -> Int
binaryToDecimal = foldl (\\acc m -> acc * 2 + (case m of Zero -> 0; One -> 1)) 0

probOfState :: Int -> SimulationResult -> Double
probOfState state result = 
  let probs = probabilities result
      stateStr = showBinaryString state (length probs)
      stateIndex = binaryToDecimal (map (\\c -> if c == '1' then One else Zero) stateStr)
  in 100 * (probs !! stateIndex)`}
        className="my-6"
      />
      
      <InfoBox type="tip" title="Number of Iterations">
        <p>The optimal number of iterations is approximately π/4 * √N, where N is the size of the search space. 
        Too few iterations won't sufficiently amplify the target state, while too many will cause the amplitude to 
        decrease.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Visualizing Grover's Algorithm</h2>
      
      <p className="mb-4">
        We can visualize how the amplitudes change during Grover's iterations:
      </p>
      
      <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg my-6">
        <pre className="whitespace-pre">
{`Initial uniform superposition:
|000⟩: 0.125    |001⟩: 0.125    |010⟩: 0.125    |011⟩: 0.125
|100⟩: 0.125    |101⟩: 0.125    |110⟩: 0.125    |111⟩: 0.125

After 1 iteration:
|000⟩: 0.078    |001⟩: 0.078    |010⟩: 0.078    |011⟩: 0.078
|100⟩: 0.078    |101⟩: 0.469    |110⟩: 0.078    |111⟩: 0.078

After 2 iterations:
|000⟩: 0.031    |001⟩: 0.031    |010⟩: 0.031    |011⟩: 0.031
|100⟩: 0.031    |101⟩: 0.781    |110⟩: 0.031    |111⟩: 0.031`}
        </pre>
      </div>
      
      <p className="mb-4">
        Notice how the amplitude of our target state |101⟩ increases with each iteration, while the 
        amplitudes of all other states decrease.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Multiple Target States</h2>
      
      <p className="mb-4">
        Grover's algorithm can also search for multiple target states. In this case, the optimal number of 
        iterations changes to approximately π/4 * √(N/M), where M is the number of target states.
      </p>
      
      <p className="mb-4">
        Let's modify our oracle to mark multiple states:
      </p>
      
      <CodeBlock 
        language="haskell" 
        code={`-- Oracle that marks multiple states (|101⟩ and |110⟩)
multipleTargetOracle :: [Qubit] -> Circ [Qubit]
multipleTargetOracle qubits = do
  -- We'll implement this by applying individual oracles for each target state
  qubits' <- oracle1 qubits  -- Mark |101⟩
  qubits'' <- oracle2 qubits'  -- Mark |110⟩
  pure qubits''

-- Oracle for |101⟩
oracle1 :: [Qubit] -> Circ [Qubit]
oracle1 = oracle  -- Reuse our existing oracle

-- Oracle for |110⟩
oracle2 :: [Qubit] -> Circ [Qubit]
oracle2 qubits = do
  -- Similar implementation as oracle1, but for the state |110⟩
  -- Create an ancilla qubit in |1⟩ state
  ancilla <- createQubit One
  
  -- Apply X gates to qubits that should be 0 in our target state
  -- For |110⟩, we apply X gate to the last qubit
  qubits' <- case qubits of
    [q0, q1, q2] -> do
      q2' <- pauliX q2  -- Invert the last qubit
      pure [q0, q1, q2']
    _ -> pure qubits
  
  -- Apply multi-controlled NOT
  (qubits'', ancilla') <- case qubits' of
    [q0, q1, q2] -> do
      (q0', q1', ancilla'') <- toffoli q0 q1 ancilla
      (q2', ancilla''') <- controlled not q2 ancilla''
      pure ([q0', q1', q2'], ancilla''')
    _ -> pure (qubits', ancilla)
  
  -- Undo the X gates
  qubits''' <- case qubits'' of
    [q0, q1, q2] -> do
      q2' <- pauliX q2  -- Invert the last qubit again
      pure [q0, q1, q2']
    _ -> pure qubits''
  
  -- Measure and discard the ancilla
  (_, _) <- measure ancilla'
  
  pure qubits'''`}
        className="my-6"
      />
      
      <p className="mb-4">
        When searching for multiple targets, the algorithm will find one of the target states with 
        high probability, but we won't know in advance which one.
      </p>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Applications of Grover's Algorithm</h2>
      
      <p className="mb-4">
        Grover's algorithm has many practical applications:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><strong>Database Search</strong>: Finding specific records in unsorted databases</li>
        <li><strong>Cryptanalysis</strong>: Breaking symmetric cryptosystems like AES with a quadratic speedup</li>
        <li><strong>Collision Finding</strong>: Finding collisions in hash functions</li>
        <li><strong>Solving NP-complete Problems</strong>: Quadratic speedup for problems like Boolean satisfiability</li>
        <li><strong>Element Distinctness</strong>: Determining if a list contains any duplicate entries</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Limitations</h2>
      
      <p className="mb-4">
        While Grover's algorithm provides a quadratic speedup, it's not as dramatic as the exponential 
        speedup offered by Shor's algorithm. However, it's more broadly applicable and can be used 
        as a subroutine in other quantum algorithms.
      </p>
      
      <InfoBox type="warning">
        <p>The quadratic speedup provided by Grover's algorithm is proven to be optimal. No quantum 
        algorithm can solve an unstructured search problem with fewer than O(√N) operations.</p>
      </InfoBox>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Conclusion</h2>
      
      <p className="mb-4">
        Grover's algorithm demonstrates one of the fundamental advantages of quantum computing: 
        the ability to search unstructured data quadratically faster than any classical algorithm. 
        While it doesn't provide an exponential speedup like some other quantum algorithms, its 
        broad applicability makes it one of the most important quantum algorithms.
      </p>
      
      <p className="mb-4">
        In this tutorial, we've:
      </p>
      
      <ul className="list-disc ml-8 space-y-2">
        <li>Learned how Grover's algorithm works</li>
        <li>Implemented the oracle and diffusion operators</li>
        <li>Created a complete implementation of Grover's search</li>
        <li>Explored variations for multiple target states</li>
        <li>Discussed practical applications and limitations</li>
      </ul>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Further Reading</h2>
      
      <ul className="list-disc ml-8 space-y-2">
        <li><Link href="/docs/core-concepts/quantum-algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Algorithms Overview</Link></li>
        <li><Link href="/docs/tutorials/qft" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Fourier Transform Tutorial</Link></li>
        <li><Link href="/docs/tutorials/shor-algorithm" className="text-indigo-600 dark:text-indigo-400 hover:underline">Shor's Algorithm Tutorial</Link></li>
      </ul>
    </div>
  );
} 