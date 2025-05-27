import React from 'react';

export default function IntroContent() {
  return (
    <article className="prose dark:prose-invert max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Introduction to HaskQ</h1>
      <p className="text-lg text-gray-600 dark:text-gray-300 mb-8">
        Welcome to HaskQ, a modern functional quantum programming language that combines Haskell&apos;s type safety with quantum computing
      </p>
      
      <div className="markdown-content">
        <p>HaskQ is a quantum programming language that combines the elegance and safety of Haskell with the power of quantum computing. It provides a type-safe, purely functional approach to quantum circuit design and simulation, empowering developers to build reliable quantum algorithms with confidence.</p>
        
        <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800 my-6">
          <h3 className="text-lg font-semibold mb-2">Why Choose HaskQ?</h3>
          <p className="mb-2">HaskQ blends the best of both worlds: Haskell&apos;s powerful type system and functional purity with quantum computing&apos;s revolutionary computational paradigm.</p>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mt-4">
            <div>
              <h4 className="font-semibold">For Haskell Programmers</h4>
              <p>A natural extension of Haskell with quantum capabilities, using familiar monadic composition and linear types.</p>
            </div>
            <div>
              <h4 className="font-semibold">For Quantum Enthusiasts</h4>
              <p>A rigorous, mathematically sound approach to quantum programming with compile-time guarantees about quantum mechanics.</p>
            </div>
          </div>
        </div>
        
        <h2>Why HaskQ?</h2>
        
        <p>Quantum computing is a fundamentally different paradigm from classical computing, and traditional programming languages weren&apos;t designed with quantum mechanics in mind. HaskQ addresses this gap by:</p>
        
        <ul>
          <li><strong>Enforcing quantum mechanics laws at compile time</strong> through linear types, preventing mistakes like cloning qubits</li>
          <li><strong>Providing clean, functional abstractions</strong> for quantum circuit design, promoting composable and reusable code</li>
          <li><strong>Including a built-in simulator</strong> for testing and debugging quantum algorithms before running on real hardware</li>
          <li><strong>Enabling seamless integration</strong> with classical Haskell code, allowing hybrid quantum-classical algorithms</li>
          <li><strong>Ensuring correctness</strong> through Haskell&apos;s strong type system, reducing runtime errors</li>
        </ul>
        
        <h2>Key Features</h2>
        
        <h3>Type-Safe Quantum Programming</h3>
        <p>HaskQ uses Haskell&apos;s linear types to enforce the no-cloning theorem at compile time. This means the compiler prevents you from writing programs that violate the laws of quantum mechanics, catching errors before they become runtime bugs.</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- This will compile
validCircuit :: Circ (Qubit, Qubit)
validCircuit = do
  q <- createQubit Zero
  q' <- hadamard q
  (q'', q''') <- controlled not q' (createQubit Zero)
  pure (q'', q''')

-- This will NOT compile - attempts to use q twice
invalidCircuit :: Circ (Qubit, Qubit)
invalidCircuit = do
  q <- createQubit Zero
  q' <- hadamard q
  q'' <- pauliX q'  -- This uses q'
  q''' <- hadamard q'  -- Error! q' was already consumed
  pure (q'', q''')
`}
          </code>
        </pre>
        
        <h3>Functional Composition</h3>
        <p>HaskQ circuits are first-class values that can be composed, passed as arguments, and returned from functions, enabling powerful abstractions and modular circuit design.</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- Create a controlled version of any single-qubit operation
controlled :: (Qubit %1-> Circ Qubit) -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlled operation control target = ...

-- Now we can easily create controlled-H, controlled-X, etc.
controlledH :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlledH = controlled hadamard

-- Build larger quantum circuits by composition
bellStateCircuit :: Circ (Qubit, Qubit)
bellStateCircuit = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q1' <- hadamard q1
  controlledX q1' q2
`}
          </code>
        </pre>
        
        <h3>Built-in Simulation</h3>
        <p>HaskQ includes a state vector simulator that lets you run and test quantum algorithms on classical hardware, providing insights into algorithm behavior without quantum hardware.</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)

main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure (m1, m2)
  
  putStrLn $ "Measurement: " ++ show result
  -- Will output either (Zero, Zero) or (One, One) with equal probability
  
  -- We can also examine the full quantum state:
  putStrLn $ "State vector: " ++ show (stateVector result)
  putStrLn $ "Probabilities: " ++ show (probabilities result)
`}
          </code>
        </pre>
        
        <h3>Visualization Tools</h3>
        <p>HaskQ provides tools to visualize quantum circuits and states, making it easier to understand and debug quantum algorithms.</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`import HaskQ.Visualization

main :: IO ()
main = do
  -- Generate a circuit diagram in various formats
  drawCircuit "bell-state.svg" bellStateCircuit
  
  -- Visualize the quantum state
  let state = runSimulation bellStateCircuit
  drawStateVector "bell-state-amplitudes.png" state
`}
          </code>
        </pre>
        
        <h2>HaskQ vs. Other Quantum Languages</h2>
        
        <div className="overflow-x-auto my-6">
          <table className="min-w-full bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700">
            <thead>
              <tr>
                <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Feature</th>
                <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">HaskQ</th>
                <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Qiskit (Python)</th>
                <th className="py-2 px-4 border-b border-gray-200 dark:border-gray-700 text-left">Q# (Microsoft)</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Paradigm</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Purely functional</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Object-oriented</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Imperative</td>
              </tr>
              <tr>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Type Safety</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Strong, static, linear types</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Dynamic typing</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Strong, static types</td>
              </tr>
              <tr>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Quantum Laws</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Enforced at compile time</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Runtime checks</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Partial compile-time checks</td>
              </tr>
              <tr>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Hardware Support</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Simulator, API adapters</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">IBM Quantum hardware</td>
                <td className="py-2 px-4 border-b border-gray-200 dark:border-gray-700">Azure Quantum</td>
              </tr>
            </tbody>
          </table>
        </div>
        
        <h2>Getting Started</h2>
        
        <p>Ready to dive into quantum programming with HaskQ? Check out our getting started guide to install HaskQ and run your first quantum circuit.</p>
        
        <p>Or, if you prefer to jump straight into coding, try the Playground for an interactive HaskQ programming environment in your browser.</p>
        
        <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-6 border border-green-100 dark:border-green-800 my-6">
          <h3 className="text-lg font-semibold mb-2">Quick Example: Bell State</h3>
          <p className="mb-4">Create your first quantum entanglement with just a few lines of code:</p>
          <pre className="bg-white dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
            <code className="language-haskell">
{`import HaskQ.Prelude

bellState :: Circ (Qubit, Qubit)
bellState = do
  -- Create two qubits in |0⟩ state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Put first qubit in superposition
  q1' <- hadamard q1
  
  -- Entangle the qubits
  (q1'', q2') <- controlled not q1' q2
  
  pure (q1'', q2')
`}
            </code>
          </pre>
        </div>
        
        <h2>Next Steps</h2>
        
        <p>After getting familiar with the basics, you can explore:</p>
        
        <ul>
          <li>Quantum Computing Basics - If you&apos;re new to quantum computing</li>
          <li>Linear Types in HaskQ - To understand HaskQ&apos;s type system</li>
          <li>Bell States Tutorial - For your first quantum algorithm</li>
          <li>Circuit Composition - Learn how to build complex quantum circuits</li>
        </ul>
        
        <p className="italic mt-8 text-gray-600 dark:text-gray-400">
          Join the quantum revolution with HaskQ and build the quantum applications of tomorrow with the reliability of functional programming.
        </p>
      </div>
    </article>
  );
} 