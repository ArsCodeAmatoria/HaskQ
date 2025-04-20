export const metadata = {
  title: 'Introduction to HaskQ | HaskQ',
  description: 'An introduction to HaskQ, a functional quantum programming language'
};

export default function IntroPage() {
  return (
    <article className="prose dark:prose-invert max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Introduction to HaskQ</h1>
      <p className="text-lg text-gray-600 dark:text-gray-300 mb-8">
        Welcome to HaskQ, a functional quantum programming language
      </p>
      
      <div className="markdown-content">
        <p>HaskQ is a quantum programming language that combines the elegance of Haskell with the power of quantum computing. It provides a type-safe, purely functional approach to quantum circuit design and simulation.</p>
        
        <h2>Why HaskQ?</h2>
        
        <p>Quantum computing is a fundamentally different paradigm from classical computing, and traditional programming languages weren't designed with quantum mechanics in mind. HaskQ addresses this gap by:</p>
        
        <ul>
          <li><strong>Enforcing quantum mechanics laws at compile time</strong> through linear types</li>
          <li><strong>Providing clean, functional abstractions</strong> for quantum circuit design</li>
          <li><strong>Including a built-in simulator</strong> for testing and debugging quantum algorithms</li>
          <li><strong>Enabling seamless integration</strong> with classical Haskell code</li>
        </ul>
        
        <h2>Key Features</h2>
        
        <h3>Type-Safe Quantum Programming</h3>
        <p>HaskQ uses Haskell's linear types to enforce the no-cloning theorem at compile time. This means the compiler prevents you from writing programs that violate the laws of quantum mechanics.</p>
        
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
        <p>HaskQ circuits are first-class values that can be composed, passed as arguments, and returned from functions, enabling powerful abstractions.</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- Create a controlled version of any single-qubit operation
controlled :: (Qubit %1-> Circ Qubit) -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlled operation control target = ...

-- Now we can easily create controlled-H, controlled-X, etc.
controlledH :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
controlledH = controlled hadamard
`}
          </code>
        </pre>
        
        <h3>Built-in Simulation</h3>
        <p>HaskQ includes a state vector simulator that lets you run and test quantum algorithms on classical hardware.</p>
        
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
`}
          </code>
        </pre>
        
        <h2>Getting Started</h2>
        
        <p>Ready to dive into quantum programming with HaskQ? Check out our <a href="/docs/getting-started" className="text-indigo-600 dark:text-indigo-400 hover:underline">Getting Started</a> guide to install HaskQ and run your first quantum circuit.</p>
        
        <p>Or, if you prefer to jump straight into coding, try the <a href="/playground" className="text-indigo-600 dark:text-indigo-400 hover:underline">Playground</a> for an interactive HaskQ programming environment in your browser.</p>
        
        <h2>Next Steps</h2>
        
        <p>After getting familiar with the basics, you can explore:</p>
        
        <ul>
          <li><a href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Computing Basics</a> - If you're new to quantum computing</li>
          <li><a href="/docs/core-concepts/linear-types" className="text-indigo-600 dark:text-indigo-400 hover:underline">Linear Types in HaskQ</a> - To understand HaskQ's type system</li>
          <li><a href="/docs/tutorials/bell-states" className="text-indigo-600 dark:text-indigo-400 hover:underline">Bell States Tutorial</a> - For your first quantum algorithm</li>
        </ul>
      </div>
    </article>
  );
} 