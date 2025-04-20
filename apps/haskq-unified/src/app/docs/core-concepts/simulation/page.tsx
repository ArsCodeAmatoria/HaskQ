export const metadata = {
  title: 'Quantum Simulation | HaskQ',
  description: 'Understand how the HaskQ simulator works and how to use it effectively'
};

export default function SimulationPage() {
  return (
    <article className="prose dark:prose-invert max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Quantum Simulation</h1>
      <p className="text-lg text-gray-600 dark:text-gray-300 mb-8">
        Understand how the HaskQ simulator works and how to use it effectively
      </p>
      
      <div className="markdown-content">
        <p>HaskQ includes a powerful quantum circuit simulator that allows you to test and analyze quantum algorithms without a physical quantum computer. This document explains how simulation works in HaskQ and how to use it effectively.</p>
        
        <h2>Getting Started with HaskQ Simulator</h2>
        
        <h3>Installation</h3>
        <p>To use the HaskQ simulator, first ensure you have the HaskQ package installed:</p>
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-bash">
{`# Install HaskQ from source
git clone https://github.com/haskq/haskq.git
cd haskq
cabal install

# Or using stack
stack install`}
          </code>
        </pre>
        
        <p>Once installed, you'll have access to the command-line simulator <code>haskq-sim</code> and the simulator library for use in your Haskell programs.</p>
        
        <h3>Quick Start Example</h3>
        <p>Create a file named <code>BellState.hs</code> with the following content:</p>
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
        pure [m1, m2]
  
  putStrLn $ "Measurement results: " ++ show (measurements result)`}
          </code>
        </pre>
        
        <p>Compile and run:</p>
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-bash">
{`ghc -o bell-state BellState.hs
./bell-state`}
          </code>
        </pre>
        
        <p>You should see either <code>[Zero, Zero]</code> or <code>[One, One]</code> as the output, demonstrating quantum entanglement.</p>
        
        <h2>The HaskQ Simulator</h2>
        
        <p>The HaskQ simulator is a state vector simulator that tracks the full quantum state through circuit execution. It's implemented in the <code>haskq-simulator</code> package, which provides:</p>
        
        <ol>
          <li>State vector representation of quantum states</li>
          <li>Matrix-based implementation of quantum gates</li>
          <li>Efficient simulation algorithms</li>
          <li>Visualization tools for circuits and results</li>
          <li>Command-line interface for quick simulations</li>
        </ol>
        
        <h2>Simulation Basics</h2>
        
        <h3>The State Vector Representation</h3>
        
        <p>In HaskQ, quantum states are represented by state vectors:</p>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- State vector for an n-qubit system
data StateVector = StateVector
  { numQubits :: Int
  , amplitudes :: Vector (Complex Double)
  }`}
          </code>
        </pre>
        
        <p>For an n-qubit system, the state vector contains 2^n complex amplitudes representing the probability amplitudes of each basis state.</p>
        
        <h3>Creating and Initializing States</h3>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- Initialize a simulation state for n qubits
initializeState :: Int -> SimState
initializeState n = SimState
  { stateVector = createStateVector n
  , qubits = [0..(n-1)]
  , measurements = []
  }`}
          </code>
        </pre>
        
        <p>By default, qubits are initialized in the |0⟩ state.</p>
        
        <h2>Running Simulations</h2>
        
        <p>HaskQ provides several ways to simulate quantum circuits:</p>
        
        <h3>Basic Circuit Simulation</h3>
        
        <pre className="bg-gray-100 dark:bg-gray-800 p-4 rounded-md overflow-x-auto">
          <code className="language-haskell">
{`-- Simulate a circuit and get the output
simulateCircuit :: Int -> Circ a %1-> CircuitOutput a
simulateCircuit numQubits circ = ...`}
          </code>
        </pre>
        
        <p>This function:</p>
        <ol>
          <li>Takes the number of qubits and a quantum circuit</li>
          <li>Simulates the circuit execution</li>
          <li>Returns the measurement results and the circuit's output value</li>
        </ol>
        
        <h2>Best Practices for Simulation</h2>
        
        <p>When using the HaskQ simulator, consider these best practices:</p>
        
        <ol>
          <li><strong>Start Small</strong>: Begin with small circuits to validate your quantum algorithm</li>
          <li><strong>Incremental Development</strong>: Add complexity incrementally, validating at each step</li>
          <li><strong>Use Visualizations</strong>: Visualize your circuits to verify their structure</li>
          <li><strong>Validate with Known Cases</strong>: Test your circuits with known inputs and expected outputs</li>
          <li><strong>Monitor Resource Usage</strong>: Keep track of memory usage for larger simulations</li>
        </ol>
        
        <h2>Next Steps</h2>
        
        <p>Now that you understand HaskQ's simulation capabilities, you can:</p>
        
        <ol>
          <li>Explore the <a href="/docs/tutorials/algorithms" className="text-indigo-600 dark:text-indigo-400 hover:underline">Quantum Algorithms</a> section to implement well-known quantum algorithms</li>
          <li>Learn about <a href="/docs/core-concepts/circuit-composition" className="text-indigo-600 dark:text-indigo-400 hover:underline">Circuit Optimization</a> techniques</li>
          <li>Try the <a href="/playground" className="text-indigo-600 dark:text-indigo-400 hover:underline">Playground</a> interface for interactive simulation</li>
        </ol>
        
        <p>HaskQ's simulator provides a powerful tool for developing and testing quantum algorithms without a physical quantum computer, allowing you to explore quantum computing concepts and develop intuition for quantum mechanics.</p>
      </div>
    </article>
  );
} 