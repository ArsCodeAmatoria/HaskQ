---
sidebar_position: 5
---

# Quantum Simulation

HaskQ includes a powerful quantum circuit simulator that allows you to test and analyze quantum algorithms without a physical quantum computer. This document explains how simulation works in HaskQ and how to use it effectively.

## Getting Started with HaskQ Simulator

### Installation

To use the HaskQ simulator, first ensure you have the HaskQ package installed:

```bash
# Install HaskQ from source
git clone https://github.com/haskq/haskq.git
cd haskq
cabal install

# Or using stack
stack install
```

Once installed, you'll have access to the command-line simulator `haskq-sim` and the simulator library for use in your Haskell programs.

### Quick Start Example

Create a file named `BellState.hs` with the following content:

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)

main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn $ "Measurement results: " ++ show (measurements result)
```

Compile and run:

```bash
ghc -o bell-state BellState.hs
./bell-state
```

You should see either `[Zero, Zero]` or `[One, One]` as the output, demonstrating quantum entanglement.

## The HaskQ Simulator

The HaskQ simulator is a state vector simulator that tracks the full quantum state through circuit execution. It's implemented in the `haskq-simulator` package, which provides:

1. State vector representation of quantum states
2. Matrix-based implementation of quantum gates
3. Efficient simulation algorithms
4. Visualization tools for circuits and results
5. Command-line interface for quick simulations

## Simulation Basics

### The State Vector Representation

In HaskQ, quantum states are represented by state vectors:

```haskell
-- State vector for an n-qubit system
data StateVector = StateVector
  { numQubits :: Int
  , amplitudes :: Vector (Complex Double)
  }
```

For an n-qubit system, the state vector contains 2^n complex amplitudes representing the probability amplitudes of each basis state.

### Creating and Initializing States

```haskell
-- Initialize a simulation state for n qubits
initializeState :: Int -> SimState
initializeState n = SimState
  { stateVector = createStateVector n
  , qubits = [0..(n-1)]
  , measurements = []
  }
```

By default, qubits are initialized in the |0⟩ state.

## Running Simulations

HaskQ provides several ways to simulate quantum circuits:

### Basic Circuit Simulation

```haskell
-- Simulate a circuit and get the output
simulateCircuit :: Int -> Circ a %1-> CircuitOutput a
simulateCircuit numQubits circ = ...
```

This function:
1. Takes the number of qubits and a quantum circuit
2. Simulates the circuit execution
3. Returns the measurement results and the circuit's output value

### Example Usage

```haskell
-- Import the required modules
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import qualified Prelude as P

-- Simulate a bell state creation and measurement
simulateBell :: IO ()
simulateBell = do
  let result = simulateCircuit 2 $ do
        -- Create bell state
        (q1, q2) <- bellState
        -- Measure both qubits
        (m1, q1') <- measure q1
        (m2, q2') <- measure q2
        pure [m1, m2]
        
  P.putStrLn $ "Measurements: " ++ P.show (measurements result)
  
main :: IO ()
main = simulateBell
```

## Simulation Results and Analysis

The `CircuitOutput` type contains the results of a simulation:

```haskell
data CircuitOutput a = CircuitOutput
  { measurements :: [Measurement]
  , result :: a
  }
```

### Analyzing Measurement Probabilities

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import qualified Prelude as P
import Data.List (group, sort)

-- Calculate probability distribution of measurement outcomes
getProbabilities :: [Measurement] -> [(String, Double)]
getProbabilities results = 
  let total = P.fromIntegral $ P.length results
      grouped = group $ sort $ P.map P.show results
      counts = P.map (\g -> (P.head g, P.fromIntegral $ P.length g)) grouped
  in P.map (\(outcome, count) -> (outcome, count / total)) counts

-- Example: Run multiple simulations and analyze results
analyzeResults :: IO ()
analyzeResults = do
  let numRuns = 1000
      results = P.map measurements $ P.replicate numRuns $ 
                simulateCircuit 2 $ do
                  (q1, q2) <- bellState
                  (m1, _) <- measure q1
                  (m2, _) <- measure q2
                  pure [m1, m2]
                  
      -- Flatten all measurement results
      allMeasurements = P.concat results
      probs = getProbabilities allMeasurements
      
  P.forM_ probs $ \(outcome, prob) ->
    P.putStrLn $ outcome ++ ": " ++ P.show (prob * 100) ++ "%"
```

## Visualizing Circuits

HaskQ provides tools to visualize quantum circuits:

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii)
import qualified Data.Text.IO as TIO

-- Define a circuit
myCircuit :: Circ [Measurement]
myCircuit = do
  (q1, q2) <- bellState
  (m1, q1') <- measure q1
  (m2, q2') <- measure q2
  pure [m1, m2]

-- Visualize and display the circuit
visualizeMyCircuit :: IO ()
visualizeMyCircuit = do
  let vis = visualizeCircuit myCircuit
  TIO.putStrLn $ circuitToAscii vis
```

### Example Visualization

```
Bell State Circuit:

0: --H--●--M--
       |
1: -----X--M--
```

## The Command-Line Simulator

HaskQ includes command-line tools for quick simulations:

### Basic Usage

```bash
# Simulate a Bell state circuit and output in ASCII format
haskq-sim --circuit bell --output ascii

# Simulate a GHZ state with 3 qubits
haskq-sim --circuit ghz --qubits 3 --output ascii

# Save the circuit visualization to a file
haskq-sim --circuit teleport --output json --file teleport.json
```

### Available Options

- `--circuit` or `-c`: Circuit to simulate (bell, ghz, teleport, deutsch)
- `--qubits` or `-q`: Number of qubits (default: 2)
- `--output` or `-o`: Output format (ascii or json, default: ascii)
- `--file` or `-f`: Output file (optional, defaults to stdout)

### Error Correction Examples

HaskQ includes specialized executable for error correction simulations:

```bash
# Run the bit-flip error correction example
haskq-error-correction bit-flip

# Run the phase-flip error correction example
haskq-error-correction phase-flip

# Run the Shor code (combined error correction) example
haskq-error-correction shor

# Export the visualization to a file
haskq-error-correction bit-flip output.txt
```

### General Examples

You can also run the general examples:

```bash
# Run the example program with the Bell state
haskq-example bell

# Run the GHZ state example and save visualization
haskq-example ghz output.json
```

## Advanced Simulation Features

### Parameterized Circuits

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import qualified Prelude as P

-- Simulate a rotation circuit with different angles
simulateRotation :: Double -> IO ()
simulateRotation theta = do
  let result = simulateCircuit 1 $ do
        q <- qubit
        q' <- hadamard q
        -- Apply a parameterized rotation
        q'' <- phase theta q'
        (m, _) <- measure q''
        pure m
  
  P.putStrLn $ "Measurement result: " ++ P.show (measurements result)
  
main :: IO ()
main = do
  P.putStrLn "Simulating with θ = 0"
  simulateRotation 0
  P.putStrLn "Simulating with θ = π/4"
  simulateRotation (pi/4)
  P.putStrLn "Simulating with θ = π/2"
  simulateRotation (pi/2)
```

### Batch Simulation

For statistical analysis:

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import qualified Prelude as P

-- Run multiple simulations of the same circuit
batchSimulate :: Int -> Circ a -> IO [CircuitOutput a]
batchSimulate numRuns circuit = do
  P.return $ P.map (\_ -> simulateCircuit (requiredQubits circuit) circuit) [1..numRuns]

-- Example usage
main :: IO ()
main = do
  results <- batchSimulate 100 $ do
    (q1, q2) <- bellState
    (m1, _) <- measure q1
    (m2, _) <- measure q2
    pure [m1, m2]
    
  let zeros = P.length $ P.filter (\r -> measurements r == [Zero, Zero]) results
  let ones = P.length $ P.filter (\r -> measurements r == [One, One]) results
  
  P.putStrLn $ "Results: |00⟩: " ++ P.show zeros ++ ", |11⟩: " ++ P.show ones
```

## Creating Custom Circuits

### Combining Basic Components

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii)
import qualified Data.Text.IO as TIO
import qualified Prelude as P

-- Define a custom quantum circuit
myCustomCircuit :: Circ [Measurement]
myCustomCircuit = do
  -- Create three qubits
  q1 <- qubit
  q2 <- qubit
  q3 <- qubit
  
  -- Apply gates to create a custom state
  q1' <- hadamard q1
  q2' <- hadamard q2
  (q1'', q3') <- cnot q1' q3
  (q2'', q3'') <- cnot q2' q3'
  
  -- Measure the results
  (m1, _) <- measure q1''
  (m2, _) <- measure q2''
  (m3, _) <- measure q3''
  
  pure [m1, m2, m3]

-- Run the simulation
main :: IO ()
main = do
  -- Visualize the circuit
  let vis = visualizeCircuit myCustomCircuit
  TIO.putStrLn $ circuitToAscii vis
  
  -- Run the simulation
  let result = simulateCircuit 3 myCustomCircuit
  P.putStrLn $ "Measurements: " ++ P.show (measurements result)
```

## Simulation Efficiency

The HaskQ simulator employs several techniques to maximize efficiency:

1. **Sparse State Vector**: For states with limited superposition
2. **Gate Fusion**: Combines sequential single-qubit gates
3. **Deferred Measurement**: Optimizes circuits by deferring measurements when possible

## Simulation Limitations

It's important to understand the limitations of classical simulation:

1. **Exponential Scaling**: Simulation memory requirements grow exponentially with qubit count
2. **Numerical Precision**: Floating-point errors can accumulate in large circuits
3. **Special Quantum Algorithms**: Some quantum algorithms (like Shor's) are designed to be hard to simulate classically

## Best Practices for Simulation

1. **Start Small**: Begin with small circuits (2-5 qubits) for testing
2. **Incremental Development**: Add complexity incrementally
3. **Verify Expected Results**: Use known quantum algorithms to verify simulation correctness
4. **Use Visualizations**: Visualize circuits to understand their structure
5. **Profile Resource Usage**: Monitor memory usage for large simulations

## Debugging Tips

1. **Visualize Before Running**: Always visualize your circuit before simulation to verify its structure
2. **Check Intermediate States**: Insert measurements to check intermediate states during development
3. **Compare with Known Results**: Validate against analytically computed results for small circuits
4. **Use Pure States**: Start with pure states (|0⟩ or |1⟩) to simplify debugging
5. **Isolate Components**: Test circuit components individually before combining them

## Next Steps

Now that you understand how to use the quantum simulator in HaskQ, you might want to explore:

1. [Advanced quantum algorithms](../tutorials/algorithms.md) and simulations
2. [Circuit optimization techniques](../tutorials/optimization.md) to make simulations more efficient
3. [Error correction](../tutorials/error-correction.md) to protect against quantum noise
4. The Playground Interface at http://localhost:3003 for interactive simulation 