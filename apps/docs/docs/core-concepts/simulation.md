---
sidebar_position: 5
---

# Quantum Simulation

HaskQ includes a powerful quantum circuit simulator that allows you to test and analyze quantum algorithms without a physical quantum computer. This document explains how simulation works in HaskQ and how to use it effectively.

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
-- Simulate a bell state creation and measurement
simulateBell :: CircuitOutput [Bit]
simulateBell = simulateCircuit 2 $ do
  [q1, q2] <- qinits [False, False]
  q1' <- gate H q1
  (q1'', q2') <- gate CNOT (q1', q2)
  m1 <- measureAndRelease q1''
  m2 <- measureAndRelease q2'
  return [m1, m2]
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
-- Calculate probability distribution of measurement outcomes
getProbabilities :: CircuitOutput [Bit] -> [(String, Double)]
getProbabilities output = ...

-- Example usage
analyzeResults :: CircuitOutput [Bit] -> IO ()
analyzeResults results = do
  let probs = getProbabilities results
  forM_ probs $ \(outcome, prob) ->
    putStrLn $ outcome ++ ": " ++ show (prob * 100) ++ "%"
```

## Visualizing Circuits

HaskQ provides tools to visualize quantum circuits:

```haskell
-- Convert a circuit to ASCII representation
circuitToAscii :: Circ a -> Text
circuitToAscii circuit = ...

-- Convert a circuit to JSON for web visualization
circuitToJson :: Circ a -> Value
circuitToJson circuit = ...
```

### Example Visualization

```
Bell State Circuit:

0: --H--●--
       |
1: -----X--
```

## The Command-Line Simulator

HaskQ includes a command-line tool for quick simulations:

```bash
haskq-sim --circuit bell --output ascii
```

Options:
- `--circuit` or `-c`: Circuit to simulate (bell, ghz, teleport, deutsch)
- `--qubits` or `-q`: Number of qubits (default: 2)
- `--output` or `-o`: Output format (ascii or json, default: ascii)
- `--file` or `-f`: Output file (optional, defaults to stdout)

## Advanced Simulation Features

### Parameterized Circuits

```haskell
-- Simulate a rotation circuit with different angles
simulateRotation :: Double -> CircuitOutput Bit
simulateRotation theta = simulateCircuit 1 $ do
  q <- qinit False
  q' <- gateP (RZ theta) q
  measureAndRelease q'
```

### Simulation with Controlled Randomness

For reproducible results, HaskQ allows setting a random seed:

```haskell
-- Simulate with a fixed random seed
simulateWithSeed :: Int -> Circ a %1-> CircuitOutput a
simulateWithSeed seed circuit = ...
```

### Batch Simulation

For statistical analysis:

```haskell
-- Run multiple simulations and collect statistics
batchSimulate :: Int -> Circ a %1-> [CircuitOutput a]
batchSimulate numRuns circuit = ...
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

## Next Steps

Now that you understand quantum simulation in HaskQ, you might want to explore:

1. Advanced quantum algorithms and simulations
2. Circuit optimization techniques to make simulations more efficient
3. The Playground Interface at http://localhost:3003 for interactive simulation 