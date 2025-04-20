---
sidebar_position: 3
---

# Grover's Search Algorithm

Grover's algorithm provides a quadratic speedup for searching unstructured databases. It can find an element in an unsorted list of N items with approximately O(√N) operations, compared to the classical O(N) approach.

## Overview

Grover's algorithm solves the following problem:

- Given a black-box oracle function that identifies a unique solution in a search space
- Find that solution with fewer queries than would be required classically

The quantum speedup comes from amplifying the probability of measuring the correct answer through a technique called "amplitude amplification."

## Algorithm Structure

Grover's algorithm consists of the following steps:

1. **Initialization**: Start with qubits in the |0⟩ state
2. **Superposition**: Apply Hadamard gates to create an equal superposition of all possible states
3. **Repeated iterations** (approximately π/4 · √N times):
   - Apply the oracle (mark the solution)
   - Apply the diffusion operator (amplify the amplitude of the marked state)
4. **Measurement**: Measure all qubits to obtain the solution

## HaskQ Implementation

HaskQ provides a built-in implementation of Grover's algorithm through the `groverSearch` function:

```haskell
groverSearch :: Int -> Int -> OracleFunction -> Circ [Qubit]
```

Where:
- The first parameter is the number of qubits
- The second parameter is the number of Grover iterations
- The third parameter is the oracle function that marks the target state(s)

### Oracle Function

The oracle function is the most important part of Grover's algorithm. It needs to perform a phase flip (-1) on the state(s) that represent the solution(s) and leave all other states unchanged.

In HaskQ, an oracle is defined as:

```haskell
type OracleFunction = [Qubit] %1-> Circ [Qubit]
```

### Example: Searching for |101⟩

Here's an example of an oracle that searches for the state |101⟩:

```haskell
-- Oracle function for searching |101⟩ state
search101Oracle :: [Qubit] %1-> Circ [Qubit]
search101Oracle qubits
  | length qubits < 3 = pure qubits  -- Not enough qubits
  | otherwise = do
      -- Apply X gates to the qubits that should be |0⟩ in the target state
      -- For |101⟩, apply X to the second qubit (index 1)
      qubits' <- applyX 1 qubits
      
      -- Apply a controlled-Z gate using all qubits as control
      -- When all qubits are |1⟩, it will apply a phase flip
      qubits'' <- controlledPhase qubits'
      
      -- Apply X gates again to restore the original state
      qubits''' <- applyX 1 qubits''
      
      pure qubits'''
```

## Using Grover's Algorithm

### Calculating Optimal Iterations

For a search space of size N = 2^n, the optimal number of Grover iterations is approximately:

```haskell
optimalIterations :: Int -> Int
optimalIterations n =
  let N = 2 ^ n  -- Size of search space
  in floor $ (pi / 4) * sqrt (fromIntegral N)
```

### Complete Example

Here's a complete example of using Grover's algorithm to search for the state |101⟩:

```haskell
-- Define the search circuit using Grover's algorithm
searchCircuit = do
  -- Run Grover's search with our oracle
  result <- groverSearch numQubits optIters search101Oracle
  
  -- Measure all qubits
  (measurements, _) <- measureAll result
  
  pure measurements

-- Run the simulation
result = simulateCircuit numQubits searchCircuit
```

## Running the Example

HaskQ includes a complete example of Grover's algorithm that you can run:

```bash
cabal run haskq-grover -- 3 1
```

Where:
- The first argument (3) is the number of qubits
- The second argument (1) is the number of iterations (optional)

## Understanding the Results

When you run the Grover example, you'll see:

1. A visualization of the circuit
2. The measurement results
3. Statistics from multiple trials

In a perfect quantum computer, Grover's algorithm would find the target state with certainty after the optimal number of iterations. In simulation, you'll see a high probability (but not 100%) of finding the target state due to the discretization of the number of iterations.

## Trade-offs and Limitations

- Grover's algorithm provides a quadratic speedup, not an exponential one like Shor's algorithm
- It requires a quantum oracle that can mark solutions
- The algorithm must know in advance how many solutions exist
- For multiple solutions, the optimal number of iterations changes

## Next Steps

Try modifying the example to:
1. Search for a different state
2. Use a different number of qubits
3. Change the number of iterations to see how it affects the success probability
4. Implement an oracle for a real search problem 