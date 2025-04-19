---
sidebar_position: 2
---

# Getting Started with HaskQ

This guide will help you get started with HaskQ, from installation to creating your first quantum circuit.

## Installation

See the [Installation](installation.md) page for detailed instructions on installing HaskQ.

## Your First Quantum Circuit

Let's create a simple quantum circuit using HaskQ—the classic Bell state generator.

```haskell
import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit (simulateCircuit)

-- Create a Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \[q1, q2] -> do
  q1' <- hadamard q1        -- Apply Hadamard to first qubit
  (q1'', q2') <- cnot q1' q2 -- Apply CNOT with first qubit as control
  pure (q1'', q2')          -- Return the entangled qubits

-- Create and measure the Bell state
main :: IO ()
main = do
  let result = simulateCircuit 2 $ do
        (q1, q2) <- bellState
        (m1, _) <- measure q1
        (m2, _) <- measure q2
        pure [m1, m2]
  
  putStrLn $ "Measurement results: " ++ show (measurements result)
```

Save this to a file (e.g., `BellState.hs`) and run it:

```bash
ghc -o bell-state BellState.hs
./bell-state
```

Each time you run this program, you should get either `[Zero, Zero]` or `[One, One]` with equal probability, demonstrating quantum entanglement.

## Using the Command-line Simulator

HaskQ comes with a built-in simulator that you can use from the command line:

```bash
haskq-sim --circuit bell --output ascii
```

This will output an ASCII representation of the Bell state circuit:

```
Circuit with 2 qubits:

0: --H--●--
       |
1: -----X--
```

## Understanding the Core Concepts

HaskQ is built on a few key concepts:

1. **Linear Types**: Ensures qubits are used exactly once, enforcing quantum mechanics' no-cloning theorem.
2. **The `Circ` Monad**: Allows composing quantum operations in a purely functional way.
3. **Gates as Functions**: Quantum gates are represented as functions from qubits to circuits producing qubits.
4. **Measurement**: Collapses quantum states to classical outcomes with appropriate probabilities.

Here's how these concepts appear in our code:

```haskell
-- Linear types: q1 is consumed by hadamard, which returns a new qubit q1'
q1' <- hadamard q1

-- Monad: operations are sequenced using do-notation
bellState = withQubits 2 $ \[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')
  
-- Measurement: returns both a result and a post-measurement qubit
(m1, q1') <- measure q1
```

## Next Steps

Now that you understand the basics, you can:

1. Learn more about [quantum computing basics](core-concepts/quantum-computing-basics.md)
2. Explore [linear types](core-concepts/linear-types.md) in HaskQ
3. Try the [Bell state tutorial](tutorials/bell-states.md) for a deeper dive
4. Experiment with the [online playground](http://localhost:3003) 