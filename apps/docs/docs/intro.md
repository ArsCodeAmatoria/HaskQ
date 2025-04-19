---
sidebar_position: 1
slug: /
---

# Introduction to HaskQ

Welcome to HaskQ, a quantum computing framework built in Haskell that combines the elegance of functional programming with the power of quantum computing.

## What is HaskQ?

HaskQ is a purely functional quantum circuit design and simulation toolkit that leverages Haskell's strong type system to build safe, composable quantum programs. Key features include:

- **Type-safe quantum programming** using linear types to enforce the no-cloning theorem
- **Purely functional** approach to quantum circuit design
- **Built-in simulator** for testing and visualizing quantum circuits
- **Clean, composable API** for building complex quantum algorithms

## Why Haskell for Quantum Computing?

Haskell provides several advantages for quantum programming:

1. **Type Safety**: Haskell's advanced type system helps catch errors at compile time
2. **Pure Functions**: Purely functional approach aligns with the mathematical nature of quantum computing
3. **Linear Types**: GHC's LinearTypes extension provides a natural way to enforce quantum constraints
4. **Composability**: Monadic composition enables clean, readable quantum circuit construction

## Getting Started

To start using HaskQ, follow these steps:

1. [Install HaskQ](installation.md) on your system
2. Explore the [project structure](project-structure.md) to understand the components
3. Try the [getting started guide](getting-started.md) for your first quantum circuit

## A Quick Example

Here's a simple example of how to create a Bell state (an entangled pair of qubits) in HaskQ:

```haskell
import HaskQ.Prelude
import HaskQ.Circuit

-- Create a Bell state
bellState :: Circ (Q Qubit, Q Qubit)
bellState = do
  -- Initialize two qubits in the |0⟩ state
  q1 <- qinit False
  q2 <- qinit False
  
  -- Apply Hadamard to the first qubit
  q1' <- gate H q1
  
  -- Apply CNOT with q1' as control and q2 as target
  (q1'', q2') <- gate CNOT (q1', q2)
  
  -- Return the entangled pair
  return (q1'', q2')
```

## Core Concepts

To understand HaskQ, you'll want to explore these core concepts:

- [Quantum Computing Basics](core-concepts/quantum-computing-basics.md)
- [Linear Types](core-concepts/linear-types.md)
- [Circuit Composition](core-concepts/circuit-composition.md)
- [Quantum Gates](core-concepts/quantum-gates.md)
- [Measurement](core-concepts/measurement.md)
- [Simulation](core-concepts/simulation.md)

## Next Steps

Once you're familiar with the basics, you can dive deeper with:

- [Tutorials](tutorials/bell-states.md) for implementing common quantum algorithms
- [API Reference](category/api) for detailed documentation of HaskQ functions and types

Happy quantum programming! 