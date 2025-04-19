---
sidebar_position: 1
---

# Introduction to HaskQ

Welcome to HaskQ, a functional quantum programming toolkit that brings together the elegance of Haskell and the power of quantum computing.

## What is HaskQ?

HaskQ is a Haskell library for designing and simulating quantum circuits in a purely functional way. It leverages Haskell's powerful type system, particularly linear types, to enforce quantum physics laws at compile time.

The core benefits of HaskQ include:

- **Type-Safe Quantum Programming**: Linear types ensure no-cloning principle at compile time.
- **Purely Functional**: Quantum circuits are first-class values that can be composed using monadic combinators.
- **Simulation Capabilities**: Built-in matrix-based simulator for circuits up to 5 qubits.
- **Circuit Visualization**: Generate ASCII and JSON representations of quantum circuits.

## Key Components

HaskQ consists of several core packages:

### haskq-core

The foundation library implementing the quantum DSL with:

- Quantum types: `Qubit`, `Gate`, `Measurement`, etc.
- The `Circ` monad for circuit composition
- Basic quantum gates: Hadamard, Pauli-X/Y/Z, CNOT, etc.
- Measurement operations

### haskq-simulator

A matrix-based quantum simulator that:

- Simulates circuits with up to 5 qubits
- Uses state vectors and unitary matrices
- Visualizes circuits in ASCII or JSON format
- Calculates measurement probabilities

## Getting Started

To get started with HaskQ, check out the [Getting Started](getting-started) guide, which will walk you through installation and your first quantum circuit.

## A Simple Example

Here's a quick example of how to create a Bell state (a maximally entangled pair of qubits) using HaskQ:

```haskell
import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit

-- Create a Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')
```

In this example:
1. We request two fresh qubits in the $|0\rangle$ state.
2. Apply a Hadamard gate to the first qubit, creating a superposition.
3. Apply a CNOT gate with the first qubit as control and second as target.
4. Return the resulting entangled qubit pair.

When simulated, this produces the Bell state $|\Phi^+\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$. 