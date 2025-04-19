---
sidebar_position: 1
---

# Quantum Gates

In quantum computing, gates are the fundamental building blocks for quantum circuits, similar to how logic gates are used in classical computing. This guide explores the quantum gates implemented in HaskQ.

## Gate Types in HaskQ

HaskQ implements the standard gates used in quantum computing:

### Single-Qubit Gates

| Gate | Symbol | Matrix Representation | Description |
|------|--------|----------------------|-------------|
| Pauli-X | X | $\begin{pmatrix} 0 & 1 \\ 1 & 0 \end{pmatrix}$ | Quantum equivalent of the NOT gate, flips the state of a qubit |
| Pauli-Y | Y | $\begin{pmatrix} 0 & -i \\ i & 0 \end{pmatrix}$ | Rotation around the Y-axis of the Bloch sphere |
| Pauli-Z | Z | $\begin{pmatrix} 1 & 0 \\ 0 & -1 \end{pmatrix}$ | Phase flip operation |
| Hadamard | H | $\frac{1}{\sqrt{2}}\begin{pmatrix} 1 & 1 \\ 1 & -1 \end{pmatrix}$ | Creates superposition, transforms basis states to equal superpositions |
| Phase | S | $\begin{pmatrix} 1 & 0 \\ 0 & i \end{pmatrix}$ | Rotates the qubit state by 90° around the Z-axis |
| π/8 | T | $\begin{pmatrix} 1 & 0 \\ 0 & e^{i\pi/4} \end{pmatrix}$ | Rotates the qubit state by 45° around the Z-axis |
| Rotation | R(θ, φ) | Complex matrix | Arbitrary rotation around a specified axis |

### Multi-Qubit Gates

| Gate | Symbol | Description |
|------|--------|-------------|
| CNOT | • - ⊕ | Controlled-NOT gate, flips the target qubit if the control qubit is in state |1⟩ |
| CZ | • - Z | Controlled-Z gate, applies a Z gate to the target if the control is |1⟩ |
| SWAP | ⨯ - ⨯ | Swaps the states of two qubits |
| Toffoli | • - • - ⊕ | Controlled-controlled-NOT gate, also known as CCNOT |

## Using Gates in HaskQ

In HaskQ, quantum gates are applied to qubits using a simple and intuitive syntax:

```haskell
-- Single-qubit gate example
applyX :: Q Qubit %1-> Q Qubit
applyX q = gate X q

-- Creating a Bell state using Hadamard and CNOT
bellState :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
bellState (q1, q2) = do
  q1' <- gate H q1
  (q1'', q2') <- gate CNOT (q1', q2)
  return (q1'', q2')
```

### Gate Application Syntax

HaskQ provides several ways to apply gates to qubits:

```haskell
-- Basic gate application
q1' <- gate X q1

-- Using the infix operator
q1' <- q1 `apply` X

-- Applying to multiple qubits (for multi-qubit gates)
(q1', q2') <- gate CNOT (q1, q2)
```

## Parameterized Gates

For rotational gates that require parameters, HaskQ provides a clean syntax:

```haskell
-- Rotation around X axis by θ radians
q' <- gateP (RX theta) q

-- Phase rotation by φ radians
q' <- gateP (Phase phi) q
```

## Creating Gate Sequences

Gates can be composed to form sequences:

```haskell
-- Applying a sequence of gates
circuit :: Q Qubit %1-> Q Qubit
circuit q = do
  q1 <- gate H q
  q2 <- gate T q1
  q3 <- gate Z q2
  return q3
  
-- Equivalent shorthand using function composition
circuit' :: Q Qubit %1-> Q Qubit
circuit' = gate Z . gate T . gate H
```

## Custom Gates

HaskQ allows you to define custom gates by specifying their matrix representation:

```haskell
-- Define a custom single-qubit gate
mySqrtX :: Gate
mySqrtX = customGate "SqrtX" 
  (Matrix [[Complex 0.5 0.5, Complex 0.5 (-0.5)],
           [Complex 0.5 (-0.5), Complex 0.5 0.5]])

-- Define a custom two-qubit gate
myCustomTwoQubitGate :: Gate
myCustomTwoQubitGate = customGate "Custom2Q" myMatrix
  where
    myMatrix = -- 4x4 complex matrix
```

## Measurement

While not technically a gate, measurement is a crucial operation in quantum computing:

```haskell
-- Measure a qubit in the standard basis
measure :: Q Qubit %1-> (Bit, Qubit)
```

## Reversibility and Gate Inverses

All quantum gates (except measurement) are reversible. HaskQ provides an `inverse` function to get the inverse of a gate:

```haskell
-- Apply a gate and then its inverse
q' <- gate H q
q'' <- gate (inverse H) q'  -- q'' is equivalent to q
```

## Next Steps

Now that you understand quantum gates in HaskQ, you can:

1. Learn about [Circuit Composition](./circuit-composition.md)
2. Explore [Quantum Algorithms](../tutorials/algorithms.md)
3. Try implementing a simple algorithm in the [Playground](../playground.md) 