---
sidebar_position: 4
---

# Quantum Fourier Transform

The Quantum Fourier Transform (QFT) is one of the most important quantum algorithms and serves as a building block for many other quantum algorithms, including Shor's factoring algorithm and quantum phase estimation.

## Overview

The Quantum Fourier Transform is the quantum analogue of the classical Discrete Fourier Transform, but with exponential speedup due to quantum parallelism. It transforms a quantum state from the computational basis to the Fourier basis.

Mathematically, the QFT transforms a quantum state according to:

$$
|j\rangle \mapsto \frac{1}{\sqrt{2^n}} \sum_{k=0}^{2^n-1} e^{2\pi i jk/2^n} |k\rangle
$$

Where $n$ is the number of qubits, $j$ is the input state, and $k$ ranges through all possible computational basis states.

## Algorithm Structure

The QFT consists of the following steps:

1. Apply Hadamard gates to each qubit
2. Apply controlled rotation gates between pairs of qubits
3. Swap the qubits to produce the correct output order (optional)

## HaskQ Implementation

HaskQ provides built-in implementations of the QFT through the following functions:

```haskell
qft :: [Qubit] %1-> Circ [Qubit]
inverseQft :: [Qubit] %1-> Circ [Qubit]
rotationGate :: Int -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
```

Where:
- `qft` applies the Quantum Fourier Transform to a list of qubits
- `inverseQft` applies the inverse QFT
- `rotationGate` is a helper function that applies the phase rotation by angle 2π/2^k

## Understanding the QFT Circuit

The QFT circuit for n qubits consists of:

1. A Hadamard gate on each qubit
2. Controlled rotation gates (CR₂, CR₄, CR₈, etc.) between qubits
3. A qubit swap operation (in most implementations)

For example, a 3-qubit QFT circuit looks like:

```
q₀: ──H──•────•─────╳─
         │    │     │ 
q₁: ─────R₂───•─────╳─
              │       
q₂: ──────────R₄──────
```

Where:
- H is the Hadamard gate
- R₂ is a rotation by π/2
- R₄ is a rotation by π/4
- ╳ is a swap operation

## Example: Using QFT in HaskQ

Here's a simple example of applying the QFT to a register of qubits:

```haskell
-- Create a circuit that applies QFT
qftCircuit = do
  -- Initialize qubits
  qubits <- withQubits numQubits (\qs -> pure qs)
  
  -- Create a superposition state (optional)
  qubits' <- applyHadamardToFirst qubits
  
  -- Apply QFT
  transformedQubits <- qft qubits'
  
  -- Measure all qubits
  (measurements, _) <- measureAll transformedQubits
  
  pure measurements
```

### Inverse QFT

The inverse QFT is used in many algorithms, including phase estimation. Here's how to use it:

```haskell
inverseQftCircuit = do
  -- Initialize qubits in superposition
  qubits <- withQubits numQubits (\qs -> pure qs)
  qubits' <- applyHadamardToAll qubits
  
  -- Apply QFT
  qftResult <- qft qubits'
  
  -- Apply inverse QFT (should return to original state)
  originalState <- inverseQft qftResult
  
  -- Measure all qubits
  (measurements, _) <- measureAll originalState
  
  pure measurements
```

## Running the QFT Example

HaskQ includes a complete example of the QFT that you can run:

```bash
cabal run haskq-qft -- 3
```

Where:
- The argument (3) is the number of qubits to use

## Applications of QFT

The QFT is used in many important quantum algorithms:

1. **Shor's Algorithm**: Used for factoring large numbers with exponential speedup
2. **Quantum Phase Estimation**: Estimating the phase of an eigenvalue of a unitary operator
3. **Quantum Counting**: Counting the number of solutions to a search problem

## Understanding QFT Results

When you run the QFT on a basis state |j⟩, the result is a superposition of all basis states with specific phase relationships. When measured, each basis state is equally likely to be observed.

What makes the QFT powerful is not the probabilities of the outputs, but the phases (complex amplitudes) of the resulting state, which encode the Fourier transform of the input.

## Implementation Details

The HaskQ implementation uses rotations to create the appropriate phase shifts:

```haskell
-- Rotation gate with angle θ = 2π/2^k
rotationGate :: Int -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
rotationGate k control target = do
  let theta = pi * 2 / (2 ^ k)
  target' <- phase theta target
  pure (control, target')
```

This creates the necessary conditional phase rotations between qubits that form the heart of the QFT.

## Next Steps

Try exploring:
1. Different input states to the QFT
2. Applying the QFT to entangled states
3. Implementing phase estimation using the QFT
4. Building a simple version of Shor's algorithm 