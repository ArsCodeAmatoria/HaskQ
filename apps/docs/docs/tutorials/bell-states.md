---
sidebar_position: 1
---

# Creating Bell States

In this tutorial, we'll learn how to create and work with Bell states, which are fundamental examples of quantum entanglement.

## What are Bell States?

Bell states (also called EPR pairs) are maximally entangled quantum states of two qubits. The most common Bell state is:

$$|\Phi^+\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$$

This state has the interesting property that when we measure one qubit, the other qubit will always be found in the same state, even though individually, each qubit appears to be in a completely random state.

## Prerequisites

Before starting, make sure you have:

- HaskQ installed (see [Installation](../installation))
- Basic understanding of quantum computing concepts (see [Quantum Computing Basics](../core-concepts/quantum-computing-basics))
- Familiarity with Haskell and the HaskQ API

## Creating a Bell State in HaskQ

Let's implement a circuit to create the Bell state $|\Phi^+\rangle$:

```haskell
import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Simulator.Circuit (simulateCircuit)

-- Create a Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')
```

Let's break down what's happening:

1. `withQubits 2 $ \[q1, q2] -> ...` - Create two qubits, initially in the $|0\rangle$ state.
2. `q1' <- hadamard q1` - Apply the Hadamard gate to the first qubit, creating a superposition state $\frac{1}{\sqrt{2}}(|0\rangle + |1\rangle)$.
3. `(q1'', q2') <- cnot q1' q2` - Apply the CNOT gate with q1' as the control and q2 as the target. This entangles the two qubits.
4. `pure (q1'', q2')` - Return the two entangled qubits.

## Simulating and Measuring the Bell State

To verify that we've created a Bell state, we can simulate the circuit and measure both qubits:

```haskell
-- Create and measure a Bell state
measureBell :: Circ [Measurement]
measureBell = do
  (q1, q2) <- bellState
  (m1, _) <- measure q1
  (m2, _) <- measure q2
  pure [m1, m2]

-- Run the simulation
main :: IO ()
main = do
  let result = simulateCircuit 2 measureBell
  putStrLn $ "Measurement results: " ++ show (measurements result)
```

When you run this simulation multiple times, you should observe that:

1. Each individual measurement gives random results (approximately 50% Zero, 50% One).
2. The two measurements always match—either both Zero or both One.

## Visualizing the Bell State Circuit

We can use HaskQ's visualization tools to see a representation of our circuit:

```haskell
import HaskQ.Simulator.Visualizer

main :: IO ()
main = do
  let bellCircuit = bellState
  let visualization = visualizeCircuit bellCircuit
  putStrLn $ circuitToAscii visualization
```

This will output an ASCII representation of our Bell state circuit.

## Bell State Variations

There are actually four Bell states, forming a complete basis for two-qubit states:

1. $|\Phi^+\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$ - The one we created above
2. $|\Phi^-\rangle = \frac{1}{\sqrt{2}}(|00\rangle - |11\rangle)$
3. $|\Psi^+\rangle = \frac{1}{\sqrt{2}}(|01\rangle + |10\rangle)$
4. $|\Psi^-\rangle = \frac{1}{\sqrt{2}}(|01\rangle - |10\rangle)$

Let's modify our circuit to create $|\Phi^-\rangle$:

```haskell
phiMinus :: Circ (Qubit, Qubit)
phiMinus = do
  (q1, q2) <- bellState
  q2' <- pauliZ q2  -- Apply phase flip to second qubit
  pure (q1, q2')
```

Similarly, we can create the other Bell states:

```haskell
psiPlus :: Circ (Qubit, Qubit)
psiPlus = do
  (q1, q2) <- bellState
  q1' <- pauliX q1  -- Apply bit flip to first qubit
  pure (q1', q2)

psiMinus :: Circ (Qubit, Qubit)
psiMinus = do
  (q1, q2) <- bellState
  q1' <- pauliX q1  -- Apply bit flip to first qubit
  q2' <- pauliZ q2  -- Apply phase flip to second qubit
  pure (q1', q2')
```

## Conclusion

You've learned how to create Bell states in HaskQ, which are fundamental building blocks for many quantum algorithms and protocols, including quantum teleportation and dense coding.

In the next tutorial, we'll build on this knowledge to implement quantum teleportation, which uses Bell states to transmit quantum information. 