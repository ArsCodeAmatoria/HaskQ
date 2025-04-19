---
sidebar_position: 1
---

# Creating Bell States

This tutorial guides you through creating a Bell state, one of the fundamental entangled quantum states, using HaskQ.

## What is a Bell State?

A Bell state (also called an EPR pair) is a maximally entangled quantum state of two qubits. The most common Bell state is:

$$|\Phi^+\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$$

This state has the special property that when you measure one qubit, you instantly know the state of the other, regardless of the distance between them.

## Creating a Bell State in HaskQ

Here's how to create a Bell state using HaskQ:

```haskell
-- Import necessary modules
import HaskQ.Prelude
import HaskQ.Circuit
import HaskQ.Simulator

-- Bell state circuit
bellState :: Circ (Q Qubit, Q Qubit)
bellState = do
  -- Initialize two qubits in the |0⟩ state
  q1 <- qinit False  -- |0⟩
  q2 <- qinit False  -- |0⟩
  
  -- Apply Hadamard to the first qubit
  q1' <- gate H q1
  
  -- Apply CNOT with q1' as control and q2 as target
  (q1'', q2') <- gate CNOT (q1', q2)
  
  -- Return the entangled pair
  return (q1'', q2')
```

## Analyzing the Circuit

Let's break down what's happening:

1. We start with two qubits in the state |00⟩
2. After applying Hadamard to the first qubit, we get:
   $$\frac{1}{\sqrt{2}}(|0\rangle + |1\rangle) \otimes |0\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |10\rangle)$$
3. After applying CNOT, we get:
   $$\frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$$
   
Which is the Bell state we wanted!

## Simulating and Measuring

You can simulate and measure this state:

```haskell
-- Function to simulate and measure Bell state
simulateBell :: IO (Bit, Bit)
simulateBell = do
  let circuit = do
        (q1, q2) <- bellState
        b1 <- measureAndRelease q1
        b2 <- measureAndRelease q2
        return (b1, b2)
  runCircuit circuit
```

When you measure this state, you'll always get either (0,0) or (1,1), each with 50% probability, demonstrating the perfect correlation between the qubits.

## Next Steps

Now that you understand how to create Bell states, you can:

1. Try creating other Bell states by adding X gates
2. Use Bell states as a foundation for quantum teleportation
3. Experiment with measuring just one qubit and observing the effects 