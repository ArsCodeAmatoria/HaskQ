---
sidebar_position: 2
---

# Linear Types in HaskQ

HaskQ leverages Haskell's linear types extension to enforce the quantum no-cloning theorem at the type level, ensuring that invalid quantum programs cannot be constructed.

## What are Linear Types?

Linear types enforce a "use exactly once" policy for resources. When a function has a linear function type like:

```haskell
f :: a %1-> b
```

It means that the function `f` must use its argument exactly once—no more, no less.

This is in contrast to the standard function arrow `->`, which allows arguments to be used zero, one, or many times.

## Why Linear Types for Quantum Computing?

The no-cloning theorem in quantum mechanics states that it's impossible to create an identical copy of an arbitrary unknown quantum state. This restriction should be enforced by the programming model.

Linear types are a perfect match for quantum computing because:

1. Qubits cannot be copied (no-cloning theorem)
2. Qubits cannot be discarded without measurement (conservation of information)
3. Quantum operations transform qubits in a one-to-one manner

## Linear Types in HaskQ

In HaskQ, the `Qubit` type is handled linearly. This means:

```haskell
-- This won't type-check: uses q twice
invalid :: Qubit %1-> (Qubit, Qubit)
invalid q = (q, q)

-- This won't type-check: doesn't use q
alsoBad :: Qubit %1-> Int
alsoBad q = 42

-- This is valid: uses q exactly once
good :: Qubit %1-> Qubit
good q = q
```

### The Circ Monad and Linear Types

HaskQ's `Circ` monad is designed to work with linear types. When you compose quantum operations in the `Circ` monad, you're guaranteed that each qubit is used exactly once at each step:

```haskell
bellCircuit :: Circ (Qubit, Qubit)
bellCircuit = do
  q1 <- qubit         -- Create a new qubit
  q2 <- qubit         -- Create another new qubit
  q1' <- hadamard q1  -- Apply H to q1, get new qubit q1'
  (q1'', q2') <- cnot q1' q2  -- Apply CNOT to q1' and q2
  pure (q1'', q2')   -- Return the final two qubits
```

In this example, each qubit is used exactly once at each step, enforcing linear usage.

## Consuming and Producing Qubits

All quantum gates in HaskQ consume qubits and produce new ones, reflecting the transformation of quantum states:

```haskell
hadamard :: Qubit %1-> Circ Qubit
pauliX   :: Qubit %1-> Circ Qubit
cnot     :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
```

### Measurement

When measuring a qubit, we get both a classical result and a post-measurement qubit state:

```haskell
measure :: Qubit %1-> Circ (Measurement, Qubit)
```

The post-measurement qubit is still a linear resource that must be handled properly.

## Benefits of Linear Types in HaskQ

The use of linear types in HaskQ provides several benefits:

1. **Safety**: Impossible to write code that violates the no-cloning theorem.
2. **Correctness**: Quantum state is tracked correctly through the program.
3. **Composability**: Linear types ensure that quantum operations chain together properly.
4. **Optimization**: Compiler can optimize using the knowledge that qubits are used linearly.

## Practical Example: Quantum Teleportation

Here's how linear types help ensure correctness in the quantum teleportation algorithm:

```haskell
teleport :: Qubit %1-> Circ Qubit
teleport q_in = do
  -- Create Bell pair
  (alice, bob) <- bellState
  
  -- Entangle input with Alice's qubit
  (q_in', alice') <- cnot q_in alice
  q_in'' <- hadamard q_in'
  
  -- Measure both qubits
  (m1, _) <- measure q_in''  -- Qubit is consumed by measurement
  (m2, _) <- measure alice'  -- Qubit is consumed by measurement
  
  -- Apply corrections to Bob's qubit based on measurements
  bob' <- case (m1, m2) of
    (Zero, Zero) -> pure bob
    (Zero, One)  -> pauliX bob
    (One, Zero)  -> pauliZ bob
    (One, One)   -> pauliZ bob >>= pauliX
    _            -> pure bob
    
  pure bob'  -- Return Bob's qubit
```

Linear types ensure that each qubit is used exactly once and all quantum operations are properly chained. 