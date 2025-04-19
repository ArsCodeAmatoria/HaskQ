---
sidebar_position: 2
---

# Circuit Composition

Circuit composition is a fundamental concept in quantum computing that enables building complex quantum algorithms from simpler components. HaskQ provides elegant ways to compose quantum circuits using Haskell's functional programming paradigm.

## Functional Approach to Circuits

In HaskQ, quantum circuits are represented as functions that transform quantum states. This functional approach allows for natural composition of circuits using Haskell's powerful function composition capabilities.

```haskell
-- A simple circuit that applies a Hadamard gate
hadamardCircuit :: Q Qubit %1-> Q Qubit
hadamardCircuit q = gate H q

-- Another circuit that applies an X gate
xCircuit :: Q Qubit %1-> Q Qubit
xCircuit q = gate X q

-- Composing the two circuits
combinedCircuit :: Q Qubit %1-> Q Qubit
combinedCircuit = xCircuit . hadamardCircuit
```

## Sequential Composition

Sequential composition applies one circuit after another. HaskQ offers several ways to achieve this:

### Using Do Notation

```haskell
bellCircuit :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
bellCircuit (q1, q2) = do
  q1' <- gate H q1
  (q1'', q2') <- gate CNOT (q1', q2)
  return (q1'', q2')
```

### Using Function Composition

```haskell
-- Apply H, then T, then Z to a qubit
htzCircuit :: Q Qubit %1-> Q Qubit
htzCircuit = gate Z . gate T . gate H
```

### Using the Bind Operator

```haskell
bellCircuitAlt :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
bellCircuitAlt (q1, q2) = 
  gate H q1 >>= \q1' -> 
  gate CNOT (q1', q2) >>= \(q1'', q2') ->
  return (q1'', q2')
```

## Parallel Composition

Parallel composition applies different operations to different qubits simultaneously. HaskQ provides elegant ways to express this:

### Using Tuples and Pattern Matching

```haskell
parallelGates :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
parallelGates (q1, q2) = do
  q1' <- gate H q1
  q2' <- gate X q2
  return (q1', q2')
```

### Using Higher-Order Functions

```haskell
-- Apply Hadamard to all qubits in a list
hadamardAll :: [Q Qubit] %1-> [Q Qubit]
hadamardAll = map (gate H)
```

## Conditional Circuit Composition

HaskQ allows circuits to be applied conditionally based on classical or quantum conditions:

### Classical Control

```haskell
classicallyControlled :: Bool -> Q Qubit %1-> Q Qubit
classicallyControlled cond q =
  if cond 
  then gate X q
  else return q
```

### Quantum Control

```haskell
-- Apply X to q2 if q1 is in state |1⟩ (equivalent to CNOT)
quantumControlled :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
quantumControlled (q1, q2) = gate CNOT (q1, q2)
```

## Reusable Circuit Blocks

HaskQ encourages creating reusable circuit blocks that can be combined to create complex algorithms:

```haskell
-- A reusable quantum Fourier transform component
qft :: Int -> Q [Qubit] %1-> Q [Qubit]
qft n qubits = do
  -- QFT implementation...
  return transformedQubits

-- Using QFT in a larger algorithm
shor :: Int -> Q [Qubit] %1-> Q [Qubit]
shor n qubits = do
  -- Prepare qubits...
  transformed <- qft n preparedQubits
  -- Further operations...
  return finalQubits
```

## Parameterized Circuits

Circuits can be parameterized, allowing for generic components that can be specialized:

```haskell
-- A rotation circuit parameterized by angle
rotationCircuit :: Double -> Q Qubit %1-> Q Qubit
rotationCircuit theta q = gateP (RZ theta) q

-- Create specific rotations
rotateBy90 :: Q Qubit %1-> Q Qubit
rotateBy90 = rotationCircuit (pi/2)

rotateBy45 :: Q Qubit %1-> Q Qubit
rotateBy45 = rotationCircuit (pi/4)
```

## Circuit Repetition

Circuits can be repeated a specified number of times:

```haskell
-- Apply a Hadamard gate n times
repeatedH :: Int -> Q Qubit %1-> Q Qubit
repeatedH 0 q = return q
repeatedH n q = do
  q' <- gate H q
  repeatedH (n-1) q'
```

## Inverting Circuits

HaskQ provides ways to automatically generate inverse circuits, which is crucial for algorithms like quantum phase estimation:

```haskell
-- A circuit
myCircuit :: Q Qubit %1-> Q Qubit
myCircuit q = do
  q1 <- gate H q
  q2 <- gate T q1
  return q2

-- Its inverse (applies T† then H†)
inverseCircuit :: Q Qubit %1-> Q Qubit
inverseCircuit = invertCircuit myCircuit
```

## Example: Creating a Quantum Teleportation Circuit

A complete example showing circuit composition for quantum teleportation:

```haskell
teleport :: Q (Qubit, Qubit, Qubit) %1-> Q (Qubit, Qubit, Qubit)
teleport (message, alice, bob) = do
  -- Create entanglement between Alice and Bob's qubits
  alice' <- gate H alice
  (alice'', bob') <- gate CNOT (alice', bob)
  
  -- Alice entangles her qubit with the message
  (message', alice''') <- gate CNOT (message, alice'')
  message'' <- gate H message'
  
  -- Measure Alice's qubits
  (bit1, message''') <- measure message''
  (bit2, alice'''') <- measure alice'''
  
  -- Apply corrections to Bob's qubit based on measurements
  bob'' <- if bit2 then gate X bob' else return bob'
  bob''' <- if bit1 then gate Z bob'' else return bob''
  
  return (message''', alice'''', bob''')
```

## Best Practices for Circuit Composition

1. **Modularity**: Build small, reusable circuit components
2. **Type Safety**: Leverage Haskell's type system to ensure correct composition
3. **Linear Types**: Use HaskQ's linear types to ensure qubits are used exactly once
4. **Documentation**: Document the purpose and expected behavior of your circuits
5. **Testing**: Write tests for circuit components to verify their behavior

## Next Steps

Now that you understand circuit composition in HaskQ, you might want to:

1. Learn about [Quantum Algorithms](../tutorials/algorithms.md)
2. Explore [Quantum Error Correction](./error-correction.md)
3. Study [Linear Types in HaskQ](./linear-types.md) for proper qubit management 