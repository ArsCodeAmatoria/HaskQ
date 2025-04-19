---
sidebar_position: 4
---

# Measurement and Classical Control

Measurement is a fundamental operation in quantum computing that bridges the quantum and classical worlds. This document explains how measurement works in HaskQ and how to use measurement results to control classical operations.

## Understanding Quantum Measurement

In quantum mechanics, measurement causes a quantum state to collapse from a superposition to a definite classical state. The outcome is probabilistic, with probabilities determined by the quantum state before measurement.

In HaskQ, measurement is represented by functions that consume a quantum resource (using linear types) and return both a classical result and the post-measurement quantum state.

## Basic Measurement in HaskQ

The fundamental measurement operation in HaskQ is:

```haskell
measure :: Q Qubit %1-> (Bit, Q Qubit)
```

This function:
1. Takes a qubit linearly (consuming it)
2. Returns a tuple containing:
   - A classical bit (the measurement result)
   - The post-measurement qubit (which can be reused)

Here's a simple example:

```haskell
measureQubit :: Q Qubit %1-> (Bit, Q Qubit)
measureQubit q = measure q

-- Usage example
singleMeasurement :: Q Qubit %1-> Bit
singleMeasurement q = do
  (result, q') <- measure q
  qdiscard q'  -- Discard the post-measurement qubit
  return result
```

## Measurement Outcomes

The `Bit` type represents classical measurement outcomes:

```haskell
data Bit = Zero | One | Superposition Double
```

Where:
- `Zero` represents the |0⟩ state
- `One` represents the |1⟩ state
- `Superposition` is used for probabilistic simulation results with the probability of measuring |1⟩

## Measuring Multiple Qubits

You can measure multiple qubits individually or as a combined system:

```haskell
-- Measure two qubits separately
measurePair :: Q (Qubit, Qubit) %1-> ((Bit, Bit), (Q Qubit, Q Qubit))
measurePair (q1, q2) = do
  (b1, q1') <- measure q1
  (b2, q2') <- measure q2
  return ((b1, b2), (q1', q2'))

-- Measure a register of qubits
measureAll :: Q [Qubit] %1-> ([Bit], Q [Qubit])
measureAll [] = return ([], [])
measureAll (q:qs) = do
  (b, q') <- measure q
  (bs, qs') <- measureAll qs
  return (b:bs, q':qs')
```

## Classical Control Based on Measurement

Measurement results can be used to control classical operations, creating hybrid quantum-classical algorithms:

```haskell
-- Apply X gate only if measurement gives |1⟩
measureAndControl :: Q (Qubit, Qubit) %1-> Q (Qubit, Qubit)
measureAndControl (q1, q2) = do
  (result, q1') <- measure q1
  q2' <- case result of
    One -> gate X q2
    _   -> return q2
  return (q1', q2')
```

## Measurement in Algorithms

Measurement is crucial in many quantum algorithms:

### Quantum Teleportation

```haskell
teleport :: Q (Qubit, Qubit, Qubit) %1-> Q (Qubit, Qubit, Qubit)
teleport (message, alice, bob) = do
  -- Create entanglement and perform operations...
  
  -- Measure Alice's qubits
  (bit1, message') <- measure message
  (bit2, alice') <- measure alice
  
  -- Apply corrections to Bob's qubit based on measurements
  bob' <- if bit2 == One then gate X bob else return bob
  bob'' <- if bit1 == One then gate Z bob' else return bob'
  
  return (message', alice', bob'')
```

### Quantum Random Number Generation

```haskell
quantumRNG :: IO Bool
quantumRNG = do
  let circuit = do
        q <- qinit False
        q' <- gate H q
        (result, q'') <- measure q'
        qdiscard q''
        return (result == One)
  runCircuit circuit
```

## Deferred Measurement

The principle of deferred measurement states that measurements can be moved to the end of a quantum circuit without affecting the outcome. HaskQ allows you to express this principle:

```haskell
-- Version with early measurement
earlyMeasurement :: Q (Qubit, Qubit) %1-> Q Qubit
earlyMeasurement (control, target) = do
  (result, control') <- measure control
  qdiscard control'
  if result == One
    then gate X target
    else return target

-- Equivalent version using CNOT (deferred measurement)
deferredMeasurement :: Q (Qubit, Qubit) %1-> Q Qubit
deferredMeasurement (control, target) = do
  (control', target') <- gate CNOT (control, target)
  qdiscard control'
  return target'
```

## Mid-Circuit Measurement

HaskQ supports measurement in the middle of a circuit:

```haskell
midCircuitMeasurement :: Q Qubit %1-> Q Qubit
midCircuitMeasurement q = do
  q1 <- gate H q
  (result, q2) <- measure q1
  -- Continue with quantum operations on the post-measurement state
  q3 <- if result == One
        then gate Z q2
        else gate X q2
  return q3
```

## Simulation and Probabilities

When simulating quantum circuits, HaskQ provides insights into measurement probabilities:

```haskell
-- Get the probability of measuring |1⟩
simulateMeasurement :: Q Qubit %1-> Double
simulateMeasurement q = do
  let sim = simulateCircuit 1 $ do
        q' <- applyCircuit q
        (result, _) <- measure q'
        return result
  case measurementResult sim of
    Superposition p -> p
    One -> 1.0
    Zero -> 0.0
```

## Visualizing Measurement

The HaskQ simulator can visualize measurements in circuit diagrams:

```
Circuit with measurement:

0: --H--M--
       |
1: ----X--
```

In this diagram, 'M' represents a measurement, and the line connects to a classically controlled operation.

## Best Practices for Measurement

1. **Post-Measurement States**: Always handle post-measurement states, either by using them or explicitly discarding them
2. **Linear Types**: Use HaskQ's linear type system to ensure all qubits are properly measured or discarded
3. **Deferred Measurement**: When possible, defer measurements to the end of the circuit for better optimization
4. **Error Handling**: Account for probabilistic nature of measurement in your algorithms

## Next Steps

Now that you understand measurement in HaskQ, you might want to explore:

1. [Quantum Algorithms](../tutorials/algorithms.md) that use measurement
2. [Error Correction](./error-correction.md) techniques that rely on measurement
3. [Quantum-Classical Hybrid Algorithms](../tutorials/hybrid-algorithms.md) 