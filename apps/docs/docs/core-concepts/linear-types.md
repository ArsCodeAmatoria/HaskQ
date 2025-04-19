---
sidebar_position: 6
---

# Linear Types in Quantum Computing

HaskQ leverages GHC's LinearTypes extension to enforce the no-cloning theorem of quantum mechanics. This document explains what linear types are, why they're essential for quantum programming, and how to use them effectively in HaskQ.

## Why Linear Types?

Quantum computing follows different rules than classical computing:

- **No-cloning theorem**: A quantum state cannot be copied
- **Measurement destroys superposition**: Once measured, a qubit collapses to a classical state
- **Quantum operations must be reversible**: Except for measurement, all operations must be unitary

Linear types provide a way to enforce these rules at compile time, ensuring that quantum programs follow the laws of quantum mechanics.

## Linear Types Basics

Linear types in Haskell enforce that a value must be used exactly once - no more, no less. This is indicated by the `%1->` arrow in type signatures.

```haskell
-- A linear function: the argument 'a' must be used exactly once
linearFunction :: a %1-> b

-- A non-linear function: the argument 'a' can be used any number of times (including zero)
normalFunction :: a -> b
```

### Examples in HaskQ

In HaskQ, quantum operations follow linear typing:

```haskell
-- Apply Hadamard gate to a qubit
-- The qubit must be used exactly once
gate H :: Q Qubit %1-> Q Qubit

-- Measure a qubit, consuming it and returning a classical bit
-- and the post-measurement qubit
measure :: Q Qubit %1-> (Bit, Q Qubit)
```

## Working with Linear Types

### Linear Resources in HaskQ

HaskQ defines several linear resource types:

```haskell
-- A quantum bit - must be used exactly once
newtype Q a = Q { unQ :: a }

-- A quantum register - must be used exactly once
data QReg a = QReg Int a
```

### Creating and Consuming Linear Resources

```haskell
-- Create a new qubit initialized to |0⟩
qinit :: Bool -> Circ (Q Qubit)

-- Release a qubit, ensuring it's in the |0⟩ state
qrelease :: Q Qubit %1-> Circ ()

-- Measure and release a qubit in one operation
measureAndRelease :: Q Qubit %1-> Circ Bit
```

## Linear Type Patterns

### Chaining Linear Operations

Linear operations are typically chained together, with each operation consuming the previous value and producing a new one:

```haskell
circuit :: Circ Bit
circuit = do
  q0 <- qinit False       -- Create a qubit in |0⟩
  q1 <- gate H q0         -- Apply H gate to q0, producing q1
  q2 <- gate T q1         -- Apply T gate to q1, producing q2
  measureAndRelease q2    -- Measure and release q2
```

### Pattern Matching with Linear Types

When pattern matching on linear values, you must ensure all branches use the value exactly once:

```haskell
conditionalGate :: Bool -> Q Qubit %1-> Circ (Q Qubit)
conditionalGate cond q = 
  if cond
    then gate H q     -- Use q once here
    else gate X q     -- Or use q once here
```

## Common Challenges with Linear Types

### The Sharing Problem

Linear types prevent sharing of variables. If you need to use a value multiple times, you need explicit duplication mechanisms:

```haskell
-- WRONG: Uses q twice
wrong :: Q Qubit %1-> Circ (Q Qubit)
wrong q = do
  q1 <- gate H q
  q2 <- gate T q  -- Error: q already used!
  return q2

-- RIGHT: Chains operations
right :: Q Qubit %1-> Circ (Q Qubit)
right q = do
  q' <- gate H q
  q'' <- gate T q'
  return q''
```

### Conditional Quantum Operations

When applying quantum operations conditionally, ensure the linear resource is consumed in all branches:

```haskell
applyIfMeasured :: Bit -> Q Qubit %1-> Circ (Q Qubit)
applyIfMeasured bit q =
  case bit of
    0 -> return q                -- Just pass q through
    1 -> gate X q                -- Apply X gate to q
```

## Advanced Linear Type Techniques

### Unrestricted Types with `Ur`

Sometimes you need to mix linear and non-linear data. HaskQ provides the `Ur` type for this purpose:

```haskell
-- Ur a represents an unrestricted (non-linear) value of type a
data Ur a = Ur a

-- Extract the value from Ur (this is a non-linear operation)
unur :: Ur a -> a
```

Example usage:

```haskell
-- A circuit that performs different operations based on a classical parameter
parameterizedCircuit :: Ur Int -> Q Qubit %1-> Circ (Q Qubit)
parameterizedCircuit (Ur n) q =
  case n of
    0 -> gate H q
    1 -> gate X q
    _ -> gate Z q
```

### Multiplicity Polymorphism

GHC's LinearTypes extension also supports multiplicity polymorphism, allowing functions to work with both linear and non-linear inputs:

```haskell
-- A function that works with any multiplicity
polymorphicFunction :: forall {m} a. a %m-> a
polymorphicFunction x = x
```

## Linear Types and Quantum Algorithms

Many quantum algorithms naturally fit the linear type paradigm:

```haskell
-- Quantum teleportation algorithm with linear types
teleport :: Q Qubit %1-> Circ (Q Qubit)
teleport q1 = do
  -- Create Bell pair
  q2 <- qinit False
  q3 <- qinit False
  q2' <- gate H q2
  (q2'', q3') <- gate CNOT (q2', q3)
  
  -- Bell measurement
  (q1', q2''') <- gate CNOT (q1, q2'')
  q1'' <- gate H q1'
  b1 <- measure q1''
  b2 <- measure q2'''
  
  -- Apply corrections based on measurement results
  q3'' <- if b2 == 1 then gate X q3' else return q3'
  q3''' <- if b1 == 1 then gate Z q3'' else return q3''
  
  return q3'''
```

## Best Practices for Linear Types

1. **Design linearly**: Think about the flow of quantum data as a pipeline
2. **Use monadic style**: The `do` notation works well with linear types
3. **Keep quantum and classical separate**: Use `Ur` to mark classical values
4. **Pattern match carefully**: Ensure all branches consume linear values
5. **Leverage the type checker**: Let the compiler catch no-cloning violations

## Common Errors and Solutions

| Error | Explanation | Solution |
|-------|-------------|----------|
| "Linear resource used more than once" | You've tried to use a quantum value multiple times | Redesign to chain operations sequentially |
| "Linear resource not used" | You've declared but not used a quantum value | Either use it or explicitly discard it |
| "Couldn't match type 'Many' with 'One'" | Mixing linear and non-linear types incorrectly | Use `Ur` to wrap non-linear values |

## Next Steps

Now that you understand linear types in HaskQ, you might want to explore:

1. [Circuit Composition](./circuit-composition.md) for building larger quantum programs
2. [Measurement](./measurement.md) for understanding quantum measurement
3. [Advanced Quantum Algorithms](../tutorials/algorithms.md) that make use of linear types 