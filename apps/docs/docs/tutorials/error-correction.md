---
sidebar_position: 7
---

# Error Correction in HaskQ

Quantum error correction is essential for building reliable quantum computers. Qubits are highly sensitive to environmental noise, and without error correction, quantum computations quickly become unreliable.

## Quantum Error Correction Fundamentals

Unlike classical error correction, quantum error correction faces unique challenges:

1. **No-cloning theorem**: We cannot make perfect copies of unknown quantum states
2. **Continuous errors**: Quantum errors are continuous rather than discrete
3. **Measurement destroys superposition**: We cannot directly measure qubits to detect errors

To overcome these challenges, quantum error correction uses entanglement and syndrome measurements to detect and correct errors without directly measuring the encoded information.

## Types of Quantum Errors

The most common types of quantum errors are:

1. **Bit-flip errors**: Similar to classical bit flips (|0⟩ ↔ |1⟩)
2. **Phase-flip errors**: Quantum-specific errors that flip the phase (|+⟩ ↔ |-⟩)
3. **Combined errors**: Both bit and phase flips occurring together

## Bit-Flip Code

The bit-flip code protects against bit-flip errors by encoding a single logical qubit using three physical qubits.

### Encoding Circuit

```
q₀: ──•───•──
      │   │  
q₁: ──X───┼──
      │   │  
q₂: ──┼───X──
```

### Implementation in HaskQ

```haskell
-- Encode a qubit using the bit-flip code
encodeBitFlip :: Qubit %1-> Circ [Qubit]
encodeBitFlip qubit = do
  -- Create two additional qubits in |0⟩ state
  extraQubits <- withQubits 2 (\qs -> pure qs)
  let [ancilla1, ancilla2] = extraQubits
  
  -- Apply CNOT gates to copy the state
  (qubit', ancilla1') <- cnot qubit ancilla1
  (qubit'', ancilla2') <- cnot qubit' ancilla2
  
  -- Return the three-qubit encoded state
  pure [qubit'', ancilla1', ancilla2']

-- Detect and correct a bit-flip error
correctBitFlip :: [Qubit] %1-> Circ [Qubit]
correctBitFlip qubits 
  | length qubits /= 3 = pure qubits  -- Invalid input
  | otherwise = do
      let [q1, q2, q3] = qubits
      
      -- Create ancilla qubits for syndrome measurement
      anc1 <- qubit
      anc2 <- qubit
      
      -- Measure syndromes using ancilla qubits
      (q1', q2', anc1') <- cnotMulti [q1, q2] anc1
      (q1'', q3', anc2') <- cnotMulti [q1', q3] anc2
      
      -- Measure ancilla qubits to get syndrome
      (m1, anc1'') <- measure anc1'
      (m2, anc2'') <- measure anc2'
      
      -- Apply correction based on syndrome
      correctedQubits <- case (m1, m2) of
        (Zero, Zero) -> pure [q1'', q2', q3']  -- No error
        (Zero, One)  -> do                     -- Error on qubit 3
          q3'' <- pauliX q3'
          pure [q1'', q2', q3'']
        (One, Zero)  -> do                     -- Error on qubit 2
          q2'' <- pauliX q2'
          pure [q1'', q2'', q3']
        (One, One)   -> do                     -- Error on qubit 1
          q1''' <- pauliX q1''
          pure [q1''', q2', q3']
      
      pure correctedQubits

-- Decode the bit-flip code back to a single qubit
decodeBitFlip :: [Qubit] %1-> Circ Qubit
decodeBitFlip [q1, q2, q3] = do
  -- Majority vote decoding (can be implemented various ways)
  -- Here we'll use the first qubit and verify consistency
  
  -- Check if qubits 1 and 2 are consistent
  (q1', q2', anc) <- consistencyCheck q1 q2
  (m, _) <- measure anc
  
  -- If consistent, use qubit 1, otherwise use qubit 3
  case m of
    Zero -> pure q1'  -- Qubits 1 and 2 agree, use qubit 1
    One  -> pure q3   -- Qubits 1 and 2 disagree, use qubit 3
decodeBitFlip qubits = do
  -- Invalid input, return first qubit or default
  if null qubits then qubit else pure (head qubits)

-- Helper function to check consistency between two qubits
consistencyCheck :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit, Qubit)
consistencyCheck q1 q2 = do
  anc <- qubit
  (q1', anc') <- cnot q1 anc
  (q2', anc'') <- cnot q2 anc'
  pure (q1', q2', anc'')
```

## Phase-Flip Code

The phase-flip code protects against phase errors by encoding in the Hadamard basis.

### Encoding Circuit

```
q₀: ──H───•───•───H──
           │   │     
q₁: ──H───X───┼───H──
           │   │     
q₂: ──H───┼───X───H──
```

### Implementation in HaskQ

```haskell
-- Encode a qubit using the phase-flip code
encodePhaseFlip :: Qubit %1-> Circ [Qubit]
encodePhaseFlip qubit = do
  -- First apply Hadamard to enter the X-basis
  qubit' <- hadamard qubit
  
  -- Then use the bit-flip code encoding
  encodedQubits <- encodeBitFlip qubit'
  
  -- Apply Hadamard to all qubits to return to Z-basis
  encodedQubits' <- mapM hadamard encodedQubits
  
  pure encodedQubits'

-- Detect and correct a phase-flip error
correctPhaseFlip :: [Qubit] %1-> Circ [Qubit]
correctPhaseFlip qubits = do
  -- Convert to the X-basis where phase flips appear as bit flips
  qubits' <- mapM hadamard qubits
  
  -- Use bit-flip correction
  correctedQubits <- correctBitFlip qubits'
  
  -- Convert back to the Z-basis
  correctedQubits' <- mapM hadamard correctedQubits
  
  pure correctedQubits'

-- Decode the phase-flip code back to a single qubit
decodePhaseFlip :: [Qubit] %1-> Circ Qubit
decodePhaseFlip qubits = do
  -- Convert to the X-basis
  qubits' <- mapM hadamard qubits
  
  -- Decode using bit-flip code decoding
  qubit' <- decodeBitFlip qubits'
  
  -- Convert back to the Z-basis
  qubit'' <- hadamard qubit'
  
  pure qubit''
```

## Shor's Nine-Qubit Code

Shor's code combines the bit-flip and phase-flip codes to protect against both types of errors using nine qubits.

### Implementation in HaskQ

```haskell
-- Encode a qubit using Shor's code
encodeShor :: Qubit %1-> Circ [Qubit]
encodeShor qubit = do
  -- First encode with phase-flip code
  phaseEncoded <- encodePhaseFlip qubit
  
  -- Then encode each resulting qubit with bit-flip code
  bitEncoded1 <- encodeBitFlip (phaseEncoded !! 0)
  bitEncoded2 <- encodeBitFlip (phaseEncoded !! 1)
  bitEncoded3 <- encodeBitFlip (phaseEncoded !! 2)
  
  -- Combine all nine qubits
  pure (bitEncoded1 ++ bitEncoded2 ++ bitEncoded3)

-- Decode Shor's code back to a single qubit
decodeShor :: [Qubit] %1-> Circ Qubit
decodeShor qubits
  | length qubits /= 9 = do
      -- Invalid input
      qubit
  | otherwise = do
      -- Split into three blocks of three qubits
      let block1 = take 3 qubits
          block2 = take 3 (drop 3 qubits)
          block3 = drop 6 qubits
      
      -- Decode each block using bit-flip code
      q1 <- decodeBitFlip block1
      q2 <- decodeBitFlip block2
      q3 <- decodeBitFlip block3
      
      -- Decode the resulting three qubits using phase-flip code
      decodePhaseFlip [q1, q2, q3]
```

## Error Correction Example

Here's a complete example demonstrating the bit-flip code with an intentional error:

```haskell
-- Run error correction example
errorCorrectionDemo :: Circ [Measurement]
errorCorrectionDemo = do
  -- Create a qubit in superposition
  q <- qubit
  q' <- hadamard q
  
  -- Encode using bit-flip code
  encoded <- encodeBitFlip q'
  
  -- Introduce an error (bit flip) on the first qubit
  let [q1, q2, q3] = encoded
  q1' <- pauliX q1
  
  -- Correct the error
  corrected <- correctBitFlip [q1', q2, q3]
  
  -- Decode back to a single qubit
  decoded <- decodeBitFlip corrected
  
  -- Measure and return the result
  -- Should match the original state despite the error
  result <- hadamard decoded
  (m, _) <- measure result
  
  pure [m]
```

## Running Error Correction in HaskQ

HaskQ includes an error correction example that you can run:

```bash
cabal run haskq-error-correction
```

This will simulate various error correction codes and compare their performance in the presence of different types of errors.

## Error Correction in Realistic Settings

In practical quantum computing, error correction is more complex:

1. **Fault Tolerance**: Ensuring that errors don't propagate during correction
2. **Error Thresholds**: Determining when error correction provides a net benefit
3. **Resource Requirements**: Balancing qubit count vs. error protection
4. **Logical Operations**: Performing computations on encoded logical qubits

These advanced aspects are covered in the [Fault Tolerance](./fault-tolerance.md) and [Surface Codes](./surface-codes.md) tutorials.

## Limitations of Error Correction

Quantum error correction has important limitations to be aware of:

1. **Threshold Theorem**: Error correction only works if physical error rates are below a threshold
2. **Qubit Overhead**: Significant numbers of physical qubits are needed for each logical qubit
3. **Measurement Speed**: Syndrome measurements must be faster than error rates
4. **Gate Complexity**: Error correction increases circuit depth and complexity

## Next Steps

To deepen your understanding of quantum error correction:

1. Experiment with the provided error correction examples
2. Learn about [surface codes](./surface-codes.md), which are more practical for large-scale quantum computers
3. Explore [noise models](./noise-models.md) to simulate realistic quantum devices
4. Understand the principles of [fault tolerance](./fault-tolerance.md) for scalable quantum computing

## Related Topics

- [Fault Tolerance](./fault-tolerance.md)
- [Surface Codes](./surface-codes.md)
- [Noise Models](./noise-models.md) 