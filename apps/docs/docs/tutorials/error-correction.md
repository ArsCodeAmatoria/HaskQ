---
sidebar_position: 3
---

# Quantum Error Correction

This tutorial explains how quantum error correction works in HaskQ and shows you how to implement and test error correction codes.

## Introduction to Quantum Error Correction

Quantum information is extremely fragile—even minor interactions with the environment can cause decoherence and errors in quantum states. Quantum Error Correction (QEC) provides techniques to protect quantum information from these errors.

HaskQ implements three fundamental quantum error correction codes:

1. **Bit-Flip Code**: Protects against X (NOT) errors
2. **Phase-Flip Code**: Protects against Z (phase) errors
3. **Shor Code**: Protects against arbitrary single-qubit errors

## Getting Started with Error Correction

HaskQ provides built-in functions for quantum error correction in the `HaskQ.Core.ErrorCorrection` module, which is re-exported through `HaskQ.Prelude`.

```haskell
import HaskQ.Prelude

-- Now you have access to:
-- encodeBitFlip, decodeBitFlip, correctBitFlip
-- encodePhaseFlip, decodePhaseFlip, correctPhaseFlip
-- encodeShor, decodeShor
```

You can test error correction examples directly from the command line:

```bash
# Run the bit-flip code example
haskq-error-correction bit-flip

# Run the phase-flip code example
haskq-error-correction phase-flip

# Run the Shor code example
haskq-error-correction shor

# Save circuit visualization to a file
haskq-error-correction bit-flip output.txt
```

## The Bit-Flip Code

The bit-flip code encodes a single logical qubit into three physical qubits, protecting against bit-flip (X) errors.

### How It Works

1. **Encoding**: Transforms |ψ⟩ = α|0⟩ + β|1⟩ into α|000⟩ + β|111⟩
2. **Error Detection**: Uses syndrome measurements to identify if a bit-flip occurred
3. **Correction**: Applies X gates to the affected qubit

### Implementation

```haskell
import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii)
import qualified Data.Text.IO as TIO
import qualified Prelude as P

-- Simple bit-flip code example
bitFlipExample :: IO ()
bitFlipExample = do
  -- Define the circuit
  let circuit = do
        -- Create a qubit in a superposition state
        q <- qubit
        q' <- hadamard q
        
        -- Encode using the bit-flip code
        (q1, q2, q3) <- encodeBitFlip q'
        
        -- Introduce an artificial bit-flip error on the first qubit
        q1' <- pauliX q1
        
        -- Perform error correction
        (q1'', q2', q3') <- correctBitFlip q1' q2 q3
        
        -- Decode the state
        q_decoded <- decodeBitFlip q1'' q2' q3'
        
        -- Apply Hadamard to return to |0⟩ state
        q_final <- hadamard q_decoded
        
        -- Measure and observe that the error was corrected
        (m, _) <- measure q_final
        pure [m]
  
  -- Visualize the circuit
  let vis = visualizeCircuit circuit
  P.putStrLn "Bit-Flip Code Circuit:"
  TIO.putStrLn $ circuitToAscii vis
  
  -- Simulate the circuit
  let result = simulateCircuit 10 circuit  -- Need enough qubits for ancillas
  P.putStrLn $ "\nMeasurement result: " ++ P.show (measurements result)
  
  -- Check if error correction was successful
  P.putStrLn $ "Error correction " ++ 
    if measurements result == [Zero]
    then "succeeded! ✓"
    else "failed! ✗"

main :: IO ()
main = bitFlipExample
```

## The Phase-Flip Code

The phase-flip code protects against phase-flip (Z) errors by encoding in the X basis.

### How It Works

1. **Encoding**: Applies Hadamard gates to transform to the X basis, then uses bit-flip encoding
2. **Result**: Transforms |ψ⟩ into α|+++⟩ + β|---⟩
3. **Correction**: Applies Hadamard transforms to convert phase errors to bit errors, corrects, then transforms back

### Implementation

```haskell
-- Phase-flip code example
phaseFlipExample :: IO ()
phaseFlipExample = do
  -- Define the circuit
  let circuit = do
        -- Create a qubit in the |0⟩ state
        q <- qubit
        
        -- Encode using the phase-flip code
        (q1, q2, q3) <- encodePhaseFlip q
        
        -- Introduce an artificial phase-flip error on the second qubit
        q2' <- pauliZ q2
        
        -- Perform error correction
        (q1', q2'', q3') <- correctPhaseFlip q1 q2' q3
        
        -- Decode the state
        q_decoded <- decodePhaseFlip q1' q2'' q3'
        
        -- Measure and observe that the error was corrected
        (m, _) <- measure q_decoded
        pure [m]
  
  -- Visualize and simulate as with the bit-flip example
  let vis = visualizeCircuit circuit
  P.putStrLn "Phase-Flip Code Circuit:"
  TIO.putStrLn $ circuitToAscii vis
  
  let result = simulateCircuit 10 circuit
  P.putStrLn $ "\nMeasurement result: " ++ P.show (measurements result)
  
  P.putStrLn $ "Error correction " ++ 
    if measurements result == [Zero]
    then "succeeded! ✓"
    else "failed! ✗"
```

## The Shor Code

The Shor code combines the bit-flip and phase-flip codes to protect against arbitrary single-qubit errors (X, Z, and Y=XZ).

### How It Works

1. **Encoding**: First encodes the qubit with the phase-flip code, then encodes each resulting qubit with the bit-flip code
2. **Result**: Uses 9 physical qubits to protect 1 logical qubit
3. **Correction**: Can correct any single-qubit error

### Implementation

```haskell
-- Shor code example
shorCodeExample :: IO ()
shorCodeExample = do
  -- Define the circuit
  let circuit = do
        -- Create a qubit in the |+⟩ state
        q <- qubit
        q' <- hadamard q
        
        -- Encode using the Shor code (9 qubits)
        encoded <- encodeShor q'
        
        -- Introduce both bit and phase errors on the middle qubit
        let q5 = encoded !! 4  -- Middle qubit of second group
        q5' <- pauliX q5
        q5'' <- pauliZ q5'
        
        -- Replace the errored qubit in the encoded list
        let encoded' = 
              take 4 encoded ++ [q5''] ++ drop 5 encoded
        
        -- Decode (error correction happens during decoding)
        q_decoded <- decodeShor encoded'
        
        -- Apply Hadamard to return to |0⟩ state
        q_final <- hadamard q_decoded
        
        -- Measure and observe that errors were corrected
        (m, _) <- measure q_final
        pure [m]
  
  -- Visualization and simulation as before
  let vis = visualizeCircuit circuit
  P.putStrLn "Shor Code Circuit:"
  TIO.putStrLn $ circuitToAscii vis
  
  let result = simulateCircuit 20 circuit  -- Need enough qubits (9 + ancillas)
  P.putStrLn $ "\nMeasurement result: " ++ P.show (measurements result)
  
  P.putStrLn $ "Error correction " ++ 
    if measurements result == [Zero]
    then "succeeded! ✓"
    else "failed! ✗"
```

## Under the Hood: How Error Correction is Implemented

Let's explore how error correction works in HaskQ by examining the implementation of the bit-flip code:

### Bit-Flip Encoding

```haskell
-- From HaskQ.Core.ErrorCorrection
encodeBitFlip :: Qubit %1-> Circ (Qubit, Qubit, Qubit)
encodeBitFlip q = withQubits 2 $ \[q2, q3] -> do
  -- Create two ancilla qubits initialized to |0⟩
  -- Apply CNOT gates with q as control
  (q', q2') <- cnot q q2
  (q'', q3') <- cnot q' q3
  
  pure (q'', q2', q3')
```

### Bit-Flip Error Correction

```haskell
correctBitFlip :: Qubit %1-> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit, Qubit)
correctBitFlip q1 q2 q3 = withQubits 2 $ \[a1, a2] -> do
  -- Syndrome measurement
  -- CNOT from q1 to a1, q2 to a1 (to check if q1 and q2 are the same)
  (q1', a1_1) <- cnot q1 a1
  (q2', a1_2) <- cnot q2 a1_1
  
  -- CNOT from q2 to a2, q3 to a2 (to check if q2 and q3 are the same)
  (q2'', a2_1) <- cnot q2' a2
  (q3', a2_2) <- cnot q3 a2_1
  
  -- Measure the ancillas to get the syndrome
  (m1, _) <- measure a1_2
  (m2, _) <- measure a2_2
  
  -- Apply corrections based on syndrome
  (q1'', q2''', q3'') <- case (m1, m2) of
    (Zero, Zero) -> pure (q1', q2'', q3')  -- No error
    (One, Zero)  -> do q1'' <- pauliX q1'; pure (q1'', q2'', q3')  -- Error on q1
    (Zero, One)  -> do q3'' <- pauliX q3'; pure (q1', q2'', q3'')  -- Error on q3
    (One, One)   -> do q2''' <- pauliX q2''; pure (q1', q2''', q3')  -- Error on q2
    _            -> pure (q1', q2'', q3')  -- Should not happen in the ideal case
  
  pure (q1'', q2''', q3'')
```

The syndrome measurements determine which qubit has the error:
- (0,0): No error
- (1,0): Error on qubit 1
- (0,1): Error on qubit 3
- (1,1): Error on qubit 2

## Advanced Error Correction Topics

### Error Thresholds

In practice, quantum error correction has a threshold—a minimum error rate below which correction improves the overall reliability. HaskQ's simulator allows you to study these thresholds:

```haskell
-- Study error threshold for the bit-flip code
studyThreshold :: [Double] -> IO ()
studyThreshold errorRates = do
  P.forM_ errorRates $ \rate -> do
    let numTrials = 1000
        results = runThresholdTest rate numTrials
        successRate = (P.fromIntegral $ P.length $ P.filter id results) / 
                      (P.fromIntegral numTrials) * 100
    
    P.putStrLn $ "Error rate: " ++ P.show rate ++ 
                ", Success rate: " ++ P.show successRate ++ "%"

-- Run multiple trials with a given error rate
runThresholdTest :: Double -> Int -> [Bool]
runThresholdTest errorRate numTrials = 
  P.replicate numTrials $ simulateWithErrorRate errorRate
  
-- Simulation with controlled error rate
simulateWithErrorRate :: Double -> Bool
simulateWithErrorRate p = 
  -- ... Implementation depends on HaskQ capabilities
  -- This would introduce errors with probability p and check if correction works
  True  -- Placeholder
```

### Comparing Error Correction Codes

Different codes have different strengths and efficiencies. Let's compare them:

| Code | Protected Errors | Physical Qubits | Logical Qubits | Notes |
|------|------------------|-----------------|----------------|-------|
| Bit-Flip | X errors | 3 | 1 | Good for environments with primarily bit-flip noise |
| Phase-Flip | Z errors | 3 | 1 | Good for environments with primarily phase-flip noise |
| Shor | X, Z, Y errors | 9 | 1 | Complete protection against any single-qubit error |

## Best Practices for Error Correction

1. **Match the code to your noise model**: Choose bit-flip or phase-flip codes if your system has a dominant error type
2. **Consider resource costs**: Higher protection requires more qubits
3. **Simulate with realistic noise**: Add noise to your simulations to test correction effectiveness
4. **Visualize error syndromes**: Understand how different errors produce different measurement patterns
5. **Combine with other techniques**: Use error mitigation techniques alongside correction

## Next Steps

Now that you understand quantum error correction in HaskQ, you might want to explore:

1. [Fault-tolerant quantum computing](./fault-tolerance.md)
2. [Surface codes](./surface-codes.md) (more advanced error correction)
3. [Quantum noise simulation](./noise-models.md)
4. Implementing your own custom error correction codes

Happy error correcting! 