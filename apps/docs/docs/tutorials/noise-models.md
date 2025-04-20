---
sidebar_position: 7
---

# Quantum Noise Models

This page explains how to simulate quantum noise in HaskQ.

## Overview

Quantum noise models allow you to simulate the effects of realistic noise on quantum circuits, helping to evaluate the performance of quantum algorithms and error correction techniques. HaskQ provides built-in noise models that approximate common types of quantum noise found in real hardware.

## Basic Noise Models

HaskQ supports several standard noise models that can be applied to your quantum circuits:

```haskell
import HaskQ.Simulator.Noise
import HaskQ.Simulator.Circuit (simulateCircuitWithNoise)

-- Create a depolarizing noise model with 5% error rate
let noiseModel = depolarizingNoise 0.05

-- Simulate a circuit with noise
result <- simulateCircuitWithNoise numQubits noiseModel myCircuit
```

### Available Noise Models

HaskQ includes the following built-in noise models:

1. **Depolarizing Noise**: The standard quantum noise model that represents a qubit randomly transitioning to one of the three error states (X, Y, or Z error).

   ```haskell
   depolarizingNoise :: NoiseStrength -> NoiseModel
   ```

2. **Bit-Flip Noise**: Models random X (NOT) errors on qubits.

   ```haskell
   bitFlipNoise :: NoiseStrength -> NoiseModel
   ```

3. **Phase-Flip Noise**: Models random Z (phase) errors on qubits.

   ```haskell
   phaseFlipNoise :: NoiseStrength -> NoiseModel
   ```

4. **Amplitude Damping**: Models energy dissipation (T1 relaxation).

   ```haskell
   amplitudeDampingNoise :: NoiseStrength -> NoiseModel
   ```

5. **Phase Damping**: Models pure dephasing (T2 relaxation).

   ```haskell
   phaseDampingNoise :: NoiseStrength -> NoiseModel
   ```

6. **Thermal Noise**: Models thermal effects with a temperature parameter.

   ```haskell
   thermalNoise :: NoiseStrength -> Double -> NoiseModel
   ```

The `NoiseStrength` parameter is a value between 0.0 (no noise) and 1.0 (maximum noise).

## Using Noise Models in Simulations

To simulate a circuit with noise, use the `simulateCircuitWithNoise` function:

```haskell
-- Create a circuit
myCircuit :: Circ [Measurement]
myCircuit = do
  q1 <- qubit
  q2 <- qubit
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  (m1, _) <- measure q1''
  (m2, _) <- measure q2'
  pure [m1, m2]

-- Define a noise model (5% depolarizing noise)
let noiseModel = depolarizingNoise 0.05

-- Simulate with noise
result <- simulateCircuitWithNoise 2 noiseModel myCircuit
```

### Noise Model Impact

When applying a noise model:

1. **Gate Errors**: Each gate operation may introduce errors according to the noise model
2. **Measurement Errors**: Measurement results can be flipped with some probability
3. **Idle Noise**: Qubits that aren't being actively operated on can experience decoherence

### Example: Bell State with Noise

Here's an example showing how a Bell state circuit's output distribution is affected by noise:

```haskell
-- Bell state circuit
bellState :: Circ [Measurement]
bellState = do
  q1 <- qubit
  q2 <- qubit
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  (m1, _) <- measure q1''
  (m2, _) <- measure q2'
  pure [m1, m2]

main :: IO ()
main = do
  -- Create noise model with 5% error rate
  let noise = depolarizingNoise 0.05
  
  -- Run 1000 times and gather statistics
  replicateM_ 1000 $ do
    result <- simulateCircuitWithNoise 2 noise bellState
    print (measurements result)
```

Without noise, a Bell state circuit would only produce |00⟩ and |11⟩ states with equal probability. With noise, you'll also observe some |01⟩ and |10⟩ states due to errors.

## Custom Noise Models

You can create custom noise models by specifying:

```haskell
data NoiseModel = NoiseModel
  { singleQubitChannels :: [NoiseChannel]  -- ^ Noise channels applied after each single-qubit gate
  , twoQubitChannels :: [NoiseChannel]     -- ^ Noise channels applied after each two-qubit gate
  , measurementError :: NoiseStrength      -- ^ Probability of measurement error
  , idleNoise :: Maybe NoiseChannel        -- ^ Noise applied to idle qubits (per time step)
  , targetQubits :: Maybe [Int]            -- ^ Specific qubits to apply noise to (Nothing = all qubits)
  }
```

Example of a custom noise model:

```haskell
-- Custom noise model with different error types
let customModel = NoiseModel
      { singleQubitChannels = [BitFlip 0.01, PhaseFlip 0.02]  -- Both bit and phase errors on single-qubit gates
      , twoQubitChannels = [Depolarizing 0.05]               -- Depolarizing noise on two-qubit gates
      , measurementError = 0.03                              -- 3% measurement error
      , idleNoise = Just (AmplitudeDamping 0.005)            -- Small amplitude damping on idle qubits
      , targetQubits = Just [0, 1]                           -- Only apply to qubits 0 and 1
      }
```

## Noise Model Example Program

HaskQ includes a complete example program that demonstrates noise models in action:

```bash
# Run the noise model example
cabal run haskq-simulator:noise-model-example -- 0.05 depolarizing 1000
```

Command-line parameters:
1. Noise strength (default: 0.05)
2. Noise type: depolarizing, bitflip, phaseflip, amplitude, phase, thermal (default: depolarizing)
3. Number of runs (default: 1000)
4. Optional: Add "ghz" to also run GHZ state example
5. Optional: GHZ state size (default: 3 qubits)

## Understanding Fidelity

The example program calculates the **fidelity** of the output state compared to the ideal case. For a Bell state, the fidelity is the proportion of measurements that give the expected |00⟩ and |11⟩ outcomes.

A fidelity of 1.0 represents a perfect circuit with no errors, while lower values indicate increasing noise effects. 