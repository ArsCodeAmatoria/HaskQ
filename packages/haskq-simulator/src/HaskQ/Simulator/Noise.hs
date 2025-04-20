{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HaskQ.Simulator.Noise
--
-- This module provides quantum noise models for realistic simulation.
module HaskQ.Simulator.Noise
  ( NoiseModel(..)
  , NoiseChannel(..)
  , NoiseStrength
  , applyNoiseChannel
  , applyNoiseModel
  , depolarizingNoise
  , bitFlipNoise
  , phaseFlipNoise
  , amplitudeDampingNoise
  , phaseDampingNoise
  , thermalNoise
  , customNoise
  ) where

import HaskQ.Simulator.StateVector
import HaskQ.Simulator.Gates
import qualified HaskQ.Core.Types as Core
import Data.Complex
import qualified Data.Vector as V
import System.Random
import Control.Monad (replicateM)

-- | Noise strength parameter between 0.0 (no noise) and 1.0 (maximum noise)
type NoiseStrength = Double

-- | Types of noise channels for quantum operations
data NoiseChannel
  = BitFlip NoiseStrength         -- ^ X error with probability p
  | PhaseFlip NoiseStrength       -- ^ Z error with probability p
  | BitPhaseFlip NoiseStrength    -- ^ Y error with probability p
  | Depolarizing NoiseStrength    -- ^ Depolarizing channel with probability p
  | AmplitudeDamping NoiseStrength -- ^ Energy dissipation (T1 error) with probability p
  | PhaseDamping NoiseStrength    -- ^ Pure dephasing (T2 error) with probability p
  | Thermal NoiseStrength Double  -- ^ Thermal noise with temperature parameter
  | Custom [(Core.Gate, Double)]  -- ^ Custom noise defined by a list of gates and probabilities
  deriving (Show, Eq)

-- | Noise model configuration for a quantum circuit
data NoiseModel = NoiseModel
  { singleQubitChannels :: [NoiseChannel]  -- ^ Noise channels applied after each single-qubit gate
  , twoQubitChannels :: [NoiseChannel]     -- ^ Noise channels applied after each two-qubit gate
  , measurementError :: NoiseStrength      -- ^ Probability of measurement error
  , idleNoise :: Maybe NoiseChannel        -- ^ Noise applied to idle qubits (per time step)
  , targetQubits :: Maybe [Int]            -- ^ Specific qubits to apply noise to (Nothing = all qubits)
  } deriving (Show, Eq)

-- | Create a depolarizing noise model with uniform error rates
depolarizingNoise :: NoiseStrength -> NoiseModel
depolarizingNoise p = NoiseModel
  { singleQubitChannels = [Depolarizing p]
  , twoQubitChannels = [Depolarizing (p * 2)]  -- Two-qubit operations typically have higher error rates
  , measurementError = p / 2
  , idleNoise = Nothing
  , targetQubits = Nothing
  }

-- | Create a bit flip noise model
bitFlipNoise :: NoiseStrength -> NoiseModel
bitFlipNoise p = NoiseModel
  { singleQubitChannels = [BitFlip p]
  , twoQubitChannels = [BitFlip p]
  , measurementError = p / 2
  , idleNoise = Nothing
  , targetQubits = Nothing
  }

-- | Create a phase flip noise model
phaseFlipNoise :: NoiseStrength -> NoiseModel
phaseFlipNoise p = NoiseModel
  { singleQubitChannels = [PhaseFlip p]
  , twoQubitChannels = [PhaseFlip p]
  , measurementError = p / 2
  , idleNoise = Nothing
  , targetQubits = Nothing
  }

-- | Create an amplitude damping noise model (energy relaxation)
amplitudeDampingNoise :: NoiseStrength -> NoiseModel
amplitudeDampingNoise p = NoiseModel
  { singleQubitChannels = [AmplitudeDamping p]
  , twoQubitChannels = [AmplitudeDamping p]
  , measurementError = p / 2
  , idleNoise = Just (AmplitudeDamping (p / 10))
  , targetQubits = Nothing
  }

-- | Create a phase damping noise model (dephasing)
phaseDampingNoise :: NoiseStrength -> NoiseModel
phaseDampingNoise p = NoiseModel
  { singleQubitChannels = [PhaseDamping p]
  , twoQubitChannels = [PhaseDamping p]
  , measurementError = p / 2
  , idleNoise = Just (PhaseDamping (p / 5))
  , targetQubits = Nothing
  }

-- | Create a thermal noise model
thermalNoise :: NoiseStrength -> Double -> NoiseModel
thermalNoise p temp = NoiseModel
  { singleQubitChannels = [Thermal p temp]
  , twoQubitChannels = [Thermal p temp]
  , measurementError = p / 2
  , idleNoise = Just (Thermal (p / 10) temp)
  , targetQubits = Nothing
  }

-- | Create a custom noise model
customNoise :: [(Core.Gate, Double)] -> NoiseModel
customNoise gateProbs = NoiseModel
  { singleQubitChannels = [Custom gateProbs]
  , twoQubitChannels = [Custom gateProbs]
  , measurementError = 0.0
  , idleNoise = Nothing
  , targetQubits = Nothing
  }

-- | Apply a noise channel to a state vector at a specific qubit
applyNoiseChannel :: NoiseChannel -> Int -> StateVector -> IO StateVector
applyNoiseChannel (BitFlip p) qubit sv = do
  r <- randomIO :: IO Double
  if r < p
    then pure $ applySingleQubitGate pauliXMatrix qubit sv
    else pure sv
    
applyNoiseChannel (PhaseFlip p) qubit sv = do
  r <- randomIO :: IO Double
  if r < p
    then pure $ applySingleQubitGate pauliZMatrix qubit sv
    else pure sv

applyNoiseChannel (BitPhaseFlip p) qubit sv = do
  r <- randomIO :: IO Double
  if r < p
    then pure $ applySingleQubitGate pauliYMatrix qubit sv
    else pure sv

applyNoiseChannel (Depolarizing p) qubit sv = do
  r <- randomIO :: IO Double
  if r < p
    then do
      -- Choose X, Y, or Z error with equal probability
      errorChoice <- randomRIO (0, 2) :: IO Int
      case errorChoice of
        0 -> pure $ applySingleQubitGate pauliXMatrix qubit sv
        1 -> pure $ applySingleQubitGate pauliYMatrix qubit sv
        _ -> pure $ applySingleQubitGate pauliZMatrix qubit sv
    else pure sv

applyNoiseChannel (AmplitudeDamping p) qubit sv = do
  -- Simplified amplitude damping - in reality would use Kraus operators
  -- Here we'll implement a simplified version that approximates amplitude damping
  r <- randomIO :: IO Double
  if r < p
    then do
      -- Project to |0⟩ with probability p if in |1⟩
      -- This is a simplified model - real implementation would use proper Kraus ops
      -- For now, we'll use a projection operation as an approximation
      let proj0Matrix = V.fromList
            [ V.fromList [1 :+ 0, 0 :+ 0]
            , V.fromList [0 :+ 0, 0 :+ 0]
            ]
      pure $ applySingleQubitGate proj0Matrix qubit sv
    else pure sv

applyNoiseChannel (PhaseDamping p) qubit sv = do
  -- Simplified phase damping - in reality would use Kraus operators
  r <- randomIO :: IO Double
  if r < p
    then do
      -- Apply Z with probability p (simplified model)
      pure $ applySingleQubitGate pauliZMatrix qubit sv
    else pure sv

applyNoiseChannel (Thermal p temp) qubit sv = do
  -- Simplified thermal noise model
  -- In a real implementation, would model thermal equilibrium
  r <- randomIO :: IO Double
  if r < p
    then do
      -- Probability of excitation based on temperature
      let pUp = exp (-1.0 / temp)
          pDown = 1.0 - pUp
      
      excite <- randomIO :: IO Double
      if excite < pUp
        then pure $ applySingleQubitGate pauliXMatrix qubit sv
        else pure sv
    else pure sv

applyNoiseChannel (Custom gateProbs) qubit sv = do
  -- Apply one of the gates based on their probabilities
  r <- randomIO :: IO Double
  let applyGateWithProb accum (gate, prob) prev =
        if r >= accum && r < accum + prob
          then case gate of
                 Core.PauliX -> applySingleQubitGate pauliXMatrix qubit sv
                 Core.PauliY -> applySingleQubitGate pauliYMatrix qubit sv
                 Core.PauliZ -> applySingleQubitGate pauliZMatrix qubit sv
                 Core.Hadamard -> applySingleQubitGate hadamardMatrix qubit sv
                 Core.Phase theta -> applySingleQubitGate (phaseMatrix theta) qubit sv
                 _ -> prev  -- Ignore multi-qubit gates in single qubit context
        else prev
  
  -- Fold through gate probabilities, accumulating the probability
  pure $ foldl (\acc (gate, prob) -> applyGateWithProb 0 (gate, prob) acc) sv gateProbs

-- | Apply a noise model to a state vector after a gate operation
applyNoiseModel :: NoiseModel -> [Int] -> StateVector -> IO StateVector
applyNoiseModel model qubits sv = do
  -- Determine which qubits to apply noise to
  let targetQbs = case targetQubits model of
                   Nothing -> qubits  -- Apply to all qubits involved in operation
                   Just tqs -> filter (`elem` tqs) qubits
      
  -- Apply single-qubit channels to each target qubit
  let applyToQubit sv' qubit = 
        foldM (\s channel -> applyNoiseChannel channel qubit s) 
              sv' 
              (if length qubits == 1 
               then singleQubitChannels model 
               else twoQubitChannels model)
              
  -- Apply noise channels sequentially to each qubit
  foldM applyToQubit sv targetQbs 