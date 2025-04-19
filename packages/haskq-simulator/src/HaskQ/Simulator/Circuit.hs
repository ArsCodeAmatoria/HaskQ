{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module HaskQ.Simulator.Circuit
  ( SimState
  , initializeState
  , runSimulation
  , simulateCircuit
  , applyOperation
  , extractMeasurement
  ) where

import qualified HaskQ.Core.Types as Core
import HaskQ.Core.Types (Circ(..), Qubit, Measurement(..), CircuitOutput(..))
import HaskQ.Simulator.StateVector
import HaskQ.Simulator.Gates
import qualified Data.Vector as V
import Data.Complex
import qualified Prelude as P
import Prelude.Linear
import Control.Monad.Linear

-- | The simulation state containing the current state vector and qubit mappings
data SimState = SimState
  { stateVector :: StateVector
  , qubits :: [Int]  -- Maps logical qubits to physical qubits in the state vector
  , measurements :: [Measurement]
  } deriving (Show)

-- | Initialize a simulation state for n qubits
initializeState :: Int -> SimState
initializeState n = SimState
  { stateVector = createStateVector n
  , qubits = [0..(n-1)]
  , measurements = []
  }

-- | Run a quantum circuit simulation
runSimulation :: Int -> Circ a %1-> (SimState, a)
runSimulation numQubits (Circ circuit) =
  circuit (undefined :: Core.CircuitState) -- Actual implementation needed

-- | Simulate a circuit and get the output
simulateCircuit :: Int -> Circ a %1-> CircuitOutput a
simulateCircuit numQubits circ =
  let (simState, result) = runSimulation numQubits circ
  in CircuitOutput
    { Core.measurements = measurements simState
    , Core.result = result
    }

-- | Apply a quantum operation to the simulation state
applyOperation :: Core.Gate -> Int -> SimState -> SimState
applyOperation gate targetQubit state =
  let sv = stateVector state
      qubitIdx = qubits state !! targetQubit
      newSv = applyGate gate sv
  in state { stateVector = newSv }

-- | Extract measurement result from a qubit in a state vector
extractMeasurement :: Int -> StateVector -> (Measurement, StateVector)
extractMeasurement qubitIdx sv =
  let probs = probabilities sv
      -- Calculate probability of measuring 1 for the specific qubit
      probOne = calculateProbOne qubitIdx sv
  in if probOne > 0.999
     then (One, sv)
     else if probOne < 0.001
     then (Zero, sv)
     else (Superposition probOne, sv)

-- | Calculate the probability of measuring the qubit in state |1⟩
calculateProbOne :: Int -> StateVector -> Double
calculateProbOne qubitIdx sv =
  let n = numQubits sv
      -- Sum probabilities of all basis states where the qubit is 1
      mask = 1 `P.shift` qubitIdx
      probSum = V.sum $ V.ifilter (\i _ -> (i P..&. mask) /= 0) (probabilities sv)
  in probSum 