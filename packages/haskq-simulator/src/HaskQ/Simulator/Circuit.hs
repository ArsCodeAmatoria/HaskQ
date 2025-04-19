{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module HaskQ.Simulator.Circuit
  ( SimState
  , initializeState
  , runSimulation
  , simulateCircuit
  , applyOperation
  , extractMeasurement
  , createQubit
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
import Data.Bifunctor (first)

-- | The simulation state containing the current state vector and qubit mappings
data SimState = SimState
  { stateVector :: StateVector
  , qubits :: [Int]  -- Maps logical qubits to physical qubits in the state vector
  , nextQubit :: Int -- Next qubit index to allocate
  , measurements :: [Measurement]
  } deriving (Show)

-- | Initialize a simulation state for n qubits
initializeState :: Int -> SimState
initializeState n = SimState
  { stateVector = createStateVector n
  , qubits = []  -- No qubits allocated initially
  , nextQubit = 0
  , measurements = []
  }

-- | Create a new qubit in the |0⟩ state
createQubit :: SimState %1-> (SimState, Qubit)
createQubit state =
  let qubitIdx = nextQubit state
      -- Dummy Qubit value - in a real implementation this would be a proper reference
      qubit = undefined :: Qubit
      newState = state { qubits = qubits state ++ [qubitIdx]
                       , nextQubit = qubitIdx + 1
                       }
  in (newState, qubit)

-- | Mapping from Core.CircuitState to SimState
-- This is a mock implementation since we don't have access to the actual Core.CircuitState
fromCoreState :: Core.CircuitState -> SimState
fromCoreState _ = initializeState 10  -- Default to 10 qubits

-- | Convert from SimState to Core.CircuitState (placeholder)
toCoreState :: SimState -> Core.CircuitState
toCoreState _ = undefined

-- Wrapper for Core.Circ to intercept operations
wrapCircuit :: (forall a. Core.Circ a %1-> (SimState, a)) -> Core.Circ b %1-> (Core.CircuitState, b)
wrapCircuit f circ = 
  let 
    (simState, result) = f circ
  in
    (toCoreState simState, result)

-- | Run a quantum circuit simulation
runSimulation :: Int -> Circ a %1-> (SimState, a)
runSimulation numQubits (Circ circuit) =
  let 
    initialState = initializeState numQubits
    
    -- Interpreter for Circ operations
    interpret :: Circ a %1-> (SimState, a)
    interpret (Circ c) = 
      let 
        wrappedC = wrapCircuit interpret
        -- This is the trick: we replace the Core.CircuitState with our SimState
        simulatedC simState = c undefined  -- We use undefined as a dummy value
      in
        case c of
          -- Handle the qubit creation operation
          Core.qubit -> createQubit initialState
            
          -- Handle gate application
          Core.applyGate gate qubit ->
            let 
              -- Find the qubit index
              qubitIdx = 0  -- In a real implementation, we would look up the qubit index
              
              -- Apply the gate to the state vector
              newState = applyOperation gate qubitIdx initialState
            in
              (newState, qubit)  -- Return the qubit unchanged (in a real implementation it would be updated)
            
          -- Handle measurement
          Core.measure qubit ->
            let 
              -- Find the qubit index
              qubitIdx = 0  -- In a real implementation, we would look up the qubit index
              
              -- Perform the measurement
              (result, newSv) = extractMeasurement qubitIdx (stateVector initialState)
              
              -- Update the state
              newState = initialState { stateVector = newSv, measurements = measurements initialState ++ [result] }
            in
              (newState, (result, qubit))  -- Return the measurement result and the qubit
              
          -- For all other operations, we just pass through
          _ -> (initialState, simulatedC initialState)
  in
    interpret (Circ circuit)

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
      qubitIdx = if not (null (qubits state)) then qubits state !! targetQubit else targetQubit
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