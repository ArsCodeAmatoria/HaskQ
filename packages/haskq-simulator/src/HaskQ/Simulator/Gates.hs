{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskQ.Simulator.Gates
  ( hadamardMatrix
  , pauliXMatrix
  , pauliYMatrix
  , pauliZMatrix
  , phaseMatrix
  , cnotMatrix
  , swapMatrix
  , applyGate
  , applySingleQubitGate
  , applyTwoQubitGate
  ) where

import HaskQ.Simulator.StateVector
import HaskQ.Core.Types (Gate(..))
import qualified Data.Vector as V
import Data.Complex
import qualified Prelude as P
import Prelude.Linear hiding ((+), (*), (/), negate)
import Prelude ((+), (*), (/), negate)

-- | 2x2, Single-qubit gate matrices

-- | Hadamard gate matrix
-- H = 1/√2 * [1  1]
--             [1 -1]
hadamardMatrix :: V.Vector (V.Vector (Complex Double))
hadamardMatrix = V.fromList
  [ V.fromList [ 1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0 ]
  , V.fromList [ 1/sqrt 2 :+ 0, -1/sqrt 2 :+ 0 ]
  ]

-- | Pauli-X (NOT) gate matrix
-- X = [0 1]
--     [1 0]
pauliXMatrix :: V.Vector (V.Vector (Complex Double))
pauliXMatrix = V.fromList
  [ V.fromList [ 0 :+ 0, 1 :+ 0 ]
  , V.fromList [ 1 :+ 0, 0 :+ 0 ]
  ]

-- | Pauli-Y gate matrix
-- Y = [0 -i]
--     [i  0]
pauliYMatrix :: V.Vector (V.Vector (Complex Double))
pauliYMatrix = V.fromList
  [ V.fromList [ 0 :+ 0, 0 :+ (-1) ]
  , V.fromList [ 0 :+ 1, 0 :+ 0 ]
  ]

-- | Pauli-Z gate matrix
-- Z = [1  0]
--     [0 -1]
pauliZMatrix :: V.Vector (V.Vector (Complex Double))
pauliZMatrix = V.fromList
  [ V.fromList [ 1 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, -1 :+ 0 ]
  ]

-- | Phase gate matrix
-- Phase(θ) = [1 0]
--            [0 e^(iθ)]
phaseMatrix :: Double -> V.Vector (V.Vector (Complex Double))
phaseMatrix theta = V.fromList
  [ V.fromList [ 1 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, cos theta :+ sin theta ]
  ]

-- | 4x4, Two-qubit gate matrices

-- | CNOT gate matrix
-- CNOT = [1 0 0 0]
--        [0 1 0 0]
--        [0 0 0 1]
--        [0 0 1 0]
cnotMatrix :: V.Vector (V.Vector (Complex Double))
cnotMatrix = V.fromList
  [ V.fromList [ 1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, 1 :+ 0, 0 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, 0 :+ 0, 0 :+ 0, 1 :+ 0 ]
  , V.fromList [ 0 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0 ]
  ]

-- | SWAP gate matrix
-- SWAP = [1 0 0 0]
--        [0 0 1 0]
--        [0 1 0 0]
--        [0 0 0 1]
swapMatrix :: V.Vector (V.Vector (Complex Double))
swapMatrix = V.fromList
  [ V.fromList [ 1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, 1 :+ 0, 0 :+ 0, 0 :+ 0 ]
  , V.fromList [ 0 :+ 0, 0 :+ 0, 0 :+ 0, 1 :+ 0 ]
  ]

-- | Apply a gate to a state vector based on the gate type
applyGate :: Gate -> StateVector -> StateVector
applyGate Hadamard = applySingleQubitGate hadamardMatrix 0
applyGate PauliX = applySingleQubitGate pauliXMatrix 0
applyGate PauliY = applySingleQubitGate pauliYMatrix 0
applyGate PauliZ = applySingleQubitGate pauliZMatrix 0
applyGate (Phase theta) = applySingleQubitGate (phaseMatrix theta) 0
applyGate CNOT = applyTwoQubitGate cnotMatrix 0 1
applyGate Swap = applyTwoQubitGate swapMatrix 0 1
applyGate Measure = id -- Measurement handled separately

-- | Apply a single-qubit gate to a specific qubit in the state vector
applySingleQubitGate :: V.Vector (V.Vector (Complex Double)) -> Int -> StateVector -> StateVector
applySingleQubitGate matrix targetQubit stateVector =
  let n = numQubits stateVector
      fullMatrix = expandMatrix matrix targetQubit n
  in applyMatrix fullMatrix stateVector

-- | Apply a two-qubit gate to specific qubits in the state vector
applyTwoQubitGate :: V.Vector (V.Vector (Complex Double)) -> Int -> Int -> StateVector -> StateVector
applyTwoQubitGate matrix controlQubit targetQubit stateVector =
  let n = numQubits stateVector
      fullMatrix = expandTwoQubitMatrix matrix controlQubit targetQubit n
  in applyMatrix fullMatrix stateVector

-- | Expand a single-qubit matrix to operate on the full state space
expandMatrix :: V.Vector (V.Vector (Complex Double)) -> Int -> Int -> V.Vector (V.Vector (Complex Double))
expandMatrix matrix targetQubit n = undefined -- Implementation omitted for brevity

-- | Expand a two-qubit matrix to operate on the full state space
expandTwoQubitMatrix :: V.Vector (V.Vector (Complex Double)) -> Int -> Int -> Int -> V.Vector (V.Vector (Complex Double))
expandTwoQubitMatrix matrix controlQubit targetQubit n = undefined -- Implementation omitted for brevity 