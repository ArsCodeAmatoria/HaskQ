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
expandMatrix matrix targetQubit n = 
  let dim = 2^n  -- Dimension of the full state space
      
      -- Helper function to determine if bit at position 'pos' is set in number 'x'
      bitIsSet x pos = (x P..&. (1 P.shift pos)) /= 0
      
      -- Create the full matrix
      fullMatrix = V.generate dim $ \i -> 
        V.generate dim $ \j -> 
          if bitAt i targetQubit == bitAt j targetQubit
          then 1 :+ 0  -- Identity for bits that don't match the target
          else
            let 
              -- Extract the relevant bits for the matrix lookup
              iTargetBit = if bitIsSet i targetQubit then 1 else 0
              jTargetBit = if bitIsSet j targetQubit then 1 else 0
              
              -- Check if all other bits are the same (identity operation on other qubits)
              otherBitsSame = (i `P.xor` j) == (1 P.shift targetQubit)
            in
              if otherBitsSame 
              then matrix V.! iTargetBit V.! jTargetBit
              else 0 :+ 0  -- Zero for elements that don't match the pattern
  in fullMatrix

-- | Extract the bit value at a specific position
bitAt :: Int -> Int -> Int
bitAt num pos = if (num P..&. (1 P.shift pos)) /= 0 then 1 else 0

-- | Expand a two-qubit matrix to operate on the full state space
expandTwoQubitMatrix :: V.Vector (V.Vector (Complex Double)) -> Int -> Int -> Int -> V.Vector (V.Vector (Complex Double))
expandTwoQubitMatrix matrix controlQubit targetQubit n =
  let dim = 2^n  -- Dimension of the full state space
      
      -- Helper function to determine if bit at position 'pos' is set in number 'x'
      bitIsSet x pos = (x P..&. (1 P.shift pos)) /= 0
      
      -- Create the full matrix
      fullMatrix = V.generate dim $ \i -> 
        V.generate dim $ \j -> 
          let 
            -- Extract the relevant bits for the control and target qubits
            iControlBit = bitAt i controlQubit
            iTargetBit = bitAt i targetQubit
            jControlBit = bitAt j controlQubit
            jTargetBit = bitAt j targetQubit
            
            -- Calculate the index in the 4x4 matrix
            matrixI = iControlBit * 2 + iTargetBit
            matrixJ = jControlBit * 2 + jTargetBit
            
            -- Check if all other bits are the same
            otherBitsSame = ((i `P.xor` j) P..&. P.complement ((1 P.shift controlQubit) P..|. (1 P.shift targetQubit))) == 0
          in
            if otherBitsSame
            then matrix V.! matrixI V.! matrixJ
            else if i == j  -- Identity for unaffected states
              then 1 :+ 0
              else 0 :+ 0
  in fullMatrix 