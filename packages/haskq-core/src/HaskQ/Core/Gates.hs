{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module HaskQ.Core.Gates
  ( hadamard
  , pauliX
  , pauliY
  , pauliZ
  , cnot
  , swap
  , phase
  , measure
  ) where

import HaskQ.Core.Types
import Prelude.Linear
import qualified Prelude as P

-- | Apply Hadamard gate to a qubit
-- H = 1/√2 * [1  1]
--             [1 -1]
hadamard :: Qubit %1-> Circ Qubit
hadamard q = Circ $ \s %1-> 
  (s, q) -- Actual implementation in simulator

-- | Apply Pauli-X (NOT) gate to a qubit
-- X = [0 1]
--     [1 0]
pauliX :: Qubit %1-> Circ Qubit
pauliX q = Circ $ \s %1-> 
  (s, q) -- Actual implementation in simulator

-- | Apply Pauli-Y gate to a qubit
-- Y = [0 -i]
--     [i  0]
pauliY :: Qubit %1-> Circ Qubit
pauliY q = Circ $ \s %1-> 
  (s, q) -- Actual implementation in simulator

-- | Apply Pauli-Z gate to a qubit
-- Z = [1  0]
--     [0 -1]
pauliZ :: Qubit %1-> Circ Qubit
pauliZ q = Circ $ \s %1-> 
  (s, q) -- Actual implementation in simulator

-- | Apply phase rotation gate to a qubit
-- Phase(θ) = [1 0]
--            [0 e^(iθ)]
phase :: Double -> Qubit %1-> Circ Qubit
phase _ q = Circ $ \s %1-> 
  (s, q) -- Actual implementation in simulator

-- | Apply CNOT (controlled-NOT) gate to a control and target qubit
-- CNOT = [1 0 0 0]
--        [0 1 0 0]
--        [0 0 0 1]
--        [0 0 1 0]
cnot :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
cnot control target = Circ $ \s %1-> 
  (s, (control, target)) -- Actual implementation in simulator

-- | Apply SWAP gate to two qubits
swap :: Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
swap q1 q2 = Circ $ \s %1-> 
  (s, (q2, q1)) -- Actual implementation in simulator

-- | Measure a qubit, collapsing its state
-- Returns the measurement result and the collapsed qubit
measure :: Qubit %1-> Circ (Measurement, Qubit)
measure q = Circ $ \s %1-> 
  (s, (Zero, q)) -- Actual implementation in simulator 