{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module HaskQ.Core.Measurement
  ( measure
  , measureAll
  , probabilityOfOne
  , expectationValue
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates (measure)
import Prelude.Linear
import qualified Prelude as P

-- | Measure all qubits in a list
measureAll :: [Qubit] %1-> Circ ([Measurement], [Qubit])
measureAll [] = pure ([], [])
measureAll (q:qs) = do
  (m, q') <- measure q
  (ms, qs') <- measureAll qs
  pure (m:ms, q':qs')

-- | Calculate probability of measuring |1⟩ from a measurement result
probabilityOfOne :: Measurement -> Double
probabilityOfOne Zero = 0.0
probabilityOfOne One = 1.0
probabilityOfOne (Superposition p) = p

-- | Calculate expectation value of a measurement
-- For the computational basis, |0⟩ has value 0, |1⟩ has value 1
expectationValue :: Measurement -> Double
expectationValue = probabilityOfOne 