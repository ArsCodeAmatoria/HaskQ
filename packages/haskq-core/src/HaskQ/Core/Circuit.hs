{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskQ.Core.Circuit
  ( circuitId
  , circuit
  , withQubit
  , withQubits
  , applyGate
  , Circuit
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import Prelude.Linear
import Control.Monad.Linear
import qualified Control.Monad as P

-- | Identity circuit - returns input unchanged
circuitId :: Qubit %1-> Circ Qubit
circuitId q = pure q

-- | Circuit constructor from a function operating on qubits
circuit :: (Qubit %1-> Circ Qubit) -> Circuit Qubit
circuit f = do
  q <- qubit
  f q

-- | Create a circuit with a single qubit and apply an operation
withQubit :: (Qubit %1-> Circ a) %1-> Circ a
withQubit f = do
  q <- qubit
  f q

-- | Create a circuit with multiple qubits and apply an operation
withQubits :: Int -> ([Qubit] %1-> Circ a) %1-> Circ a
withQubits 0 f = f []
withQubits n f = withQubit $ \q -> 
  withQubits (n-1) $ \qs -> 
    f (q:qs)

-- | Apply a gate to a qubit inside the circuit
applyGate :: Gate -> Qubit %1-> Circ Qubit
applyGate Hadamard = hadamard
applyGate PauliX = pauliX
applyGate PauliY = pauliY
applyGate PauliZ = pauliZ
applyGate (Phase theta) = phase theta
applyGate _ = circuitId -- Other gates handled differently 