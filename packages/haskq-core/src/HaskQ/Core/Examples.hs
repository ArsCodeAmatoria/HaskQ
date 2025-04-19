{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module HaskQ.Core.Examples
  ( bellState
  , ghzState
  , teleport
  , deutschJozsa
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import Prelude.Linear
import qualified Control.Monad.Linear as L

-- | Create a Bell state (maximally entangled two-qubit state)
-- |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')

-- | Create a GHZ state (maximally entangled three-qubit state)
-- |GHZ⟩ = 1/√2 (|000⟩ + |111⟩)
ghzState :: Circ (Qubit, Qubit, Qubit)
ghzState = withQubits 3 $ \[q1, q2, q3] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  (q1''', q3') <- cnot q1'' q3
  pure (q1''', q2', q3')

-- | Quantum teleportation circuit
-- Transfers the state of one qubit to another using entanglement and classical communication
teleport :: Qubit %1-> Circ Qubit
teleport q_in = do
  -- Create Bell pair
  (alice, bob) <- bellState
  
  -- Entangle input with Alice's qubit
  (q_in', alice') <- cnot q_in alice
  q_in'' <- hadamard q_in'
  
  -- Measure qubits (simulates classical communication)
  (m1, _) <- measure q_in''
  (m2, _) <- measure alice'
  
  -- Apply corrections based on measurement results
  bob' <- case (m1, m2) of
    (Zero, Zero) -> pure bob
    (Zero, One)  -> pauliX bob
    (One, Zero)  -> pauliZ bob
    (One, One)   -> pauliZ bob L.>>= pauliX
    _            -> pure bob
    
  pure bob'

-- | Deutsch-Jozsa algorithm
-- Determines if a function is constant or balanced with a single query
deutschJozsa :: Circ Measurement
deutschJozsa = withQubits 2 $ \[q1, q2] -> do
  -- Prepare qubits
  q1' <- hadamard q1
  q2' <- pauliX q2 L.>>= hadamard
  
  -- Apply oracle (in this example, a CNOT gate representing a balanced function)
  (q1'', q2'') <- cnot q1' q2'
  
  -- Measure
  q1''' <- hadamard q1''
  (result, _) <- measure q1'''
  
  pure result
  -- Result interpretation:
  -- Zero => function is constant
  -- One => function is balanced 