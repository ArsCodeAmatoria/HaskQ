{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HaskQ.Core.Algorithms.Grover
--
-- This module provides an implementation of Grover's Search Algorithm,
-- which provides a quadratic speedup for searching unstructured databases.
module HaskQ.Core.Algorithms.Grover
  ( groverSearch
  , diffusion
  , oracle
  , OracleFunction
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import Prelude.Linear
import qualified Control.Monad.Linear as L
import qualified Prelude as P

-- | Oracle function type
-- A function that marks target states by applying a phase flip
type OracleFunction = [Qubit] %1-> Circ [Qubit]

-- | Oracle for Grover's algorithm
-- This applies a phase flip (-1) to the marked state(s)
-- The provided function should implement the specific oracle logic
oracle :: OracleFunction -> [Qubit] %1-> Circ [Qubit]
oracle f qubits = f qubits

-- | Diffusion operator (Grover's diffusion)
-- This performs the reflection about the average amplitude
diffusion :: [Qubit] %1-> Circ [Qubit]
diffusion qubits = do
  -- Apply Hadamard to all qubits
  qubits' <- applyToAll hadamard qubits
  
  -- Apply phase flip to all states except |00...0⟩
  qubits'' <- phaseFlipNonZero qubits'
  
  -- Apply Hadamard to all qubits again
  qubits''' <- applyToAll hadamard qubits''
  
  pure qubits'''
  where
    -- Apply a gate to all qubits
    applyToAll :: (Qubit %1-> Circ Qubit) -> [Qubit] %1-> Circ [Qubit]
    applyToAll _ [] = pure []
    applyToAll gate (q:qs) = do
      q' <- gate q
      qs' <- applyToAll gate qs
      pure (q' : qs')
    
    -- Phase flip on all states except |00...0⟩
    -- This is typically implemented as a multi-controlled Z gate with X gates before and after
    phaseFlipNonZero :: [Qubit] %1-> Circ [Qubit]
    phaseFlipNonZero [] = pure []
    phaseFlipNonZero qs = do
      -- Apply X to all qubits
      qs' <- applyToAll pauliX qs
      
      -- Apply multi-controlled Z
      -- For simplicity, we'll implement this using a sequence of CNOT and phase gates
      let lastQubit = P.last qs'
          restQubits = P.init qs'
      
      -- Apply multi-controlled phase to |11...1⟩
      qs'' <- applyMultiControlledPhase qs'
      
      -- Apply X to all qubits again
      qs''' <- applyToAll pauliX qs''
      
      pure qs'''
    
    -- Apply a multi-controlled phase gate (phase flip on |11...1⟩)
    applyMultiControlledPhase :: [Qubit] %1-> Circ [Qubit]
    applyMultiControlledPhase [] = pure []
    applyMultiControlledPhase [q] = do
      q' <- pauliZ q
      pure [q']
    applyMultiControlledPhase (q:qs) = do
      -- We'll implement it recursively
      -- Apply a controlled operation from q to the result of multi-controlled phase on qs
      qs' <- applyMultiControlledPhase qs
      
      -- Controls with the first qubit
      let controlPhase :: Qubit %1-> [Qubit] %1-> Circ (Qubit, [Qubit])
          controlPhase ctrl [] = pure (ctrl, [])
          controlPhase ctrl (target:rest) = do
            (ctrl', target') <- cnot ctrl target
            (ctrl'', rest') <- controlPhase ctrl' rest
            pure (ctrl'', target':rest')
      
      (q', qs'') <- controlPhase q qs'
      pure (q' : qs'')

-- | Grover's search algorithm
-- Finds a marked element in an unstructured database with O(√N) operations
-- numQubits: number of qubits (database size is 2^numQubits)
-- numIterations: number of Grover iterations (approximately π/4 * √N for a single marked item)
-- oracleFn: oracle function that marks the target states with a phase flip
groverSearch :: Int -> Int -> OracleFunction -> Circ [Qubit]
groverSearch numQubits numIterations oracleFn = do
  -- Initialize qubits in |0⟩ state
  qubits <- withQubits numQubits (\qs -> pure qs)
  
  -- Apply Hadamard to create equal superposition
  qubits' <- applyToAll hadamard qubits
  
  -- Apply Grover iterations
  qubits'' <- applyIterations numIterations qubits'
  
  pure qubits''
  
  where
    -- Apply a gate to all qubits
    applyToAll :: (Qubit %1-> Circ Qubit) -> [Qubit] %1-> Circ [Qubit]
    applyToAll _ [] = pure []
    applyToAll gate (q:qs) = do
      q' <- gate q
      qs' <- applyToAll gate qs
      pure (q' : qs')
    
    -- Apply iterations of Grover's algorithm
    applyIterations :: Int -> [Qubit] %1-> Circ [Qubit]
    applyIterations 0 qs = pure qs
    applyIterations n qs = do
      -- Apply oracle
      qs' <- oracle oracleFn qs
      
      -- Apply diffusion
      qs'' <- diffusion qs'
      
      -- Recursive call for remaining iterations
      applyIterations (n-1) qs''

-- | Example oracle for finding the state |101⟩
-- This can be replaced with any custom oracle
exampleOracle :: [Qubit] %1-> Circ [Qubit]
exampleOracle qubits
  | P.length qubits < 3 = pure qubits  -- Not enough qubits
  | otherwise = do
      -- Apply X gates to the qubits that should be |0⟩ in the target state
      qubits' <- applyXToIndex 1 qubits  -- Apply X to the second qubit (0-indexed)
      
      -- Apply controlled-Z using all qubits as control
      qubits'' <- applyMultiControlledZ qubits'
      
      -- Apply X gates again to restore the original state
      qubits''' <- applyXToIndex 1 qubits''
      
      pure qubits'''
  
  where
    -- Apply X gate to a qubit at a specific index
    applyXToIndex :: Int -> [Qubit] %1-> Circ [Qubit]
    applyXToIndex _ [] = pure []
    applyXToIndex 0 (q:qs) = do
      q' <- pauliX q
      pure (q' : qs)
    applyXToIndex i (q:qs) = do
      qs' <- applyXToIndex (i-1) qs
      pure (q : qs')
    
    -- Apply a multi-controlled Z gate
    applyMultiControlledZ :: [Qubit] %1-> Circ [Qubit]
    applyMultiControlledZ [] = pure []
    applyMultiControlledZ [q] = do
      q' <- pauliZ q
      pure [q']
    applyMultiControlledZ (q:qs) = do
      qs' <- applyMultiControlledZ qs
      
      -- Control with the first qubit
      let controlZ :: Qubit %1-> [Qubit] %1-> Circ (Qubit, [Qubit])
          controlZ ctrl [] = pure (ctrl, [])
          controlZ ctrl (target:rest) = do
            (ctrl', target') <- cnot ctrl target
            target'' <- pauliZ target'
            (ctrl'', rest') <- controlZ ctrl' rest
            pure (ctrl'', target'':rest')
      
      (q', qs'') <- controlZ q qs'
      pure (q' : qs'') 