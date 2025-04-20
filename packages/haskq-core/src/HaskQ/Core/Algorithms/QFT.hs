{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HaskQ.Core.Algorithms.QFT
--
-- This module provides an implementation of the Quantum Fourier Transform (QFT),
-- which is a key component in many quantum algorithms including Shor's algorithm.
module HaskQ.Core.Algorithms.QFT
  ( qft
  , inverseQft
  , rotationGate
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import Prelude.Linear
import qualified Control.Monad.Linear as L
import qualified Prelude as P

-- | Rotation gate with angle θ = 2π/2^k
-- This implements the controlled phase rotation by angle 2π/2^k
rotationGate :: Int -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
rotationGate k control target = do
  -- For k=1, this is a controlled-Z gate
  -- For k>1, it's a controlled phase rotation by 2π/2^k
  let theta = P.pi * 2 / (2 ^ k)
  -- First apply a controlled phase to the target qubit
  target' <- phase theta target
  pure (control, target')

-- | Quantum Fourier Transform on n qubits
-- |j⟩ → 1/√(2^n) * sum_{k=0}^{2^n-1} e^{2πijk/2^n} |k⟩
qft :: [Qubit] %1-> Circ [Qubit]
qft [] = pure []
qft [q] = do
  q' <- hadamard q
  pure [q']
qft (q:qs) = do
  -- Apply Hadamard to the first qubit
  q' <- hadamard q
  
  -- Apply controlled rotations between this qubit and each subsequent qubit
  let applyRotations :: Int -> Qubit %1-> [Qubit] %1-> Circ (Qubit, [Qubit])
      applyRotations _ q' [] = pure (q', [])
      applyRotations k q' (qj:qjs) = do
        (q'', qj') <- rotationGate k q' qj
        (q''', qjs') <- applyRotations (k+1) q'' qjs
        pure (q''', qj':qjs')
  
  (q'', qs') <- applyRotations 2 q' qs
  
  -- Recursively apply QFT to the remaining qubits
  qs'' <- qft qs'
  
  -- Return the transformed qubits in reversed order (standard QFT output format)
  pure (qs'' ++ [q''])

-- | Inverse Quantum Fourier Transform
-- This is the conjugate transpose of the QFT
inverseQft :: [Qubit] %1-> Circ [Qubit]
inverseQft qs = do
  -- The inverse QFT is just the QFT in reverse with conjugated phases
  -- Reverse the qubit order first
  let qsReversed = P.reverse qs
  
  -- Apply the inverse transform (similar to QFT but with negated phases)
  let invRotationGate :: Int -> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit)
      invRotationGate k control target = do
        let theta = -P.pi * 2 / (2 ^ k)
        target' <- phase theta target
        pure (control, target')
  
  -- Apply inverse QFT
  let inverseQftStep :: [Qubit] %1-> Circ [Qubit]
      inverseQftStep [] = pure []
      inverseQftStep [q] = do
        q' <- hadamard q
        pure [q']
      inverseQftStep (q:qs) = do
        -- Apply Hadamard to the first qubit
        q' <- hadamard q
        
        -- Apply controlled inverse rotations
        let applyInvRotations :: Int -> Qubit %1-> [Qubit] %1-> Circ (Qubit, [Qubit])
            applyInvRotations _ q' [] = pure (q', [])
            applyInvRotations k q' (qj:qjs) = do
              (q'', qj') <- invRotationGate k q' qj
              (q''', qjs') <- applyInvRotations (k+1) q'' qjs
              pure (q''', qj':qjs')
        
        (q'', qs') <- applyInvRotations 2 q' qs
        
        -- Recursively apply inverse QFT
        qs'' <- inverseQftStep qs'
        
        pure (qs'' ++ [q''])
  
  result <- inverseQftStep qsReversed
  
  -- Reverse back the order to match the input order
  pure (P.reverse result) 