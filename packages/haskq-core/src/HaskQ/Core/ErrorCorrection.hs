{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

-- | HaskQ.Core.ErrorCorrection
--
-- This module provides quantum error correction codes and utilities.
module HaskQ.Core.ErrorCorrection
  ( 
    -- * Bit-flip code
    encodeBitFlip
  , decodeBitFlip
  , correctBitFlip
    
    -- * Phase-flip code
  , encodePhaseFlip
  , decodePhaseFlip
  , correctPhaseFlip
    
    -- * Shor code (combines bit and phase flip)
  , encodeShor
  , decodeShor
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import qualified Control.Monad.Linear as L

-- | Encode a logical qubit using the three-qubit bit-flip code
-- |ψ⟩ -> α|000⟩ + β|111⟩
encodeBitFlip :: Qubit %1-> Circ (Qubit, Qubit, Qubit)
encodeBitFlip q = withQubits 2 $ \[q2, q3] -> do
  -- Create two ancilla qubits initialized to |0⟩
  -- Apply CNOT gates with q as control
  (q', q2') <- cnot q q2
  (q'', q3') <- cnot q' q3
  
  pure (q'', q2', q3')

-- | Decode a logical qubit from the three-qubit bit-flip code
decodeBitFlip :: Qubit %1-> Qubit %1-> Qubit %1-> Circ Qubit
decodeBitFlip q1 q2 q3 = do
  -- Majority voting is implicit in the error correction
  -- To decode, just return the first qubit, as the error has already been corrected
  pure q1

-- | Correct errors in a bit-flip encoded state
correctBitFlip :: Qubit %1-> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit, Qubit)
correctBitFlip q1 q2 q3 = withQubits 2 $ \[a1, a2] -> do
  -- Syndrome measurement
  -- CNOT from q1 to a1, q2 to a1 (to check if q1 and q2 are the same)
  (q1', a1_1) <- cnot q1 a1
  (q2', a1_2) <- cnot q2 a1_1
  
  -- CNOT from q2 to a2, q3 to a2 (to check if q2 and q3 are the same)
  (q2'', a2_1) <- cnot q2' a2
  (q3', a2_2) <- cnot q3 a2_1
  
  -- Measure the ancillas to get the syndrome
  (m1, _) <- measure a1_2
  (m2, _) <- measure a2_2
  
  -- Apply corrections based on syndrome
  (q1'', q2''', q3'') <- case (m1, m2) of
    (Zero, Zero) -> pure (q1', q2'', q3')  -- No error
    (One, Zero)  -> do q1'' <- pauliX q1'; pure (q1'', q2'', q3')  -- Error on q1
    (Zero, One)  -> do q3'' <- pauliX q3'; pure (q1', q2'', q3'')  -- Error on q3
    (One, One)   -> do q2''' <- pauliX q2''; pure (q1', q2''', q3')  -- Error on q2
    _            -> pure (q1', q2'', q3')  -- Should not happen in the ideal case
  
  pure (q1'', q2''', q3'')

-- | Encode a logical qubit using the three-qubit phase-flip code
-- |ψ⟩ -> α|+++⟩ + β|---⟩
encodePhaseFlip :: Qubit %1-> Circ (Qubit, Qubit, Qubit)
encodePhaseFlip q = withQubits 2 $ \[q2, q3] -> do
  -- Apply Hadamard to input qubit
  q' <- hadamard q
  
  -- Encode using the bit-flip code
  (q'', q2', q3') <- encodeBitFlip q'
  
  -- Apply Hadamard to all three qubits
  q''' <- hadamard q''
  q2'' <- hadamard q2'
  q3'' <- hadamard q3'
  
  pure (q''', q2'', q3'')

-- | Decode a logical qubit from the three-qubit phase-flip code
decodePhaseFlip :: Qubit %1-> Qubit %1-> Qubit %1-> Circ Qubit
decodePhaseFlip q1 q2 q3 = do
  -- Apply Hadamard to all three qubits
  q1' <- hadamard q1
  q2' <- hadamard q2
  q3' <- hadamard q3
  
  -- Decode using the bit-flip code
  decodeBitFlip q1' q2' q3'

-- | Correct errors in a phase-flip encoded state
correctPhaseFlip :: Qubit %1-> Qubit %1-> Qubit %1-> Circ (Qubit, Qubit, Qubit)
correctPhaseFlip q1 q2 q3 = do
  -- Apply Hadamard to all qubits
  q1' <- hadamard q1
  q2' <- hadamard q2
  q3' <- hadamard q3
  
  -- Correct using the bit-flip code
  (q1'', q2'', q3'') <- correctBitFlip q1' q2' q3'
  
  -- Apply Hadamard again to all qubits
  q1''' <- hadamard q1''
  q2''' <- hadamard q2''
  q3''' <- hadamard q3''
  
  pure (q1''', q2''', q3''')

-- | Encode a logical qubit using the nine-qubit Shor code
-- Protects against both bit and phase flips
encodeShor :: Qubit %1-> Circ [Qubit]
encodeShor q = do
  -- First encode with the phase-flip code
  (q1, q2, q3) <- encodePhaseFlip q
  
  -- Then encode each of those qubits with the bit-flip code
  (q11, q12, q13) <- encodeBitFlip q1
  (q21, q22, q23) <- encodeBitFlip q2
  (q31, q32, q33) <- encodeBitFlip q3
  
  pure [q11, q12, q13, q21, q22, q23, q31, q32, q33]

-- | Decode a logical qubit from the nine-qubit Shor code
decodeShor :: [Qubit] %1-> Circ Qubit
decodeShor qs = do
  -- Split the nine qubits into three groups of three
  let (g1, rest1) = splitAt 3 qs
      (g2, g3) = splitAt 3 rest1
  
  -- Decode each group with the bit-flip code
  q1 <- decodeBitFlip (g1 !! 0) (g1 !! 1) (g1 !! 2)
  q2 <- decodeBitFlip (g2 !! 0) (g2 !! 1) (g2 !! 2)
  q3 <- decodeBitFlip (g3 !! 0) (g3 !! 1) (g3 !! 2)
  
  -- Decode the three qubits with the phase-flip code
  decodePhaseFlip q1 q2 q3 