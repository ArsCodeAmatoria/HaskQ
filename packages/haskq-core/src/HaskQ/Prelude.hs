{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

-- | HaskQ.Prelude
-- 
-- This module provides the core functionality of HaskQ, re-exporting
-- all the necessary components for quantum programming.
module HaskQ.Prelude
  ( 
    -- * Types
    Qubit
  , Circ
  , Measurement(..)
  , CircuitOutput(..)
  
    -- * Circuit construction
  , qubit
  , circuit
  , withQubit
  , withQubits
  , runCirc
  
    -- * Gates
  , hadamard
  , pauliX
  , pauliY
  , pauliZ
  , phase
  , cnot
  , swap
  
    -- * Measurement
  , measure
  , measureAll
  , probabilityOfOne
  
    -- * Examples
  , bellState
  , ghzState
  , teleport
  , deutschJozsa
  
    -- * Error Correction
  , encodeBitFlip
  , decodeBitFlip
  , correctBitFlip
  , encodePhaseFlip
  , decodePhaseFlip
  , correctPhaseFlip
  , encodeShor
  , decodeShor
  ) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Measurement
import HaskQ.Core.Examples
import HaskQ.Core.ErrorCorrection

-- | Re-export everything needed for quantum programming with HaskQ 