{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

-- | Foreign Function Interface to HaskQ Rust Core
-- 
-- This module provides low-level bindings to the high-performance
-- Rust quantum simulation engine.
module HaskQ.Framework.Internal.FFI
  ( -- * Simulator Management
    SimulatorId
  , createSimulator
  , destroySimulator
  , resetSimulator
  
  -- * Gate Operations
  , applyI, applyX, applyY, applyZ
  , applyH, applyS, applyT
  , applyRX, applyRY, applyRZ, applyPhase
  , applyCNOT, applyCZ, applySwap
  , applyToffoli
  
  -- * State Access
  , getNumQubits
  , getStateSize
  , getAmplitudes
  , getProbabilities
  , measureAll
  
  -- * Circuit Processing
  , applyCircuitJson
  
  -- * Utility
  , getVersion
  , freeString
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception (bracket)
import qualified Data.Vector.Storable as V

-- | Opaque simulator identifier
type SimulatorId = Word32

-- | Complex number representation for FFI
data ComplexF64 = ComplexF64
  { complexReal :: !CDouble
  , complexImag :: !CDouble
  } deriving (Show, Eq)

instance Storable ComplexF64 where
  sizeOf _ = 16  -- 2 * sizeof(double)
  alignment _ = 8
  peek ptr = do
    r <- peekByteOff ptr 0
    i <- peekByteOff ptr 8
    return $ ComplexF64 r i
  poke ptr (ComplexF64 r i) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 8 i

-- | FFI imports from Rust core
foreign import capi "haskq_core.h haskq_create_simulator"
  c_create_simulator :: CInt -> IO SimulatorId

foreign import capi "haskq_core.h haskq_destroy_simulator"
  c_destroy_simulator :: SimulatorId -> IO CBool

foreign import capi "haskq_core.h haskq_reset_simulator"
  c_reset_simulator :: SimulatorId -> IO CBool

-- Single-qubit gates
foreign import capi "haskq_core.h haskq_apply_i"
  c_apply_i :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_x"
  c_apply_x :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_y"
  c_apply_y :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_z"
  c_apply_z :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_h"
  c_apply_h :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_s"
  c_apply_s :: SimulatorId -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_t"
  c_apply_t :: SimulatorId -> CInt -> IO CBool

-- Rotation gates
foreign import capi "haskq_core.h haskq_apply_rx"
  c_apply_rx :: SimulatorId -> CDouble -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_ry"
  c_apply_ry :: SimulatorId -> CDouble -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_rz"
  c_apply_rz :: SimulatorId -> CDouble -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_phase"
  c_apply_phase :: SimulatorId -> CDouble -> CInt -> IO CBool

-- Two-qubit gates
foreign import capi "haskq_core.h haskq_apply_cnot"
  c_apply_cnot :: SimulatorId -> CInt -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_cz"
  c_apply_cz :: SimulatorId -> CInt -> CInt -> IO CBool

foreign import capi "haskq_core.h haskq_apply_swap"
  c_apply_swap :: SimulatorId -> CInt -> CInt -> IO CBool

-- Three-qubit gates
foreign import capi "haskq_core.h haskq_apply_toffoli"
  c_apply_toffoli :: SimulatorId -> CInt -> CInt -> CInt -> IO CBool

-- State access
foreign import capi "haskq_core.h haskq_get_num_qubits"
  c_get_num_qubits :: SimulatorId -> IO CInt

foreign import capi "haskq_core.h haskq_get_state_size"
  c_get_state_size :: SimulatorId -> IO CSize

foreign import capi "haskq_core.h haskq_get_amplitudes"
  c_get_amplitudes :: SimulatorId -> Ptr ComplexF64 -> CSize -> IO CBool

foreign import capi "haskq_core.h haskq_get_probabilities"
  c_get_probabilities :: SimulatorId -> Ptr CDouble -> CSize -> IO CBool

foreign import capi "haskq_core.h haskq_measure_all"
  c_measure_all :: SimulatorId -> Ptr CBool -> CSize -> IO CBool

-- Circuit processing
foreign import capi "haskq_core.h haskq_apply_circuit_json"
  c_apply_circuit_json :: SimulatorId -> CString -> IO CBool

-- Utility functions
foreign import capi "haskq_core.h haskq_get_version"
  c_get_version :: IO CString

foreign import capi "haskq_core.h haskq_free_string"
  c_free_string :: CString -> IO ()

-- | High-level Haskell wrappers

-- | Create a new quantum simulator
createSimulator :: Int -> IO (Maybe SimulatorId)
createSimulator numQubits
  | numQubits <= 0 || numQubits > 30 = return Nothing
  | otherwise = do
      simId <- c_create_simulator (fromIntegral numQubits)
      return $ if simId == 0 then Nothing else Just simId

-- | Destroy a quantum simulator
destroySimulator :: SimulatorId -> IO Bool
destroySimulator simId = toBool <$> c_destroy_simulator simId

-- | Reset simulator to initial state
resetSimulator :: SimulatorId -> IO Bool
resetSimulator simId = toBool <$> c_reset_simulator simId

-- | Apply gates (single-qubit)
applyI, applyX, applyY, applyZ, applyH, applyS, applyT :: SimulatorId -> Int -> IO Bool
applyI simId target = toBool <$> c_apply_i simId (fromIntegral target)
applyX simId target = toBool <$> c_apply_x simId (fromIntegral target)
applyY simId target = toBool <$> c_apply_y simId (fromIntegral target)
applyZ simId target = toBool <$> c_apply_z simId (fromIntegral target)
applyH simId target = toBool <$> c_apply_h simId (fromIntegral target)
applyS simId target = toBool <$> c_apply_s simId (fromIntegral target)
applyT simId target = toBool <$> c_apply_t simId (fromIntegral target)

-- | Apply rotation gates
applyRX, applyRY, applyRZ, applyPhase :: SimulatorId -> Double -> Int -> IO Bool
applyRX simId angle target = toBool <$> c_apply_rx simId (realToFrac angle) (fromIntegral target)
applyRY simId angle target = toBool <$> c_apply_ry simId (realToFrac angle) (fromIntegral target)
applyRZ simId angle target = toBool <$> c_apply_rz simId (realToFrac angle) (fromIntegral target)
applyPhase simId angle target = toBool <$> c_apply_phase simId (realToFrac angle) (fromIntegral target)

-- | Apply two-qubit gates
applyCNOT, applyCZ, applySwap :: SimulatorId -> Int -> Int -> IO Bool
applyCNOT simId control target = toBool <$> c_apply_cnot simId (fromIntegral control) (fromIntegral target)
applyCZ simId control target = toBool <$> c_apply_cz simId (fromIntegral control) (fromIntegral target)
applySwap simId q1 q2 = toBool <$> c_apply_swap simId (fromIntegral q1) (fromIntegral q2)

-- | Apply Toffoli gate
applyToffoli :: SimulatorId -> Int -> Int -> Int -> IO Bool
applyToffoli simId c1 c2 target = toBool <$> c_apply_toffoli simId (fromIntegral c1) (fromIntegral c2) (fromIntegral target)

-- | Get number of qubits
getNumQubits :: SimulatorId -> IO (Maybe Int)
getNumQubits simId = do
  result <- c_get_num_qubits simId
  return $ if result < 0 then Nothing else Just (fromIntegral result)

-- | Get state vector size
getStateSize :: SimulatorId -> IO Int
getStateSize simId = fromIntegral <$> c_get_state_size simId

-- | Get quantum state amplitudes
getAmplitudes :: SimulatorId -> IO (Maybe (V.Vector (Complex Double)))
getAmplitudes simId = do
  size <- getStateSize simId
  if size == 0
    then return Nothing
    else do
      allocaArray size $ \ptr -> do
        success <- c_get_amplitudes simId ptr (fromIntegral size)
        if toBool success
          then do
            amps <- peekArray size ptr
            let converted = map (\(ComplexF64 r i) -> realToFrac r :+ realToFrac i) amps
            return $ Just $ V.fromList converted
          else return Nothing

-- | Get measurement probabilities
getProbabilities :: SimulatorId -> IO (Maybe (V.Vector Double))
getProbabilities simId = do
  size <- getStateSize simId
  if size == 0
    then return Nothing
    else do
      allocaArray size $ \ptr -> do
        success <- c_get_probabilities simId ptr (fromIntegral size)
        if toBool success
          then do
            probs <- peekArray size ptr
            let converted = map realToFrac probs
            return $ Just $ V.fromList converted
          else return Nothing

-- | Measure all qubits
measureAll :: SimulatorId -> IO (Maybe [Bool])
measureAll simId = do
  numQubits <- getNumQubits simId
  case numQubits of
    Nothing -> return Nothing
    Just n -> do
      allocaArray n $ \ptr -> do
        success <- c_measure_all simId ptr (fromIntegral n)
        if toBool success
          then do
            results <- peekArray n ptr
            return $ Just $ map toBool results
          else return Nothing

-- | Apply circuit from JSON description
applyCircuitJson :: SimulatorId -> String -> IO Bool
applyCircuitJson simId jsonStr = do
  withCString jsonStr $ \cstr -> do
    toBool <$> c_apply_circuit_json simId cstr

-- | Get library version
getVersion :: IO String
getVersion = do
  cstr <- c_get_version
  if cstr == nullPtr
    then return "unknown"
    else do
      version <- peekCString cstr
      c_free_string cstr
      return version

-- | Free a C string
freeString :: CString -> IO ()
freeString = c_free_string

-- | Helper functions
toBool :: CBool -> Bool
toBool = (/= 0)

-- | Complex number type alias
type Complex a = Data.Complex.Complex a

-- Import the Complex type
import Data.Complex (Complex(..)) 