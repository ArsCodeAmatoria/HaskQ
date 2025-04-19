{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HaskQ.Simulator.StateVector
  ( StateVector
  , createStateVector
  , numQubits
  , amplitude
  , probabilities
  , basis
  , applyMatrix
  , tensorProduct
  , normalize
  , fromComplex
  , toComplex
  , Complex(..)
  ) where

import qualified Data.Vector as V
import qualified Prelude as P
import Prelude.Linear
import Data.Complex

-- | State vector representation for quantum state
data StateVector = StateVector
  { numQubits :: Int
  , amplitudes :: V.Vector (Complex Double)
  } deriving (Show, Eq)

-- | Create a new state vector for n qubits
createStateVector :: Int -> StateVector
createStateVector n = StateVector
  { numQubits = n
  , amplitudes = V.generate (2^n) (\i -> if i == 0 then 1 :+ 0 else 0 :+ 0)
  }

-- | Get amplitude at a specific basis state
amplitude :: StateVector -> Int -> Complex Double
amplitude sv i
  | i < 0 || i >= V.length (amplitudes sv) = 0 :+ 0
  | otherwise = amplitudes sv V.! i

-- | Calculate probability distribution from state vector
probabilities :: StateVector -> V.Vector Double
probabilities sv = V.map (\c -> magnitude c ^ 2) (amplitudes sv)

-- | Get the computational basis states
basis :: Int -> [Int]
basis n = [0..(2^n - 1)]

-- | Apply a unitary matrix to the state vector
applyMatrix :: V.Vector (V.Vector (Complex Double)) -> StateVector -> StateVector
applyMatrix matrix sv =
  let dim = V.length matrix
      amps = amplitudes sv
      newAmps = V.generate dim $ \i ->
        V.sum $ V.zipWith (*) (matrix V.! i) amps
  in StateVector (numQubits sv) newAmps

-- | Tensor product of two state vectors
tensorProduct :: StateVector -> StateVector -> StateVector
tensorProduct sv1 sv2 =
  let n1 = numQubits sv1
      n2 = numQubits sv2
      amps1 = amplitudes sv1
      amps2 = amplitudes sv2
      newAmps = V.generate (V.length amps1 * V.length amps2) $ \i ->
        let (i1, i2) = i `P.divMod` V.length amps2
        in amps1 V.! i1 * amps2 V.! i2
  in StateVector (n1 + n2) newAmps

-- | Normalize a state vector
normalize :: StateVector -> StateVector
normalize sv =
  let amps = amplitudes sv
      norm = sqrt $ V.sum $ V.map (\c -> magnitude c ^ 2) amps
      normalized = V.map (/ (norm :+ 0)) amps
  in StateVector (numQubits sv) normalized

-- | Convert from a complex number representation
fromComplex :: Complex Double -> (Double, Double)
fromComplex (r :+ i) = (r, i)

-- | Convert to a complex number representation
toComplex :: (Double, Double) -> Complex Double
toComplex (r, i) = r :+ i 