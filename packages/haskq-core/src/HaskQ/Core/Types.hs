{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module HaskQ.Core.Types 
  ( Qubit
  , Gate(..)
  , Circ(..)
  , Measurement(..)
  , runCirc
  , qubit
  , Circuit
  , CircuitState
  , CircuitOutput(..)
  , compose
  ) where

import qualified Prelude
import Prelude.Linear
import Data.Kind (Type)
import Control.Monad.Linear
import qualified Control.Monad as P

-- | Qubit represents a quantum bit in our system
-- Linear types ensure qubits cannot be copied (quantum no-cloning theorem)
data Qubit

-- | Internal state representation for a quantum circuit
data CircuitState

-- | Circuit output after execution
data CircuitOutput a = CircuitOutput
  { measurements :: [Measurement]
  , result :: a
  } deriving (Show, P.Functor)

-- | Result of a quantum measurement
data Measurement = 
    Zero 
  | One 
  | Superposition Double -- ^ Probability of measuring |1⟩
  deriving (Show, Eq)

-- | Quantum gates operate on qubits
data Gate where
  -- Single-qubit gates
  Hadamard :: Gate
  PauliX   :: Gate
  PauliY   :: Gate
  PauliZ   :: Gate
  Phase    :: Double -> Gate
  -- Two-qubit gates
  CNOT     :: Gate
  Swap     :: Gate
  -- Measurement gate
  Measure  :: Gate

-- | Circuit monad for composing quantum operations
newtype Circ a = Circ { unCirc :: CircuitState %1-> (CircuitState, a) }

instance Functor Circ where
  fmap f (Circ c) = Circ $ \s %1-> 
    let (s', a) = c s
    in (s', f a)

instance Applicative Circ where
  pure x = Circ $ \s %1-> (s, x)
  Circ cf <*> Circ ca = Circ $ \s %1-> 
    let (s', f) = cf s
        (s'', a) = ca s'
    in (s'', f a)

instance Monad Circ where
  Circ ca >>= f = Circ $ \s %1-> 
    let (s', a) = ca s
        Circ cb = f a
    in cb s'

-- | Type alias for a quantum circuit
type Circuit a = Circ a

-- | Initialize a qubit in the |0⟩ state
qubit :: Circ Qubit
qubit = Circ $ \s %1-> (s, undefined) -- Implementation provided by simulator

-- | Execute a quantum circuit and get the result
runCirc :: Circ a %1-> CircuitOutput a
runCirc _ = undefined -- Implementation provided by simulator

-- | Compose two circuits sequentially
compose :: Circ a %1-> (a %1-> Circ b) %1-> Circ b
compose = (>>=) 