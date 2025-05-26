{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HaskQ Core: Type-safe quantum circuit DSL
module HaskQ.Core
  ( -- * Quantum Types
    Qubit(..)
  , Circuit(..)
  , Gate(..)
  
  -- * Circuit Construction
  , createQubit
  , withQubits
  , (><)
  
  -- * Basic Gates
  , hadamard
  , pauliX
  , pauliY
  , pauliZ
  , cnot
  , cz
  , toffoli
  
  -- * Rotation Gates
  , rx
  , ry
  , rz
  , phase
  
  -- * Circuit Composition
  , sequential
  , parallel
  , controlled
  
  -- * Examples
  , bellState
  , ghzState
  , deutschAlgorithm
  , groverIteration
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object)
import qualified Data.Text as T
import HaskQ.Types
import HaskQ.Gates

-- | Create a qubit in the |0⟩ state
createQubit :: QubitState -> Qubit
createQubit = Qubit

-- | Construct a circuit with n qubits
withQubits :: Int -> ([Qubit] -> Circuit) -> Circuit
withQubits n f = f $ map (Qubit . Zero) [0..n-1]

-- | Circuit composition operator
(><) :: Circuit -> Circuit -> Circuit
(Circuit g1) >< (Circuit g2) = Circuit (g1 ++ g2)
infixr 6 ><

-- | Sequential gate application (monadic composition)
sequential :: [Gate] -> Circuit
sequential = Circuit

-- | Parallel gate application
parallel :: [Gate] -> Circuit
parallel gates = Circuit gates

-- | Create controlled version of a gate
controlled :: Qubit -> Gate -> Gate
controlled control gate = ControlledGate control gate

-- | Example: Bell state circuit
-- |00⟩ → (|00⟩ + |11⟩)/√2
bellState :: Circuit
bellState = withQubits 2 $ \[q1, q2] -> sequential
  [ hadamard q1
  , cnot q1 q2
  ]

-- | Example: GHZ state circuit  
-- |000⟩ → (|000⟩ + |111⟩)/√2
ghzState :: Int -> Circuit
ghzState n = withQubits n $ \qubits -> case qubits of
  [] -> Circuit []
  (q:qs) -> sequential $ 
    hadamard q : [cnot q qi | qi <- qs]

-- | Deutsch's algorithm implementation
deutschAlgorithm :: (Bool -> Bool) -> Circuit
deutschAlgorithm oracle = withQubits 2 $ \[x, y] -> sequential
  [ pauliX y
  , hadamard x
  , hadamard y
  , oracleGate oracle x y
  , hadamard x
  ]
  where
    oracleGate :: (Bool -> Bool) -> Qubit -> Qubit -> Gate
    oracleGate f qx qy 
      | f False == f True = Identity qx  -- Constant function
      | otherwise = cnot qx qy           -- Balanced function

-- | Single iteration of Grover's algorithm
groverIteration :: [Int] -> Int -> Circuit
groverIteration marked n = withQubits n $ \qubits -> sequential $
  [ hadamard q | q <- qubits ] ++
  oracleGates marked qubits ++
  diffuserGates qubits
  where
    oracleGates :: [Int] -> [Qubit] -> [Gate]
    oracleGates marks qs = [phase (-1) (qs !! i) | i <- marks]
    
    diffuserGates :: [Qubit] -> [Gate]
    diffuserGates qs = 
      [ hadamard q | q <- qs ] ++
      [ pauliX q | q <- qs ] ++
      [multiControlledZ qs] ++
      [ pauliX q | q <- qs ] ++
      [ hadamard q | q <- qs ]
      
    multiControlledZ :: [Qubit] -> Gate
    multiControlledZ [] = error "Empty qubit list"
    multiControlledZ [q] = pauliZ q
    multiControlledZ (q:qs) = foldr (\ctrl acc -> controlled ctrl acc) (pauliZ q) qs

-- JSON serialization instances
instance ToJSON Circuit where
  toJSON (Circuit gates) = object ["gates" .= gates]

instance FromJSON Circuit where
  parseJSON = error "Circuit parsing not yet implemented" 