{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HaskQ Types: Core quantum data structures
module HaskQ.Types where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import GHC.Generics (Generic)

-- | Quantum bit states
data QubitState = Zero | One | Plus | Minus | Superposition Double Double
  deriving (Eq, Show, Generic)

-- | Quantum bit with linear type semantics
newtype Qubit = Qubit { qubitId :: Int }
  deriving (Eq, Show, Generic)

-- | Quantum gates
data Gate 
  = Identity Qubit
  | Hadamard Qubit
  | PauliX Qubit
  | PauliY Qubit  
  | PauliZ Qubit
  | CNOT Qubit Qubit
  | CZ Qubit Qubit
  | Toffoli Qubit Qubit Qubit
  | RX Double Qubit
  | RY Double Qubit
  | RZ Double Qubit
  | Phase Double Qubit
  | ControlledGate Qubit Gate
  | Measurement Qubit
  deriving (Eq, Show, Generic)

-- | Quantum circuit as a sequence of gates
newtype Circuit = Circuit [Gate]
  deriving (Eq, Show, Generic)

-- | Quantum measurement result
data MeasurementResult = MResult
  { qubit :: Qubit
  , outcome :: Bool
  , probability :: Double
  } deriving (Eq, Show, Generic)

-- | Quantum state representation
data QuantumState = QState
  { amplitudes :: [Complex Double]
  , numQubits :: Int
  } deriving (Eq, Show, Generic)

-- | Complex number representation  
data Complex a = Complex a a
  deriving (Eq, Show, Generic)

instance Num a => Num (Complex a) where
  (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
  (Complex r1 i1) - (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)
  (Complex r1 i1) * (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)
  abs (Complex r i) = Complex (sqrt (r*r + i*i)) 0
  signum (Complex r i) = 
    let magnitude = sqrt (r*r + i*i)
    in if magnitude == 0 then Complex 0 0 else Complex (r/magnitude) (i/magnitude)
  fromInteger n = Complex (fromInteger n) 0

-- JSON instances for serialization
instance ToJSON QubitState where
  toJSON Zero = object ["state" .= ("zero" :: String)]
  toJSON One = object ["state" .= ("one" :: String)]
  toJSON Plus = object ["state" .= ("plus" :: String)]
  toJSON Minus = object ["state" .= ("minus" :: String)]
  toJSON (Superposition a b) = object 
    [ "state" .= ("superposition" :: String)
    , "alpha" .= a
    , "beta" .= b
    ]

instance ToJSON Qubit where
  toJSON (Qubit i) = object ["qubit" .= i]

instance ToJSON Gate where
  toJSON (Identity q) = object ["type" .= ("I" :: String), "target" .= q]
  toJSON (Hadamard q) = object ["type" .= ("H" :: String), "target" .= q]
  toJSON (PauliX q) = object ["type" .= ("X" :: String), "target" .= q]
  toJSON (PauliY q) = object ["type" .= ("Y" :: String), "target" .= q]
  toJSON (PauliZ q) = object ["type" .= ("Z" :: String), "target" .= q]
  toJSON (CNOT c t) = object ["type" .= ("CNOT" :: String), "control" .= c, "target" .= t]
  toJSON (CZ c t) = object ["type" .= ("CZ" :: String), "control" .= c, "target" .= t]
  toJSON (Toffoli c1 c2 t) = object 
    [ "type" .= ("Toffoli" :: String)
    , "control1" .= c1
    , "control2" .= c2  
    , "target" .= t
    ]
  toJSON (RX angle q) = object ["type" .= ("RX" :: String), "angle" .= angle, "target" .= q]
  toJSON (RY angle q) = object ["type" .= ("RY" :: String), "angle" .= angle, "target" .= q]
  toJSON (RZ angle q) = object ["type" .= ("RZ" :: String), "angle" .= angle, "target" .= q]
  toJSON (Phase angle q) = object ["type" .= ("Phase" :: String), "angle" .= angle, "target" .= q]
  toJSON (ControlledGate c g) = object 
    [ "type" .= ("Controlled" :: String)
    , "control" .= c
    , "gate" .= g
    ]
  toJSON (Measurement q) = object ["type" .= ("Measure" :: String), "target" .= q]

instance ToJSON (Complex Double) where
  toJSON (Complex r i) = object ["real" .= r, "imag" .= i]

instance ToJSON QuantumState
instance ToJSON MeasurementResult

-- FromJSON instances would be implemented for parsing
instance FromJSON QubitState where
  parseJSON = withObject "QubitState" $ \o -> do
    state <- o .: "state"
    case state :: String of
      "zero" -> return Zero
      "one" -> return One
      "plus" -> return Plus
      "minus" -> return Minus
      "superposition" -> Superposition <$> o .: "alpha" <*> o .: "beta"
      _ -> fail "Unknown qubit state"

instance FromJSON Qubit where
  parseJSON = withObject "Qubit" $ \o -> Qubit <$> o .: "qubit"

instance FromJSON Gate where
  parseJSON = withObject "Gate" $ \o -> do
    gateType <- o .: "type"
    case gateType :: String of
      "I" -> Identity <$> o .: "target"
      "H" -> Hadamard <$> o .: "target"
      "X" -> PauliX <$> o .: "target"
      "Y" -> PauliY <$> o .: "target"
      "Z" -> PauliZ <$> o .: "target"
      "CNOT" -> CNOT <$> o .: "control" <*> o .: "target"
      "CZ" -> CZ <$> o .: "control" <*> o .: "target"
      "Toffoli" -> Toffoli <$> o .: "control1" <*> o .: "control2" <*> o .: "target"
      "RX" -> RX <$> o .: "angle" <*> o .: "target"
      "RY" -> RY <$> o .: "angle" <*> o .: "target"
      "RZ" -> RZ <$> o .: "angle" <*> o .: "target"
      "Phase" -> Phase <$> o .: "angle" <*> o .: "target"
      "Controlled" -> ControlledGate <$> o .: "control" <*> o .: "gate"
      "Measure" -> Measurement <$> o .: "target"
      _ -> fail "Unknown gate type"

instance FromJSON (Complex Double)
instance FromJSON QuantumState  
instance FromJSON MeasurementResult 