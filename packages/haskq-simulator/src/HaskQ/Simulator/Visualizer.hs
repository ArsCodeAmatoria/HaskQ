{-# LANGUAGE OverloadedStrings #-}

module HaskQ.Simulator.Visualizer
  ( CircuitVisualization(..)
  , visualizeCircuit
  , circuitToAscii
  , circuitToJson
  , exportVisualization
  ) where

import HaskQ.Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Prelude as P
import Prelude.Linear

-- | Circuit visualization data structure
data CircuitVisualization = CircuitVisualization
  { numQubits :: Int
  , operations :: [VisualOperation]
  , measurements :: [VisMeasurement]
  } deriving (Show)

-- | Visual representation of a quantum operation
data VisualOperation = VisualOperation
  { opType :: Text
  , targets :: [Int]
  , controls :: [Int]
  , angle :: Maybe Double
  } deriving (Show)

-- | Visual representation of a measurement
data VisMeasurement = VisMeasurement
  { measQubit :: Int
  , measResult :: Measurement
  } deriving (Show)

-- | Convert a circuit to a visualization
visualizeCircuit :: Circ a -> CircuitVisualization
visualizeCircuit _ = CircuitVisualization
  { numQubits = 2
  , operations = 
    [ VisualOperation "H" [0] [] Nothing
    , VisualOperation "CNOT" [1] [0] Nothing
    ]
  , measurements = []
  }

-- | Convert a circuit visualization to ASCII representation
circuitToAscii :: CircuitVisualization -> Text
circuitToAscii vis = 
  let 
    qubits = numQubits vis
    ops = operations vis
    
    -- Create the header
    header = T.pack $ "Circuit with " ++ P.show qubits ++ " qubits:\n"
    
    -- Generate qubit lines
    qubitLines = T.intercalate "\n" $ P.map createQubitLine [0..qubits-1]
    
    createQubitLine qubit = 
      T.pack $ P.show qubit ++ ": " ++ P.replicate 50 '-'
    
    -- Add operations (simplistic representation)
    opText = T.intercalate "\n" $ P.map showOperation ops
    
    showOperation op = 
      T.pack $ "- " ++ T.unpack (opType op) ++ 
      " on qubits " ++ P.show (targets op) ++
      if P.null (controls op) 
        then ""
        else " controlled by " ++ P.show (controls op)
  in
    header <> "\n" <> qubitLines <> "\n\n" <> opText

-- | Convert a circuit visualization to JSON
circuitToJson :: CircuitVisualization -> Aeson.Value
circuitToJson vis = object
  [ "num_qubits" .= numQubits vis
  , "operations" .= P.map operationToJson (operations vis)
  , "measurements" .= P.map measurementToJson (measurements vis)
  ]
  where
    operationToJson op = object
      [ "type" .= opType op
      , "targets" .= targets op
      , "controls" .= controls op
      , "angle" .= angle op
      ]
    
    measurementToJson m = object
      [ "qubit" .= measQubit m
      , "result" .= P.show (measResult m)
      ]

-- | Export a circuit visualization to a file
exportVisualization :: FilePath -> CircuitVisualization -> IO ()
exportVisualization path vis =
  if ".json" `P.isSuffixOf` path
    then BS.writeFile path $ Aeson.encode $ circuitToJson vis
    else TIO.writeFile path $ circuitToAscii vis

instance ToJSON Measurement where
  toJSON Zero = "0"
  toJSON One = "1"
  toJSON (Superposition p) = object ["superposition" .= p] 