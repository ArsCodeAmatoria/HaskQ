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
import qualified System.IO as P

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
  , column :: Int  -- Position in the circuit timeline
  } deriving (Show)

-- | Visual representation of a measurement
data VisMeasurement = VisMeasurement
  { measQubit :: Int
  , measResult :: Measurement
  , measColumn :: Int  -- Position in the circuit timeline
  } deriving (Show)

-- | Convert a circuit to a visualization
-- This is a simplified implementation that generates a Bell circuit
visualizeCircuit :: Circ a -> CircuitVisualization
visualizeCircuit _ = CircuitVisualization
  { numQubits = 2
  , operations = 
    [ VisualOperation "H" [0] [] Nothing 1
    , VisualOperation "CNOT" [1] [0] Nothing 3
    ]
  , measurements = 
    [ VisMeasurement 0 (Superposition 0.5) 5
    , VisMeasurement 1 (Superposition 0.5) 5
    ]
  }

-- | Convert a circuit visualization to ASCII representation
circuitToAscii :: CircuitVisualization -> Text
circuitToAscii vis = 
  let 
    qubits = numQubits vis
    ops = operations vis
    meas = measurements vis
    
    -- Find maximum column (circuit width)
    maxCol = P.maximum $ [0] ++ P.map column ops ++ P.map measColumn meas
    
    -- Create the header
    header = T.pack $ "Circuit with " ++ P.show qubits ++ " qubits:\n\n"
    
    -- Generate qubit lines
    qubitLines = T.intercalate "\n" $ P.map (createQubitLine maxCol ops meas) [0..qubits-1]
    
    -- Add operations description
    opDesc = T.pack "\n\nOperation descriptions:\n" <> 
             T.intercalate "\n" (P.map describeOperation ops) <>
             "\n\nMeasurement results:\n" <>
             T.intercalate "\n" (P.map describeMeasurement meas)
  in
    header <> qubitLines <> opDesc

-- | Create a visual line for a qubit
createQubitLine :: Int -> [VisualOperation] -> [VisMeasurement] -> Int -> Text
createQubitLine maxCol ops meas qubitIdx = 
  let
    -- Label for the qubit
    label = T.pack $ P.show qubitIdx ++ ": "
    
    -- Filter operations that involve this qubit
    qubitOps = P.filter (\op -> qubitIdx `P.elem` targets op || qubitIdx `P.elem` controls op) ops
    
    -- Filter measurements for this qubit
    qubitMeas = P.filter (\m -> measQubit m == qubitIdx) meas
    
    -- Create the line
    line = T.concat [createSegment col qubitIdx qubitOps qubitMeas | col <- [1..maxCol]]
    
    -- Line with qubit label
    fullLine = label <> line
  in
    fullLine

-- | Create a segment of the qubit line for a specific column
createSegment :: Int -> Int -> [VisualOperation] -> [VisMeasurement] -> Text
createSegment col qubitIdx ops meas =
  let
    -- Check for operations at this position
    opAtPos = P.filter (\op -> column op == col) ops
    
    -- Check for measurements at this position
    measAtPos = P.filter (\m -> measColumn m == col) meas
    
    -- Render the segment
    segment = 
      if not (P.null opAtPos) then
        let op = P.head opAtPos in
        if qubitIdx `P.elem` targets op then
          case opType op of
            "H" -> "--H--"
            "X" -> "--X--"
            "Y" -> "--Y--"
            "Z" -> "--Z--"
            "CNOT" | qubitIdx == P.head (targets op) -> "--X--"
            _ -> "--*--"
        else if qubitIdx `P.elem` controls op then
          "--●--"
        else
          if any (\otherOp -> 
                  column otherOp == col && 
                  any (\t -> t > qubitIdx) (targets otherOp) && 
                  any (\c -> c < qubitIdx) (controls otherOp)
                ) ops
          then
            "--|--"  -- Vertical line for connecting controls and targets
          else
            "-----"  -- Regular wire
      else if not (P.null measAtPos) then
        "--M--"
      else
        "-----"  -- Regular wire
  in
    segment

-- | Describe an operation
describeOperation :: VisualOperation -> Text
describeOperation op =
  let
    opStr = opType op
    targetsStr = T.pack $ P.show (targets op)
    controlsStr = if P.null (controls op) 
                  then "" 
                  else ", controlled by " <> T.pack (P.show (controls op))
    angleStr = case angle op of
                Just a -> ", angle = " <> T.pack (P.show a)
                Nothing -> ""
  in
    "- " <> opStr <> " on qubit(s) " <> targetsStr <> controlsStr <> angleStr

-- | Describe a measurement
describeMeasurement :: VisMeasurement -> Text
describeMeasurement m =
  let
    qubitStr = T.pack $ P.show (measQubit m)
    resultStr = case measResult m of
                  Zero -> "0 (|0⟩)"
                  One -> "1 (|1⟩)"
                  Superposition p -> T.pack $ "superposition with " ++ P.show (p * 100) ++ "% chance of |1⟩"
  in
    "- Qubit " <> qubitStr <> " measured: " <> resultStr

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
      , "column" .= column op
      ]
    
    measurementToJson m = object
      [ "qubit" .= measQubit m
      , "result" .= P.show (measResult m)
      , "column" .= measColumn m
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