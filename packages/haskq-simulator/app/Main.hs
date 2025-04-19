{-# LANGUAGE LinearTypes #-}

module Main (main) where

import HaskQ.Core.Types
import HaskQ.Core.Gates
import HaskQ.Core.Circuit
import HaskQ.Core.Examples
import HaskQ.Simulator.Circuit
import HaskQ.Simulator.Visualizer
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Options.Applicative
import qualified Prelude as P
import Prelude.Linear

-- | Command line options
data Options = Options
  { circuit :: String    -- Circuit to simulate
  , qubits  :: Int       -- Number of qubits
  , output  :: String    -- Output format (ascii/json)
  , file    :: Maybe String  -- Optional output file
  }

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "circuit"
     <> short 'c'
     <> metavar "CIRCUIT"
     <> help "Circuit to simulate (bell, ghz, teleport, deutsch)" )
  <*> option auto
      ( long "qubits"
     <> short 'q'
     <> metavar "NUM"
     <> value 2
     <> help "Number of qubits (default: 2)" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FORMAT"
     <> value "ascii"
     <> help "Output format: ascii or json (default: ascii)" )
  <*> optional (strOption
      ( long "file"
     <> short 'f'
     <> metavar "FILE"
     <> help "Output file (if not specified, output to stdout)" ))

-- | Main function
main :: P.IO ()
main = execParser opts P.>>= runWithOptions
  where
    opts = info (optionsParser P.<**> helper)
      ( fullDesc
     <> progDesc "Simulate quantum circuits using HaskQ"
     <> header "haskq-simulator - a quantum circuit simulator" )

-- | Run with parsed options
runWithOptions :: Options -> P.IO ()
runWithOptions Options{circuit=circuitName, qubits=numQubits, output=format, file=outputFile} = do
  -- Select circuit based on name
  let circ = case circuitName of
        "bell"     -> bellState
        "ghz"      -> ghzState
        "teleport" -> teleport undefined -- This is just a placeholder
        "deutsch"  -> deutschJozsa
        _          -> bellState  -- Default to Bell state
  
  -- Visualize the circuit
  let vis = visualizeCircuit circ
  
  -- Format output
  let formattedOutput = if format == "json"
                        then P.show (circuitToJson vis)
                        else T.unpack (circuitToAscii vis)
  
  -- Output to file or stdout
  case outputFile of
    Just file -> P.writeFile file formattedOutput
    Nothing   -> P.putStrLn formattedOutput 