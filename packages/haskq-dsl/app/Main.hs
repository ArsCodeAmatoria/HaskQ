{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import HaskQ.Core
import HaskQ.Types
import HaskQ.Export

-- Command line options
data Options = Options
  { optCommand :: Command
  } deriving (Show)

data Command
  = Generate GenerateOpts
  | Analyze AnalyzeOpts
  | Export ExportOpts
  deriving (Show)

data GenerateOpts = GenerateOpts
  { genAlgorithm :: String
  , genQubits :: Int
  , genOutput :: Maybe FilePath
  } deriving (Show)

data AnalyzeOpts = AnalyzeOpts
  { analyzeFile :: FilePath
  } deriving (Show)

data ExportOpts = ExportOpts
  { exportFile :: FilePath
  , exportFormat :: String
  , exportOutput :: Maybe FilePath
  } deriving (Show)

-- Parser for command line options
options :: Parser Options
options = Options <$> subparser
  ( command "generate" (info generateOpts (progDesc "Generate quantum circuits"))
  <> command "analyze" (info analyzeOpts (progDesc "Analyze quantum circuits"))
  <> command "export" (info exportOpts (progDesc "Export circuits to different formats"))
  )

generateOpts :: Parser Command
generateOpts = Generate <$> (GenerateOpts
  <$> strOption
      ( long "algorithm"
      <> short 'a'
      <> metavar "ALGORITHM"
      <> help "Algorithm to generate (bell, ghz, grover, deutsch)" )
  <*> option auto
      ( long "qubits"
      <> short 'q'
      <> metavar "N"
      <> value 2
      <> help "Number of qubits" )
  <*> optional (strOption
      ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file (default: stdout)" )))

analyzeOpts :: Parser Command
analyzeOpts = Analyze <$> (AnalyzeOpts
  <$> strArgument
      ( metavar "FILE"
      <> help "Haskell file to analyze" ))

exportOpts :: Parser Command
exportOpts = Export <$> (ExportOpts
  <$> strArgument
      ( metavar "FILE"
      <> help "Input circuit file" )
  <*> strOption
      ( long "format"
      <> short 'f'
      <> metavar "FORMAT"
      <> value "json"
      <> help "Export format (json, qasm, latex)" )
  <*> optional (strOption
      ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file (default: stdout)" )))

-- Main function
main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper)
    ( fullDesc
    <> progDesc "HaskQ - Quantum Circuit DSL"
    <> header "haskq-cli - command line interface for HaskQ" )
  
  case optCommand opts of
    Generate genOpts -> runGenerate genOpts
    Analyze anaOpts -> runAnalyze anaOpts
    Export expOpts -> runExport expOpts

-- Command implementations
runGenerate :: GenerateOpts -> IO ()
runGenerate opts = do
  circuit <- case genAlgorithm opts of
    "bell" -> return bellState
    "ghz" -> return $ ghzState (genQubits opts)
    "grover" -> return $ groverIteration [0] (genQubits opts)
    "deutsch" -> return $ deutschAlgorithm id  -- constant function
    alg -> do
      hPutStrLn stderr $ "Unknown algorithm: " ++ alg
      exitFailure
  
  let json = encodePretty circuit
  case genOutput opts of
    Nothing -> L8.putStrLn json
    Just file -> L8.writeFile file json
  
  putStrLn $ "Generated " ++ genAlgorithm opts ++ " circuit with " ++ show (genQubits opts) ++ " qubits"

runAnalyze :: AnalyzeOpts -> IO ()
runAnalyze opts = do
  content <- readFile (analyzeFile opts)
  putStrLn $ "Analyzing file: " ++ analyzeFile opts
  putStrLn $ "File contains " ++ show (length (lines content)) ++ " lines"
  putStrLn $ "Lines with 'Circuit': " ++ show (length $ filter (elem "Circuit") $ lines content)
  putStrLn $ "Lines with 'Gate': " ++ show (length $ filter (elem "Gate") $ lines content)

runExport :: ExportOpts -> IO ()
runExport opts = do
  putStrLn $ "Exporting " ++ exportFile opts ++ " to " ++ exportFormat opts
  case exportFormat opts of
    "json" -> putStrLn "JSON export not yet implemented"
    "qasm" -> putStrLn "QASM export not yet implemented"
    "latex" -> putStrLn "LaTeX export not yet implemented"
    fmt -> do
      hPutStrLn stderr $ "Unknown format: " ++ fmt
      exitFailure 