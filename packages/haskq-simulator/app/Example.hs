{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii, exportVisualization)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- | Simple example demonstrating how to use HaskQ
-- Creates a Bell state and measures both qubits
bellStateCircuit :: Circ [Measurement]
bellStateCircuit = do
  (q1, q2) <- bellState
  (m1, q1') <- measure q1
  (m2, q2') <- measure q2
  pure [m1, m2]

-- | GHZ state circuit - demonstrating a 3-qubit entangled state
ghzStateCircuit :: Circ [Measurement]
ghzStateCircuit = do
  (q1, q2, q3) <- ghzState
  (m1, q1') <- measure q1
  (m2, q2') <- measure q2
  (m3, q3') <- measure q3
  pure [m1, m2, m3]

-- | Quantum teleportation circuit - demonstrating quantum information transfer
teleportCircuit :: Circ Measurement
teleportCircuit = do
  -- Create an input qubit in the |+⟩ state
  input <- qubit
  input' <- hadamard input
  
  -- Teleport the qubit
  output <- teleport input'
  
  -- Measure the output
  (result, _) <- measure output
  pure result

-- | Example of Deutsch-Jozsa algorithm to determine if a function is constant or balanced
deutschJozsaCircuit :: Circ Measurement
deutschJozsaCircuit = deutschJozsa

-- | Main program to run the examples
main :: IO ()
main = do
  args <- getArgs
  
  -- Default to the Bell state circuit if no arguments provided
  let circuitName = if null args then "bell" else head args
  
  -- Select the appropriate circuit based on the argument
  let (circuit, description) = case circuitName of
        "bell" -> (bellStateCircuit, "Bell State Circuit")
        "ghz" -> (ghzStateCircuit, "GHZ State Circuit")
        "teleport" -> (teleportCircuit, "Quantum Teleportation Circuit")
        "deutsch" -> (deutschJozsaCircuit, "Deutsch-Jozsa Algorithm Circuit")
        _ -> (bellStateCircuit, "Bell State Circuit (default)")
  
  -- Visualize the circuit
  let vis = visualizeCircuit circuit
  
  -- Display the circuit visualization
  putStrLn $ "HaskQ Example: " ++ description
  putStrLn "----------------------------------------"
  TIO.putStrLn $ circuitToAscii vis
  
  -- Simulate the circuit (simplified as our simulator isn't fully functional yet)
  let result = simulateCircuit 3 circuit
  
  -- Display the simulation results
  putStrLn "\nSimulation Results:"
  putStrLn "----------------------------------------"
  putStrLn $ "Measurements: " ++ show (measurements result)
  
  -- Export the visualization to a file (optional)
  if length args > 1 && args !! 1 /= "-" then
    do
      let outFile = args !! 1
      exportVisualization outFile vis
      putStrLn $ "\nCircuit visualization exported to: " ++ outFile
  else
    pure () 