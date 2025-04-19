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

-- | Example circuit demonstrating the bit-flip code
bitFlipExample :: Circ [Measurement]
bitFlipExample = do
  -- Create a qubit in a superposition state |+⟩ = (|0⟩ + |1⟩)/√2
  q <- qubit
  q' <- hadamard q
  
  -- Encode the qubit using the bit-flip code
  (q1, q2, q3) <- encodeBitFlip q'
  
  -- Introduce an artificial bit-flip error on the first qubit
  q1' <- pauliX q1
  
  -- Perform error correction
  (q1'', q2', q3') <- correctBitFlip q1' q2 q3
  
  -- Decode the state
  q_decoded <- decodeBitFlip q1'' q2' q3'
  
  -- Apply Hadamard to return from |+⟩ to |0⟩ state
  q_final <- hadamard q_decoded
  
  -- Measure and observe that the state should be |0⟩
  (m, _) <- measure q_final
  
  pure [m]

-- | Example circuit demonstrating the phase-flip code
phaseFlipExample :: Circ [Measurement]
phaseFlipExample = do
  -- Create a qubit in the |0⟩ state
  q <- qubit
  
  -- Encode the qubit using the phase-flip code
  (q1, q2, q3) <- encodePhaseFlip q
  
  -- Introduce an artificial phase-flip error on the second qubit
  q2' <- pauliZ q2
  
  -- Perform error correction
  (q1', q2'', q3') <- correctPhaseFlip q1 q2' q3
  
  -- Decode the state
  q_decoded <- decodePhaseFlip q1' q2'' q3'
  
  -- Measure and observe that the state should be |0⟩
  (m, _) <- measure q_decoded
  
  pure [m]

-- | Example circuit demonstrating the Shor code (combined bit and phase flips)
shorCodeExample :: Circ [Measurement]
shorCodeExample = do
  -- Create a qubit in the |+⟩ state
  q <- qubit
  q' <- hadamard q
  
  -- Encode the qubit using the Shor code
  encoded <- encodeShor q'
  
  -- Extract a specific qubit and introduce both bit and phase errors
  let q5 = encoded !! 4  -- Middle qubit of the second group
  q5' <- pauliX q5
  q5'' <- pauliZ q5'
  
  -- Replace the errored qubit in the encoded list
  let encoded' = 
        take 4 encoded ++ [q5''] ++ drop 5 encoded
  
  -- Decode (error correction is built into the decoding process)
  q_decoded <- decodeShor encoded'
  
  -- Apply Hadamard to return to |0⟩ state
  q_final <- hadamard q_decoded
  
  -- Measure and observe that the state should be |0⟩
  (m, _) <- measure q_final
  
  pure [m]

-- | Main program to run the examples
main :: IO ()
main = do
  args <- getArgs
  
  -- Default to the bit-flip example if no arguments provided
  let exampleName = if null args then "bit-flip" else head args
  
  -- Select the appropriate example based on the argument
  let (circuit, description) = case exampleName of
        "bit-flip" -> (bitFlipExample, "Bit-Flip Error Correction")
        "phase-flip" -> (phaseFlipExample, "Phase-Flip Error Correction")
        "shor" -> (shorCodeExample, "Shor Code (Combined Error Correction)")
        _ -> (bitFlipExample, "Bit-Flip Error Correction (default)")
  
  -- Visualize the circuit
  let vis = visualizeCircuit circuit
  
  -- Display the circuit visualization
  putStrLn $ "HaskQ Error Correction Example: " ++ description
  putStrLn "=====================================================\n"
  TIO.putStrLn $ circuitToAscii vis
  
  -- Simulate the circuit
  let result = simulateCircuit 10 circuit  -- Need enough qubits for error correction
  
  -- Display the simulation results
  putStrLn "\nSimulation Results:"
  putStrLn "=====================================================\n"
  putStrLn $ "Final measurement: " ++ show (measurements result)
  putStrLn $ "Success: " ++ 
    case measurements result of
      [Zero] -> "Yes! Error was correctly detected and fixed"
      _ -> "No, error correction failed"
  
  -- Export the visualization to a file (optional)
  if length args > 1 && args !! 1 /= "-" then
    do
      let outFile = args !! 1
      exportVisualization outFile vis
      putStrLn $ "\nCircuit visualization exported to: " ++ outFile
  else
    pure () 