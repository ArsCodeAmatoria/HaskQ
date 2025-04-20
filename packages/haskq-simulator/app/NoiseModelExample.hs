{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuitWithNoise)
import HaskQ.Simulator.Noise
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii, exportVisualization)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Monad (replicateM)
import Text.Printf (printf)

-- | Bell state preparation circuit
bellStateCircuit :: Circ [Measurement]
bellStateCircuit = do
  -- Create two qubits in |0⟩ state
  q1 <- qubit
  q2 <- qubit
  
  -- Create Bell state |Φ⁺⟩ = (|00⟩ + |11⟩)/√2
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  
  -- Measure both qubits
  (m1, _) <- measure q1''
  (m2, _) <- measure q2'
  
  pure [m1, m2]

-- | Multiple-run statistics for Bell state
bellStateStatistics :: Int -> NoiseModel -> IO ()
bellStateStatistics numRuns model = do
  -- Run the circuit multiple times and collect results
  results <- replicateM numRuns $ do
    output <- simulateCircuitWithNoise 2 model bellStateCircuit
    pure (measurements output)
  
  -- Count occurrences of each outcome
  let countResults = foldr countMeasurement (0, 0, 0, 0) results
      (count00, count01, count10, count11) = countResults
      
      -- Calculate percentages
      total = fromIntegral numRuns :: Double
      pct00 = 100.0 * fromIntegral count00 / total
      pct01 = 100.0 * fromIntegral count01 / total
      pct10 = 100.0 * fromIntegral count10 / total
      pct11 = 100.0 * fromIntegral count11 / total
      
  -- Print statistics
  putStrLn $ "Results from " ++ show numRuns ++ " runs with noise model:"
  putStrLn $ "  |00⟩: " ++ show count00 ++ " (" ++ printf "%.2f" pct00 ++ "%)"
  putStrLn $ "  |01⟩: " ++ show count01 ++ " (" ++ printf "%.2f" pct01 ++ "%)"
  putStrLn $ "  |10⟩: " ++ show count10 ++ " (" ++ printf "%.2f" pct10 ++ "%)"
  putStrLn $ "  |11⟩: " ++ show count11 ++ " (" ++ printf "%.2f" pct11 ++ "%)"
  putStrLn ""
  putStrLn $ "Fidelity: " ++ printf "%.2f" ((pct00 + pct11) / 100.0) ++ " (ideal: 1.0)"
  where
    countMeasurement [Zero, Zero] (c00, c01, c10, c11) = (c00 + 1, c01, c10, c11)
    countMeasurement [Zero, One] (c00, c01, c10, c11) = (c00, c01 + 1, c10, c11)
    countMeasurement [One, Zero] (c00, c01, c10, c11) = (c00, c01, c10 + 1, c11)
    countMeasurement [One, One] (c00, c01, c10, c11) = (c00, c01, c10, c11 + 1)
    countMeasurement _ counts = counts  -- Handle unexpected cases

-- | GHZ state preparation circuit
ghzStateCircuit :: Int -> Circ [Measurement]
ghzStateCircuit n = do
  -- Create n qubits in |0⟩ state
  qubits <- replicateM n qubit
  
  -- Apply Hadamard to the first qubit
  q1' <- hadamard (head qubits)
  
  -- Apply CNOT gates to entangle all qubits
  (q1'', rest') <- foldM applyCNOT (q1', tail qubits) [0..(n-2)]
  
  -- Measure all qubits
  measurements <- measureAll (q1'' : rest')
  
  pure (fst measurements)
  where
    applyCNOT (control, target:rest) _ = do
      (control', target') <- cnot control target
      pure (control', target':rest)
    applyCNOT (control, []) _ = pure (control, [])

-- | Main program to run the noise model examples
main :: IO ()
main = do
  args <- getArgs
  
  -- Default parameters
  let noiseStrength = if length args > 0
                      then read (head args) :: Double
                      else 0.05 -- 5% noise by default
                      
      modelType = if length args > 1
                  then args !! 1
                  else "depolarizing"
                  
      numRuns = if length args > 2
                then read (args !! 2) :: Int
                else 1000
                
  -- Select the noise model based on the argument
  let noiseModel = case modelType of
        "depolarizing" -> depolarizingNoise noiseStrength
        "bitflip" -> bitFlipNoise noiseStrength
        "phaseflip" -> phaseFlipNoise noiseStrength
        "amplitude" -> amplitudeDampingNoise noiseStrength
        "phase" -> phaseDampingNoise noiseStrength
        "thermal" -> thermalNoise noiseStrength 1.0
        _ -> depolarizingNoise noiseStrength  -- Default to depolarizing noise
  
  -- Print information about the noise model
  putStrLn $ "HaskQ Noise Model Example"
  putStrLn "=============================="
  putStrLn $ "Noise type: " ++ modelType
  putStrLn $ "Noise strength: " ++ show noiseStrength
  putStrLn $ "Number of runs: " ++ show numRuns
  putStrLn ""
  
  -- Visualize the bell state circuit
  let bellVis = visualizeCircuit bellStateCircuit
  putStrLn "Bell State Circuit:"
  putStrLn "=============================="
  TIO.putStrLn $ circuitToAscii bellVis
  putStrLn ""
  
  -- Run statistics for Bell state with noise
  putStrLn "Bell State Statistics with Noise:"
  putStrLn "=============================="
  bellStateStatistics numRuns noiseModel
  
  -- Also demonstrate GHZ state if requested
  when (length args > 3 && args !! 3 == "ghz") $ do
    let ghzSize = if length args > 4
                  then read (args !! 4) :: Int
                  else 3  -- Default to 3-qubit GHZ
    
    putStrLn $ "GHZ State (" ++ show ghzSize ++ " qubits) Circuit:"
    putStrLn "=============================="
    
    let ghzVis = visualizeCircuit (ghzStateCircuit ghzSize)
    TIO.putStrLn $ circuitToAscii ghzVis
    
    -- A simple GHZ state run with noise (just one run for demonstration)
    putStrLn "\nGHZ State with Noise (single run):"
    putStrLn "=============================="
    
    ghzResult <- simulateCircuitWithNoise ghzSize noiseModel (ghzStateCircuit ghzSize)
    putStrLn $ "Measurements: " ++ show (measurements ghzResult)
    
    let idealGHZ = all (== Zero) (measurements ghzResult) || all (== One) (measurements ghzResult)
    putStrLn $ "Perfect GHZ state: " ++ (if idealGHZ then "Yes" else "No") ++ "\n"

-- Utility function to simulate measurement of all qubits
measureAll :: [Qubit] %1-> Circ ([Measurement], [Qubit])
measureAll [] = pure ([], [])
measureAll (q:qs) = do
  (m, q') <- measure q
  (ms, qs') <- measureAll qs
  pure (m:ms, q':qs') 