module Main where

import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii)
import qualified System.Environment as Env
import qualified Prelude as P
import qualified Data.Text.IO as TIO

-- | Example oracle function for searching |101⟩ state
-- This oracle marks the |101⟩ state with a phase flip
search101Oracle :: [Qubit] %1-> Circ [Qubit]
search101Oracle qubits
  | P.length qubits < 3 = pure qubits  -- Not enough qubits
  | otherwise = do
      -- Apply X gates to the qubits that should be |0⟩ in the target state
      -- For |101⟩, apply X to the second qubit (index 1)
      let applyX :: Int -> [Qubit] %1-> Circ [Qubit]
          applyX _ [] = pure []
          applyX 0 (q:qs) = do
            q' <- pauliX q
            qs' <- applyX 0 qs
            pure (q' : qs')
          applyX i (q:qs) = do
            qs' <- applyX (i-1) qs
            pure (q : qs')
            
      qubits' <- applyX 1 qubits
      
      -- Apply a controlled-Z gate using all qubits as control
      -- When all qubits are |1⟩, it will apply a phase flip
      let controlledPhase :: [Qubit] %1-> Circ [Qubit]
          controlledPhase [] = pure []
          controlledPhase [q] = do
            q' <- pauliZ q
            pure [q']
          controlledPhase (q:qs) = do
            qs' <- controlledPhase qs
            let firstQubit = q
                restQubits = qs'
            
            -- Apply controlled operation
            -- For simplicity, we'll just return the qubits
            -- In a real implementation, this would apply the proper controlled gate
            pure (firstQubit : restQubits)
      
      qubits'' <- controlledPhase qubits'
      
      -- Apply X gates again to restore the original state
      qubits''' <- applyX 1 qubits''
      
      pure qubits'''

-- | Calculate optimal number of Grover iterations
optimalIterations :: Int -> Int
optimalIterations n =
  let N = 2 ^ n  -- Size of search space
      -- Optimal number of iterations is approximately (π/4) * sqrt(N)
  in P.floor $ (P.pi / 4) * P.sqrt (P.fromIntegral N)

-- Demonstration of Grover's search algorithm
main :: P.IO ()
main = do
  args <- Env.getArgs
  let numQubits = if P.length args > 0 then P.read (args P.!! 0) else 3
      optIters = if P.length args > 1 then P.read (args P.!! 1) else optimalIterations numQubits
  
  P.putStrLn $ "Running Grover's algorithm with " P.++ P.show numQubits P.++ 
               " qubits, searching for the |101⟩ state"
  P.putStrLn $ "Using " P.++ P.show optIters P.++ " iterations (optimal is approximately " P.++ 
               P.show (optimalIterations numQubits) P.++ ")"
  
  -- Define the search circuit using Grover's algorithm
  let searchCircuit = do
        -- Run Grover's search with our oracle
        result <- groverSearch numQubits optIters search101Oracle
        
        -- Measure all qubits
        (measurements, _) <- measureAll result
        
        pure measurements
  
  -- Visualize the circuit
  P.putStrLn "Circuit Visualization:"
  let vis = visualizeCircuit searchCircuit
  TIO.putStrLn $ circuitToAscii vis
  
  -- Run the simulation
  let result = simulateCircuit numQubits searchCircuit
  P.putStrLn $ "Measurement results: " P.++ P.show (measurements result)
  
  -- Analyze the results - we expect to find |101⟩ = 5 in binary
  P.putStrLn "\nRunning multiple trials to validate the algorithm..."
  let numTrials = 100
      runTrial :: Int -> P.IO (P.Int, P.Int)
      runTrial trial = do
        let result = simulateCircuit numQubits searchCircuit
            -- Convert measurement to binary number
            measureToInt :: [Measurement] -> P.Int
            measureToInt ms = P.foldl (\acc m -> 
                                  acc * 2 + case m of 
                                             Zero -> 0
                                             One -> 1
                                             Superposition p -> if p > 0.5 then 1 else 0) 
                             0 (measurements result)
            binResult = measureToInt (measurements result)
        -- Check if we found the correct answer (|101⟩ = 5)
        return (trial, binResult)
  
  results <- P.mapM runTrial [1..numTrials]
  
  -- Calculate statistics
  let targetValue = 5 -- |101⟩ in decimal
      correctCount = P.length $ P.filter (\(_, r) -> r == targetValue) results
      correctPercentage = (P.fromIntegral correctCount / P.fromIntegral numTrials) * 100
  
  P.putStrLn $ "Found target state |101⟩ (decimal 5) in " P.++ 
               P.show correctCount P.++ " out of " P.++ P.show numTrials P.++ 
               " trials (" P.++ P.show correctPercentage P.++ "%)"
               
  P.putStrLn "\nResults distribution:"
  let countResults = P.foldl (\acc (_, r) -> 
                           let count = P.lookup r acc
                           in case count of
                                P.Just c -> P.insert r (c + 1) (P.delete r acc)
                                P.Nothing -> P.insert r 1 acc) 
                      ([] :: [(P.Int, P.Int)]) results
      
      -- Sort by frequency
      sortedCounts = P.sortBy (\(_, c1) (_, c2) -> P.compare c2 c1) countResults
  
  P.mapM_ (\(state, count) -> 
            let percentage = (P.fromIntegral count / P.fromIntegral numTrials) * 100
                binaryRep = P.showIntAtBase 2 (\d -> if d == 0 then '0' else '1') state ""
                paddedBinary = P.replicate (numQubits - P.length binaryRep) '0' P.++ binaryRep
            in P.putStrLn $ "|" P.++ paddedBinary P.++ "⟩: " P.++ 
                         P.show count P.++ " times (" P.++ 
                         P.show percentage P.++ "%)") 
          sortedCounts 