module Main where

import HaskQ.Prelude
import HaskQ.Simulator.Circuit (simulateCircuit)
import HaskQ.Simulator.Visualizer (visualizeCircuit, circuitToAscii)
import qualified System.Environment as Env
import qualified Prelude as P
import qualified Data.Text.IO as TIO

-- Simple QFT example with visualization
main :: P.IO ()
main = do
  args <- Env.getArgs
  let numQubits = if P.length args > 0 then P.read (args P.!! 0) else 3
  
  P.putStrLn $ "Running QFT example with " P.++ P.show numQubits P.++ " qubits"
  
  -- Circuit for creating a superposition and applying QFT
  let qftCircuit = do
        -- Create n qubits
        qubits <- withQubits numQubits (\qs -> P.pure qs)
        
        -- Apply Hadamard to first qubit to create a superposition
        qubits' <- applyHadamardToFirst qubits
        
        -- Apply QFT
        qftResult <- qft qubits'
        
        -- Measure all qubits
        (measurements, _) <- measureAll qftResult
        
        P.pure measurements
      
      -- Apply Hadamard to the first qubit in a list
      applyHadamardToFirst :: [Qubit] %1-> P.Circ [Qubit]
      applyHadamardToFirst [] = P.pure []
      applyHadamardToFirst (q:qs) = do
        q' <- hadamard q
        P.pure (q' : qs)
  
  -- Visualize the circuit
  P.putStrLn "Circuit Visualization:"
  let vis = visualizeCircuit qftCircuit
  TIO.putStrLn $ circuitToAscii vis
  
  -- Simulate the circuit
  let result = simulateCircuit numQubits qftCircuit
  P.putStrLn $ "Measurement results: " P.++ P.show (measurements result)
  
  -- Try the inverse QFT
  P.putStrLn "\nInverse QFT Example:"
  let inverseQftCircuit = do
        -- Create n qubits in superposition
        qubits <- withQubits numQubits (\qs -> P.pure qs)
        qubits' <- applyHadamardToAll qubits
        
        -- Apply QFT
        qftResult <- qft qubits'
        
        -- Apply inverse QFT (should return to original state)
        originalState <- inverseQft qftResult
        
        -- Measure all qubits
        (measurements, _) <- measureAll originalState
        
        P.pure measurements
      
      -- Apply Hadamard to all qubits
      applyHadamardToAll :: [Qubit] %1-> P.Circ [Qubit]
      applyHadamardToAll [] = P.pure []
      applyHadamardToAll (q:qs) = do
        q' <- hadamard q
        qs' <- applyHadamardToAll qs
        P.pure (q' : qs')
  
  -- Visualize the inverse QFT circuit
  let invVis = visualizeCircuit inverseQftCircuit
  TIO.putStrLn $ circuitToAscii invVis
  
  -- Simulate the inverse QFT circuit
  let invResult = simulateCircuit numQubits inverseQftCircuit
  P.putStrLn $ "Inverse QFT results: " P.++ P.show (measurements invResult)
  P.putStrLn "Note: For perfect QFT + inverse QFT, we expect to return to the initial superposition state" 