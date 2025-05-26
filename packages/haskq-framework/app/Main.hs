{-# LANGUAGE OverloadedStrings #-}

-- | HaskQ Framework Demo Application
-- 
-- This application demonstrates the power of the HaskQ Framework,
-- showcasing how elegant Haskell quantum programming seamlessly
-- integrates with high-performance Rust simulation.
module Main where

import Control.Monad.IO.Class
import Data.Complex
import qualified Data.Vector as V
import System.Exit (exitFailure)
import Text.Printf

import HaskQ.Framework.Core

-- | Main application entry point
main :: IO ()
main = do
  putStrLn "🚀 HaskQ Framework - Hybrid Quantum Computing"
  putStrLn "============================================"
  putStrLn ""
  
  putStrLn "🔬 Running Quantum Algorithm Demonstrations..."
  putStrLn ""
  
  -- Demonstrate different quantum algorithms
  demonstrateBellState
  demonstrateGroverSearch
  demonstrateQuantumTeleportation
  demonstrateQuantumFourierTransform
  demonstrateComplexCircuit
  
  putStrLn "✅ All demonstrations completed successfully!"

-- | Demonstrate Bell state creation and measurement
demonstrateBellState :: IO ()
demonstrateBellState = do
  putStrLn "📡 Bell State Creation:"
  putStrLn "   Creating maximally entangled state |00⟩ + |11⟩"
  
  result <- runQuantum 2 $ do
    [q0, q1] <- mapM qubit [0, 1]
    
    -- Create Bell state
    h q0           -- Hadamard on first qubit
    cnot q0 q1     -- CNOT with q0 as control, q1 as target
    
    -- Get amplitudes before measurement
    amps <- amplitudes
    probs <- probabilities
    
    return (amps, probs)
  
  case result of
    Left err -> putStrLn $ "   Error: " ++ show err
    Right (amps, probs) -> do
      putStrLn "   State amplitudes:"
      putStrLn $ "     |00⟩: " ++ show (V.head amps)
      putStrLn $ "     |01⟩: " ++ show (amps V.! 1)
      putStrLn $ "     |10⟩: " ++ show (amps V.! 2)
      putStrLn $ "     |11⟩: " ++ show (amps V.! 3)
      putStrLn "   Measurement probabilities:"
      putStrLn $ "     P(|00⟩) = " ++ printf "%.3f" (V.head probs)
      putStrLn $ "     P(|11⟩) = " ++ printf "%.3f" (probs V.! 3)
  putStrLn ""

-- | Demonstrate Grover's search algorithm
demonstrateGroverSearch :: IO ()
demonstrateGroverSearch = do
  putStrLn "🔍 Grover's Search Algorithm:"
  putStrLn "   Searching for marked state |10⟩ in 2-qubit space"
  
  result <- runQuantum 2 $ do
    qreg <- qubits 2
    
    -- Initialize uniform superposition
    let [q0, q1] = V.toList $ unQReg qreg
    h q0
    h q1
    
    -- Apply Grover operator (simplified for 2 qubits)
    groverOperator qreg [2]  -- Mark state |10⟩ (index 2)
    
    probs <- probabilities
    return probs
  
  case result of
    Left err -> putStrLn $ "   Error: " ++ show err
    Right probs -> do
      putStrLn "   Probabilities after Grover iteration:"
      forM_ (zip [0..] (V.toList probs)) $ \(i, p) -> do
        let state = show i ++ "⟩"
        putStrLn $ "     P(|" ++ state ++ " = " ++ printf "%.3f" p
      putStrLn $ "   Marked state |10⟩ has probability: " ++ printf "%.3f" (probs V.! 2)
  putStrLn ""

-- | Demonstrate quantum teleportation protocol
demonstrateQuantumTeleportation :: IO ()
demonstrateQuantumTeleportation = do
  putStrLn "📡 Quantum Teleportation Protocol:"
  putStrLn "   Teleporting quantum state from Alice to Bob"
  
  result <- runQuantum 3 $ do
    [alice, bob, charlie] <- mapM qubit [0, 1, 2]
    
    -- Prepare Alice's qubit in |+⟩ state
    h alice
    
    -- Create entangled pair between Bob and Charlie
    h bob
    cnot bob charlie
    
    -- Teleportation protocol
    (m1, m2) <- teleport alice bob charlie
    
    -- Measure final state
    measurements <- measureAll
    
    return (m1, m2, measurements)
  
  case result of
    Left err -> putStrLn $ "   Error: " ++ show err
    Right (m1, m2, final) -> do
      putStrLn $ "   Classical measurement results: (" ++ show m1 ++ ", " ++ show m2 ++ ")"
      putStrLn $ "   Final quantum state: " ++ show final
      putStrLn "   Teleportation complete!"
  putStrLn ""

-- | Demonstrate Quantum Fourier Transform
demonstrateQuantumFourierTransform :: IO ()
demonstrateQuantumFourierTransform = do
  putStrLn "🌊 Quantum Fourier Transform:"
  putStrLn "   Applying QFT to 3-qubit register"
  
  result <- runQuantum 3 $ do
    qreg <- qubits 3
    let [q0, q1, q2] = V.toList $ unQReg qreg
    
    -- Prepare initial state |001⟩
    x q2
    
    -- Apply QFT
    qft qreg
    
    -- Get final amplitudes
    amps <- amplitudes
    probs <- probabilities
    
    return (amps, probs)
  
  case result of
    Left err -> putStrLn $ "   Error: " ++ show err
    Right (amps, probs) -> do
      putStrLn "   QFT amplitudes (showing first 4 states):"
      forM_ (take 4 $ zip [0..] (V.toList amps)) $ \(i, amp) -> do
        putStrLn $ "     |" ++ show i ++ "⟩: " ++ show amp
      putStrLn "   QFT creates uniform superposition with phase relationships"
  putStrLn ""

-- | Demonstrate complex quantum circuit
demonstrateComplexCircuit :: IO ()
demonstrateComplexCircuit = do
  putStrLn "🔧 Complex Quantum Circuit:"
  putStrLn "   Multi-gate circuit with various quantum operations"
  
  result <- runQuantum 4 $ do
    [q0, q1, q2, q3] <- mapM qubit [0, 1, 2, 3]
    
    -- Complex circuit composition
    circuit [
      h q0,                    -- Hadamard
      rx (pi/4) q1,           -- X-rotation
      cnot q0 q1,             -- Entanglement
      ry (pi/3) q2,           -- Y-rotation
      cz q1 q2,               -- Controlled-Z
      toffoli q0 q1 q3,       -- Toffoli gate
      phase (pi/2) q3,        -- Phase gate
      swap q2 q3              -- SWAP gate
    ]
    
    -- Get final state information
    probs <- probabilities
    
    -- Calculate some quantum properties
    totalProb <- return $ V.sum probs
    entropy <- return $ -V.sum (V.map (\p -> if p > 0 then p * log p else 0) probs)
    
    return (probs, totalProb, entropy)
  
  case result of
    Left err -> putStrLn $ "   Error: " ++ show err
    Right (probs, total, entropy) -> do
      putStrLn $ "   Final state has " ++ show (V.length probs) ++ " amplitude components"
      putStrLn $ "   Total probability: " ++ printf "%.6f" total ++ " (should be ~1.0)"
      putStrLn $ "   Quantum entropy: " ++ printf "%.4f" entropy
      putStrLn "   Circuit executed successfully with multiple gate types!"
  putStrLn ""

-- Helper function for iteration
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs) 