---
sidebar_position: 9
---

# Surface Codes

Surface codes are the most promising quantum error correction codes for building large-scale fault-tolerant quantum computers. They combine relatively high error thresholds with a practical 2D nearest-neighbor architecture.

## Introduction to Surface Codes

Surface codes encode quantum information in the topological features of a 2D lattice of physical qubits. Their key advantages include:

1. **High error threshold** (~1%, compared to <0.1% for many other codes)
2. **Local operations** (gates only between neighboring qubits)
3. **Scalable design** (code distance can be increased by adding more qubits)
4. **Compatibility with physical implementations** (superconducting, trapped ion, etc.)

## Surface Code Structure

A surface code consists of data qubits arranged in a 2D grid with syndrome qubits (or ancilla qubits) placed on the vertices or faces:

```
Z: Syndrome qubit for Z-type stabilizers
X: Syndrome qubit for X-type stabilizers
D: Data qubit

Z---D---Z---D---Z
|       |       |
D   X   D   X   D
|       |       |
Z---D---Z---D---Z
|       |       |
D   X   D   X   D
|       |       |
Z---D---Z---D---Z
```

## Stabilizers in Surface Codes

Surface codes use two types of stabilizer operators:

1. **X-type stabilizers**: Product of X operators on data qubits around a face
2. **Z-type stabilizers**: Product of Z operators on data qubits around a vertex

These stabilizers detect phase flip and bit flip errors, respectively.

## Surface Code Implementation in HaskQ

HaskQ provides tools for implementing and simulating surface codes:

```haskell
-- Define a distance-d surface code
createSurfaceCode :: Int -> Circ [Qubit]
createSurfaceCode d = do
  -- Calculate number of physical qubits required
  let numDataQubits = d * d
      numSyndromeQubits = (d-1) * (d-1) * 2
      totalQubits = numDataQubits + numSyndromeQubits
  
  -- Create physical qubits
  physicalQubits <- withQubits totalQubits (\qs -> pure qs)
  
  -- Initialize in |0⟩ state (or encoded logical state if needed)
  pure physicalQubits

-- Define the qubit layout for a distance-d surface code
surfaceCodeLayout :: Int -> [(Int, Int, QubitType)]
surfaceCodeLayout d = 
  -- Generate data qubit coordinates
  let dataQubits = [(x, y, DataQubit) | x <- [0..d-1], y <- [0..d-1]]
      -- Generate X syndrome qubit coordinates (faces)
      xSyndromes = [(x+0.5, y+0.5, XSyndromeQubit) | x <- [0..d-2], y <- [0..d-2]]
      -- Generate Z syndrome qubit coordinates (vertices)
      zSyndromes = [(x, y, ZSyndromeQubit) | x <- [1..d-1], y <- [1..d-1]]
  in dataQubits ++ xSyndromes ++ zSyndromes

-- Initialize a logical |0⟩ state in the surface code
prepareLogicalZero :: Int -> Circ [Qubit]
prepareLogicalZero d = do
  -- Create physical qubits
  qubits <- createSurfaceCode d
  
  -- Apply stabilizer measurements to project into the codespace
  qubits' <- measureStabilizersAndCorrect qubits
  
  pure qubits'
```

## Syndrome Extraction

Measuring stabilizers is crucial for error detection in surface codes:

```haskell
-- Measure all stabilizers in a surface code
measureAllStabilizers :: [Qubit] %1-> Circ ([Qubit], [Measurement])
measureAllStabilizers qubits = do
  -- Get layout information
  let d = determineDistance qubits
      layout = surfaceCodeLayout d
  
  -- Measure X stabilizers (for Z errors)
  (qubits', xSyndromes) <- measureXStabilizers qubits layout
  
  -- Measure Z stabilizers (for X errors)
  (qubits'', zSyndromes) <- measureZStabilizers qubits' layout
  
  pure (qubits'', xSyndromes ++ zSyndromes)

-- Measure a single X-type stabilizer (detects Z errors)
measureXStabilizer :: [Qubit] %1-> [(Int, Int)] -> Circ ([Qubit], Measurement)
measureXStabilizer qubits dataCoords = do
  -- Get data qubits involved in this stabilizer
  let dataIndices = [coordToIndex coord | coord <- dataCoords]
      dataQubits = [qubits !! idx | idx <- dataIndices]
  
  -- Create ancilla in |+⟩ state
  ancilla <- qubit
  ancilla' <- hadamard ancilla
  
  -- Apply CNOT from ancilla to each data qubit
  (ancilla'', dataQubits') <- foldM applyCNOT (ancilla', []) dataQubits
  
  -- Measure ancilla in X basis
  ancilla''' <- hadamard ancilla''
  (result, _) <- measure ancilla'''
  
  -- Replace data qubits in the original list
  let qubits' = replaceQubitsAt qubits dataIndices dataQubits'
  
  pure (qubits', result)
```

## Error Decoding

After measuring syndromes, we need to identify the most likely error pattern:

```haskell
-- Decode error syndromes using minimum weight perfect matching
decodeSyndromes :: [Measurement] -> Int -> [ErrorType]
decodeSyndromes syndromes d = do
  -- Convert syndromes to syndrome graph
  let graph = createSyndromeGraph syndromes d
  
  -- Apply minimum weight perfect matching algorithm
  let matchings = minimumWeightPerfectMatching graph
  
  -- Convert matchings to error chain
  let errorChain = matchingsToErrorChain matchings d
  
  pure errorChain

-- Apply corrections based on decoded error chain
applyCorrections :: [Qubit] %1-> [ErrorType] -> Circ [Qubit]
applyCorrections qubits errorChain = do
  -- Apply X corrections for Z errors
  qubits' <- applyXCorrections qubits errorChain
  
  -- Apply Z corrections for X errors
  qubits'' <- applyZCorrections qubits' errorChain
  
  pure qubits''
```

## Logical Operations on Surface Codes

Performing logical operations on surface-code-encoded qubits:

```haskell
-- Apply a logical X operator (along a vertical path)
applyLogicalX :: [Qubit] %1-> Int -> Circ [Qubit]
applyLogicalX qubits d = do
  -- Get the indices of qubits along a vertical path
  let pathIndices = getVerticalPathIndices d
      pathQubits = [qubits !! idx | idx <- pathIndices]
  
  -- Apply X gates to all qubits on the path
  pathQubits' <- mapM pauliX pathQubits
  
  -- Replace path qubits in the full list
  let qubits' = replaceQubitsAt qubits pathIndices pathQubits'
  
  pure qubits'

-- Apply a logical Z operator (along a horizontal path)
applyLogicalZ :: [Qubit] %1-> Int -> Circ [Qubit]
applyLogicalZ qubits d = do
  -- Get the indices of qubits along a horizontal path
  let pathIndices = getHorizontalPathIndices d
      pathQubits = [qubits !! idx | idx <- pathIndices]
  
  -- Apply Z gates to all qubits on the path
  pathQubits' <- mapM pauliZ pathQubits
  
  -- Replace path qubits in the full list
  let qubits' = replaceQubitsAt qubits pathIndices pathQubits'
  
  pure qubits'
```

## Logical CNOT in Surface Codes

Implementing a logical CNOT gate is more complex and can be done through lattice surgery:

```haskell
-- Perform lattice surgery CNOT between two surface codes
latticeSurgeryCNOT :: [Qubit] %1-> [Qubit] %1-> Int -> Circ ([Qubit], [Qubit])
latticeSurgeryCNOT controlQubits targetQubits d = do
  -- 1. Create a connecting region between the codes
  mergedQubits <- mergeCodes controlQubits targetQubits
  
  -- 2. Measure joint stabilizers across the boundary
  (mergedQubits', syndromes) <- measureJointStabilizers mergedQubits
  
  -- 3. Apply corrections based on syndrome measurements
  mergedQubits'' <- applyMergeCorrections mergedQubits' syndromes
  
  -- 4. Split the codes back
  (controlQubits', targetQubits') <- splitCodes mergedQubits'' d
  
  pure (controlQubits', targetQubits')
```

## Complete Example: Error Detection and Correction

Running a full cycle of surface code error detection and correction:

```haskell
-- Run a complete error correction cycle on a surface code
surfaceCodeCycle :: [Qubit] %1-> Double -> Circ [Qubit]
surfaceCodeCycle qubits errorRate = do
  -- Determine code distance
  let d = determineDistance qubits
  
  -- 1. Introduce random errors based on the error rate
  qubits' <- introduceRandomErrors qubits errorRate
  
  -- 2. Measure all stabilizers
  (qubits'', syndromes) <- measureAllStabilizers qubits'
  
  -- 3. Decode the syndromes to find likely errors
  let errorChain = decodeSyndromes syndromes d
  
  -- 4. Apply corrections based on the error chain
  correctedQubits <- applyCorrections qubits'' errorChain
  
  pure correctedQubits
```

## Logical Error Rates and Thresholds

Evaluating the performance of surface codes:

```haskell
-- Calculate logical error rate for a surface code of given distance
calculateLogicalErrorRate :: Int -> Double -> Int -> IO Double
calculateLogicalErrorRate distance physicalErrorRate numTrials = do
  -- Run multiple trials
  results <- forM [1..numTrials] $ \_ -> do
    -- Create a logical state
    state <- runCirc $ do
      qubits <- prepareLogicalZero distance
      pure qubits
    
    -- Run error correction cycles
    correctedState <- runCirc $ do
      let numCycles = distance  -- Run d cycles
      foldM (\qs _ -> surfaceCodeCycle qs physicalErrorRate) state [1..numCycles]
    
    -- Measure logical state and check if it's still |0⟩
    result <- runCirc $ measureLogicalState correctedState
    pure (result == Zero)
  
  -- Calculate the logical error rate
  let successCount = length (filter id results)
      logicalErrorRate = 1.0 - (fromIntegral successCount / fromIntegral numTrials)
  
  pure logicalErrorRate
```

## Visualizing Surface Codes

HaskQ provides tools to visualize surface codes and error correction:

```haskell
-- Visualize a surface code state
visualizeSurfaceCode :: [Qubit] -> String
visualizeSurfaceCode qubits = do
  -- Determine code distance
  let d = determineDistance qubits
      layout = surfaceCodeLayout d
  
  -- Create grid representation
  let grid = createGridVisualization qubits layout
  
  -- Convert grid to ASCII art
  let asciiArt = gridToAscii grid
  
  asciiArt

-- Example output for a distance-3 code might look like:
--
-- Z(+)─D(0)─Z(+)─D(0)─Z(+)
--  │     │     │     │
-- D(0) X(+) D(1) X(+) D(0)
--  │     │     │     │
-- Z(+)─D(0)─Z(-)─D(1)─Z(+)
--  │     │     │     │
-- D(0) X(+) D(0) X(-) D(0)
--  │     │     │     │
-- Z(+)─D(0)─Z(+)─D(0)─Z(+)
```

## Running Surface Code Simulations in HaskQ

HaskQ provides a command-line tool for simulating surface codes:

```bash
# Run a surface code simulation with distance 3 and error rate 0.01
cabal run haskq-surface-code -- --distance 3 --error-rate 0.01 --cycles 10

# Generate a threshold curve by running simulations at multiple error rates
cabal run haskq-surface-code -- --threshold-curve --distances "3,5,7" --error-rates "0.001,0.005,0.01,0.02,0.05"
```

## Practical Considerations

When working with surface codes in HaskQ:

1. **Simulation costs**: Surface code simulations with classical computers become expensive for large distances
2. **Decoding algorithms**: Minimum weight perfect matching is standard, but other decoders can be implemented
3. **Syndrome extraction circuits**: Different implementations affect the fault-tolerance threshold
4. **Realistic noise models**: Use appropriate noise models for your target hardware

## Next Steps

To deepen your understanding of surface codes:

1. Experiment with different code distances and error rates
2. Implement and test various error decoders
3. Try advanced techniques like code deformation and lattice surgery
4. Explore color codes and other topological quantum error correction approaches 