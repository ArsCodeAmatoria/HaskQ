  {
    id: 'mond-simulation',
    name: "Modified Gravity (MOND) Simulation",
    description: "Quantum modeling of Modified Newtonian Dynamics from Romulus research",
    difficulty: 'advanced',
    code: `-- Modified Gravity Quantum Simulation
-- Implementing MOND and emergent gravity theories from Romulus

module ModifiedGravity where

-- MOND (Modified Newtonian Dynamics) quantum field
mondField :: Double -> Circ Qubit
mondField acceleration = do
  q <- createQubit Zero
  if acceleration < mondConstant
    then do
      -- Low acceleration regime: modified dynamics
      q' <- rotateY (sqrt acceleration) q
      pure q'
    else do
      -- High acceleration: Newtonian behavior
      q' <- rotateY acceleration q
      pure q'
  where
    mondConstant = 1.2e-10  -- MOND acceleration constant

-- Emergent gravity from quantum entanglement
emergentGravity :: Circ (Qubit, Qubit)
emergentGravity = withQubits 2 $ \\[matter1, matter2] -> do
  -- Create entanglement that emerges as gravitational force
  matter1' <- hadamard matter1
  (matter1'', matter2') <- cnot matter1' matter2
  
  -- Apply phase that represents gravitational potential
  matter1''' <- rotateZ (-pi/4) matter1''  -- Attractive force
  matter2'' <- rotateZ (-pi/4) matter2'
  
  pure (matter1''', matter2'')

-- Dark matter alternative: modified quantum vacuum
quantumVacuumModification :: Circ [Qubit]
quantumVacuumModification = withQubits 4 $ \\[q1, q2, q3, q4] -> do
  -- Create vacuum fluctuations that mimic dark matter effects
  qs' <- mapM hadamard [q1, q2, q3, q4]
  -- ... complex entanglement pattern ...
  pure qs'`,
  },

  {
    id: 'consciousness-iit',
    name: "Consciousness Integration (IIT)",
    description: "Quantum implementation of Integrated Information Theory from consciousness research",
    difficulty: 'advanced',
    code: `-- Integrated Information Theory (IIT) Quantum Implementation
-- Modeling consciousness as integrated information (Φ)

module ConsciousnessIIT where

-- Phi (Φ) calculation: measure of consciousness
calculatePhi :: [Qubit] -> Circ Double
calculatePhi qubits = do
  -- Calculate integrated information across subsystems
  wholeSystemInfo <- systemInformation qubits
  partitionedInfo <- maxPartitionInfo qubits
  let phi = wholeSystemInfo - partitionedInfo
  pure phi

-- Measure information in quantum system
systemInformation :: [Qubit] -> Circ Double
systemInformation qubits = do
  -- Quantum mutual information between subsystems
  correlations <- measureCorrelations qubits
  entropy <- vonNeumannEntropy qubits
  pure (entropy - correlations)

-- Consciousness as quantum coherence
consciousnessCoherence :: [Qubit] -> Circ Double
consciousnessCoherence qubits = do
  -- Measure quantum coherence across all qubits
  coherenceValues <- mapM quantumCoherence qubits
  let totalCoherence = sum coherenceValues
  pure totalCoherence

-- Global Workspace Theory quantum model
globalWorkspace :: [Qubit] -> Circ [Qubit]
globalWorkspace qubits = do
  -- Create global entanglement (broadcast)
  let workspace = take 2 qubits  -- Central workspace
      modules = drop 2 qubits    -- Cognitive modules
  
  -- Entangle workspace with all modules
  entangledSystem <- foldM connectToWorkspace workspace modules
  pure entangledSystem

connectToWorkspace :: [Qubit] -> Qubit -> Circ [Qubit]
connectToWorkspace workspace module_q = do
  -- Connect module to global workspace
  case workspace of
    (w1:w2:ws) -> do
      (w1', module_q') <- cnot w1 module_q
      (w2', module_q'') <- cnot w2 module_q'
      pure (w1':w2':ws)
    _ -> pure workspace

-- Consciousness detection threshold
consciousnessThreshold :: [Qubit] -> Circ Bool
consciousnessThreshold qubits = do
  phi <- calculatePhi qubits
  coherence <- consciousnessCoherence qubits
  let consciousnessLevel = phi + coherence
  pure (consciousnessLevel > 0.5)  -- Arbitrary threshold`,
  },

  {
    id: 'quantum-ml',
    name: "Quantum Machine Learning",
    description: "Quantum neural network and variational classifier implementation",
    difficulty: 'advanced', 
    code: `-- Quantum Machine Learning Algorithms
module QuantumML where

-- Variational Quantum Classifier
quantumClassifier :: [Double] -> [Qubit] -> Circ [Measurement]
quantumClassifier params qubits = do
  -- Apply parameterized quantum circuit
  qubits' <- variationalAnsatz params qubits
  -- Measure in computational basis
  mapM measureQubit qubits'

-- Parameterized quantum circuit (ansatz)
variationalAnsatz :: [Double] -> [Qubit] -> Circ [Qubit]
variationalAnsatz params qubits = go params qubits 0
  where
    go [] qs _ = pure qs
    go (p:ps) qs layer = do
      -- Layer of single-qubit rotations
      qs' <- zipWithM rotateY (repeat p) qs
      -- Entangling layer
      qs'' <- entanglingLayer qs'
      go ps qs'' (layer + 1)

-- Entangling layer (circular connectivity)
entanglingLayer :: [Qubit] -> Circ [Qubit]
entanglingLayer qubits = do
  let pairs = zip qubits (tail qubits ++ [head qubits])
  foldM (\\qs (q1, q2) -> do
    let idx1 = findIndex q1 qs
        idx2 = findIndex q2 qs
    case (idx1, idx2) of
      (Just i1, Just i2) -> do
        let qi = qs !! i1
            qj = qs !! i2
        (qi', qj') <- cnot qi qj
        pure $ updateAt i1 qi' $ updateAt i2 qj' qs
      _ -> pure qs
  ) qubits pairs

-- Quantum Support Vector Machine
qsvm :: [([Double], Int)] -> [Double] -> Circ Int
qsvm trainingData testPoint = do
  -- Encode training data
  trainingStates <- mapM (encodeDataPoint . fst) trainingData
  testState <- encodeDataPoint testPoint
  
  -- Compute quantum kernel values
  kernels <- mapM (quantumKernel testState) trainingStates
  
  -- Classical post-processing (simplified)
  let labels = map snd trainingData
      weightedSum = sum $ zipWith (*) kernels (map fromIntegral labels)
  
  pure $ if weightedSum > 0 then 1 else -1

-- Data encoding into quantum states
encodeDataPoint :: [Double] -> Circ Qubit
encodeDataPoint features = do
  q <- createQubit Zero
  -- Amplitude encoding
  foldM (\\qubit angle -> rotateY angle qubit) q features

-- Quantum kernel computation
quantumKernel :: Qubit -> Qubit -> Circ Double
quantumKernel q1 q2 = do
  -- Swap test for state overlap
  ancilla <- createQubit Zero
  ancilla' <- hadamard ancilla
  (ancilla'', q1', q2') <- controlledSwap ancilla' q1 q2
  ancilla''' <- hadamard ancilla''
  measurement <- measureQubit ancilla'''
  pure $ if measurement == Zero then 1.0 else 0.0

-- Quantum Approximate Optimization Algorithm (QAOA)
qaoa :: [(Double, Double)] -> [Qubit] -> [(Int, Int)] -> Circ [Qubit]
qaoa parameters qubits edges = 
  foldM (\\qs (gamma, beta) -> qaoaLayer gamma beta qs edges) qubits parameters

qaoaLayer :: Double -> Double -> [Qubit] -> [(Int, Int)] -> Circ [Qubit]
qaoaLayer gamma beta qubits edges = do
  -- Apply cost Hamiltonian
  qs1 <- costHamiltonian gamma qubits edges
  -- Apply mixer Hamiltonian
  qs2 <- mixerHamiltonian beta qs1
  pure qs2

costHamiltonian :: Double -> [Qubit] -> [(Int, Int)] -> Circ [Qubit]
costHamiltonian gamma qubits edges = do
  foldM (\\qs (i, j) -> do
    let qi = qs !! i
        qj = qs !! j
    (qi', qj') <- zzRotation gamma qi qj
    pure $ updateAt i qi' $ updateAt j qj' qs
  ) qubits edges

mixerHamiltonian :: Double -> [Qubit] -> Circ [Qubit]
mixerHamiltonian beta qubits = 
  mapM (rotateX beta) qubits`,
  },

  {
    id: 'quantum-simulation',
    name: "Molecular Quantum Simulation",
    description: "Simulate molecular systems using Variational Quantum Eigensolver (VQE)",
    difficulty: 'advanced',
    code: `-- Molecular Quantum Simulation with VQE
module MolecularSimulation where

-- Hydrogen molecule simulation
hydrogenMolecule :: [Double] -> Circ Double
hydrogenMolecule parameters = do
  -- Two qubits represent H₂ minimal basis
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  
  -- Apply molecular ansatz
  [q1', q2'] <- molecularAnsatz parameters [q1, q2]
  
  -- Measure energy expectation value
  energy <- measureMolecularEnergy [q1', q2']
  pure energy

-- Molecular orbital ansatz
molecularAnsatz :: [Double] -> [Qubit] -> Circ [Qubit]
molecularAnsatz [theta1, theta2, theta3] [q1, q2] = do
  -- Single excitation: |01⟩ ↔ |10⟩
  q1' <- rotateY theta1 q1
  (q1'', q2') <- cnot q1' q2
  q2'' <- rotateY theta2 q2'
  (q1''', q2''') <- cnot q1'' q2''
  q1'''' <- rotateY theta3 q1'''
  pure [q1'''', q2''']

-- Simulate H₂ Hamiltonian measurement
measureMolecularEnergy :: [Qubit] -> Circ Double
measureMolecularEnergy [q1, q2] = do
  -- Pauli-Z measurements on both qubits
  m1 <- measureQubit q1
  m2 <- measureQubit q2
  
  -- H₂ Hamiltonian coefficients (simplified)
  let h_ii = -1.0523732  -- Diagonal terms
      h_12 = -0.39793742 -- Off-diagonal
      
  -- Calculate energy based on measurements
  let z1 = if m1 == Zero then 1.0 else -1.0
      z2 = if m2 == Zero then 1.0 else -1.0
      energy = h_ii * (z1 + z2) + h_12 * z1 * z2
  
  pure energy

-- Water molecule (H₂O) simulation
waterMolecule :: [Double] -> Circ Double
waterMolecule params = do
  -- Six qubits for minimal basis H₂O
  qubits <- replicateM 6 (createQubit Zero)
  
  -- Apply water ansatz
  qubits' <- waterAnsatz params qubits
  
  -- Measure total energy
  energy <- measureWaterEnergy qubits'
  pure energy

waterAnsatz :: [Double] -> [Qubit] -> Circ [Qubit]
waterAnsatz params qubits = do
  -- Hardware-efficient ansatz for water
  -- Layer 1: Single-qubit rotations
  qubits1 <- zipWithM rotateY (take 6 params) qubits
  
  -- Layer 2: Entangling gates
  qubits2 <- entangleAdjacent qubits1
  
  -- Layer 3: More rotations
  qubits3 <- zipWithM rotateY (drop 6 params) qubits2
  
  pure qubits3

entangleAdjacent :: [Qubit] -> Circ [Qubit]
entangleAdjacent qubits = do
  let pairs = zip qubits (tail qubits)
  foldM (\\qs (q1, q2) -> do
    let i1 = findQubitIndex q1 qs
        i2 = findQubitIndex q2 qs
    case (i1, i2) of
      (Just idx1, Just idx2) -> do
        let qi = qs !! idx1
            qj = qs !! idx2
        (qi', qj') <- cnot qi qj
        pure $ updateAt idx1 qi' $ updateAt idx2 qj' qs
      _ -> pure qs
  ) qubits pairs

measureWaterEnergy :: [Qubit] -> Circ Double
measureWaterEnergy qubits = do
  measurements <- mapM measureQubit qubits
  -- Simplified energy calculation
  let energy = sum $ zipWith (*) [1.0, -0.5, 0.3, -0.2, 0.1, -0.1] 
                     [if m == Zero then 1.0 else -1.0 | m <- measurements]
  pure energy

-- Drug molecule docking simulation
drugDocking :: [Double] -> [Double] -> Circ Double
drugDocking drugParams targetParams = do
  -- Encode drug molecule
  drugQubits <- replicateM 4 (createQubit Zero)
  drugState <- molecularAnsatz drugParams drugQubits
  
  -- Encode target protein site
  targetQubits <- replicateM 4 (createQubit Zero)
  targetState <- molecularAnsatz targetParams targetQubits
  
  -- Calculate binding affinity (overlap)
  affinity <- measureBindingAffinity drugState targetState
  pure affinity

measureBindingAffinity :: [Qubit] -> [Qubit] -> Circ Double
measureBindingAffinity drug target = do
  -- Quantum state overlap measurement
  overlap <- stateOverlap drug target
  pure overlap

stateOverlap :: [Qubit] -> [Qubit] -> Circ Double
stateOverlap qs1 qs2 = do
  -- Simplified overlap calculation
  measurements1 <- mapM measureQubit qs1
  measurements2 <- mapM measureQubit qs2
  let matches = length $ filter (uncurry (==)) $ zip measurements1 measurements2
      overlap = fromIntegral matches / fromIntegral (length qs1)
  pure overlap`,
  },

  {
    id: 'quantum-cryptography', 
    name: "Quantum Cryptography",
    description: "Quantum key distribution and cryptographic protocols",
    difficulty: 'intermediate',
    code: `-- Quantum Cryptography Protocols
module QuantumCryptography where

-- BB84 Quantum Key Distribution Protocol
bb84Protocol :: [Bit] -> [Basis] -> Circ [Bit]
bb84Protocol bits bases = do
  -- Alice prepares qubits
  qubits <- prepareQubits bits bases
  
  -- Bob measures with random bases
  bobBases <- generateRandomBases (length bits)
  measurements <- measureWithBases qubits bobBases
  
  -- Classical post-processing (basis reconciliation)
  let sharedKey = [bit | (bit, basis1, basis2) <- zip3 bits bases bobBases, 
                   basis1 == basis2]
  pure sharedKey

-- Prepare qubits according to BB84 protocol
prepareQubits :: [Bit] -> [Basis] -> Circ [Qubit]
prepareQubits bits bases = 
  zipWithM prepareQubit bits bases

prepareQubit :: Bit -> Basis -> Circ Qubit
prepareQubit bit basis = do
  q <- createQubit Zero
  q' <- if bit == One then gateX q else pure q
  q'' <- if basis == DiagonalBasis then hadamard q' else pure q'
  pure q''

-- Measure qubits with chosen bases
measureWithBases :: [Qubit] -> [Basis] -> Circ [Bit]
measureWithBases qubits bases =
  zipWithM measureWithBasis qubits bases

measureWithBasis :: Qubit -> Basis -> Circ Bit
measureWithBasis qubit basis = do
  q' <- if basis == DiagonalBasis then hadamard qubit else pure qubit
  measureQubit q'

-- Quantum Secret Sharing (QSS)
quantumSecretSharing :: Bit -> Circ (Qubit, Qubit, Qubit)
quantumSecretSharing secret = do
  -- Create secret sharing state
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q3 <- createQubit Zero
  
  -- Encode secret
  q1' <- if secret == One then gateX q1 else pure q1
  
  -- Create sharing correlations
  (q1'', q2') <- cnot q1' q2
  (q1''', q3') <- cnot q1'' q3
  
  -- Add random phases
  q2'' <- hadamard q2'
  q3'' <- hadamard q3'
  
  pure (q1''', q2'', q3'')

-- Quantum coin flipping
quantumCoinFlip :: Circ Bit
quantumCoinFlip = do
  -- Alice prepares random state
  q <- createQubit Zero
  q' <- hadamard q
  
  -- Bob applies random rotation
  randomAngle <- getRandomAngle
  q'' <- rotateY randomAngle q'
  
  -- Measure final result
  measureQubit q''

getRandomAngle :: Circ Double
getRandomAngle = pure (pi / 4)  -- Simplified

-- Quantum digital signatures
quantumSignature :: [Bit] -> Circ [Qubit]
quantumSignature message = do
  -- Create signature qubits
  signatureQubits <- replicateM (length message) (createQubit Zero)
  
  -- Encode message with quantum error correction
  signedQubits <- zipWithM encodeWithSignature message signatureQubits
  
  pure signedQubits

encodeWithSignature :: Bit -> Qubit -> Circ Qubit
encodeWithSignature bit q = do
  q' <- if bit == One then gateX q else pure q
  q'' <- rotateY (pi/8) q'  -- Signature phase
  pure q''

-- Quantum money (unforgeable quantum states)
quantumMoney :: String -> Circ [Qubit]
quantumMoney serialNumber = do
  let n = length serialNumber
  qubits <- replicateM n (createQubit Zero)
  
  -- Create unforgeable quantum state based on serial number
  moneyQubits <- zipWithM createMoneyQubit serialNumber qubits
  
  pure moneyQubits

createMoneyQubit :: Char -> Qubit -> Circ Qubit
createMoneyQubit c q = do
  let angle = fromIntegral (fromEnum c) * pi / 128
  q' <- rotateY angle q
  q'' <- hadamard q'
  pure q''`,
  }
} 