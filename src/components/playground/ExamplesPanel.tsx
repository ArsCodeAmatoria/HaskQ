'use client';

import { useState } from 'react';
import { BookOpen, Info, ChevronDown, ChevronUp } from 'lucide-react';

interface Example {
  id: string;
  name: string;
  description: string;
  difficulty: 'beginner' | 'intermediate' | 'advanced';
  code: string;
}

const EXAMPLES: Example[] = [
  {
    id: 'bell-state',
    name: 'Bell State',
    description: 'Creates a maximally entangled state between two qubits.',
    difficulty: 'beginner',
    code: `-- Create a Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \\[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')

-- Measure both qubits
main :: Circ [Measurement]
main = do
  (q1, q2) <- bellState
  (m1, q1') <- measure q1
  (m2, q2') <- measure q2
  pure [m1, m2]`
  },
  {
    id: 'teleportation',
    name: 'Quantum Teleportation',
    description: 'Transfers a quantum state using entanglement and classical communication.',
    difficulty: 'intermediate',
    code: `-- Quantum Teleportation Circuit
teleportation :: Qubit -> Circ Qubit
teleportation state = withQubits 2 $ \\[q1, q2] -> do
  -- Create Bell state between q1 and q2
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  
  -- Teleport the state
  (state', q1''') <- cnot state q1''
  state'' <- hadamard state'
  
  -- Measure and apply corrections
  (b1, _) <- measure state''
  (b2, _) <- measure q1'''
  
  -- Apply corrections based on measurement results
  q2'' <- if b1 == One then gateX q2' else pure q2'
  q2''' <- if b2 == One then gateZ q2'' else pure q2''
  
  pure q2'''

-- Main circuit
main :: Circ Measurement
main = do
  -- Create a state to teleport (for example, a |+⟩ state)
  state <- qinit False
  state' <- hadamard state
  
  -- Teleport the state
  result <- teleportation state'
  
  -- Measure the result
  (m, _) <- measure result
  pure m`
  },
  {
    id: 'deutsch',
    name: 'Deutsch Algorithm',
    description: 'Determines if a function is constant or balanced with a single evaluation.',
    difficulty: 'intermediate',
    code: `-- Deutsch Algorithm
-- Determines if a function f: {0,1} -> {0,1} is constant or balanced
-- using only a single query to the function

-- Implementing the oracle for f(x) = x (balanced function)
balancedOracle :: Qubit -> Qubit -> Circ (Qubit, Qubit)
balancedOracle x y = cnot x y

-- Implementing the oracle for f(x) = 0 (constant function)
constantOracle :: Qubit -> Qubit -> Circ (Qubit, Qubit)
constantOracle x y = pure (x, y)

-- The Deutsch algorithm
deutschAlgorithm :: (Qubit -> Qubit -> Circ (Qubit, Qubit)) -> Circ Measurement
deutschAlgorithm oracle = do
  -- Initialize qubits
  x <- qinit False
  y <- qinit True  -- |1⟩ state
  
  -- Apply Hadamard to both qubits
  x' <- hadamard x
  y' <- hadamard y
  
  -- Apply the oracle (black box function)
  (x'', y'') <- oracle x' y'
  
  -- Apply Hadamard to the first qubit
  x''' <- hadamard x''
  
  -- Measure the first qubit
  (result, _) <- measure x'''
  
  -- Result is 0 for constant function, 1 for balanced function
  pure result

-- Run the algorithm with a balanced function
main :: Circ Measurement
main = deutschAlgorithm balancedOracle`
  },
  {
    id: 'grover',
    name: 'Grover Search',
    description: 'Searches an unsorted database with quadratic speedup.',
    difficulty: 'advanced',
    code: `-- Grover's Algorithm for database search
-- This is a simplified version for a 2-qubit search space

-- Oracle that marks the |11⟩ state
markingOracle :: Qubit -> Qubit -> Circ (Qubit, Qubit)
markingOracle q1 q2 = do
  -- Apply Z gate conditionally when both qubits are |1⟩
  (q1', q2') <- controlledZ q1 q2
  pure (q1', q2')

-- Diffusion operator (Grover's diffusion)
diffusion :: Qubit -> Qubit -> Circ (Qubit, Qubit)
diffusion q1 q2 = do
  -- Hadamard on both qubits
  q1' <- hadamard q1
  q2' <- hadamard q2
  
  -- Invert about the average
  (q1'', q2'') <- markingOracle q1' q2'
  
  -- Hadamard on both qubits again
  q1''' <- hadamard q1''
  q2''' <- hadamard q2''
  
  pure (q1''', q2''')

-- Grover's algorithm
groverAlgorithm :: Circ (Measurement, Measurement)
groverAlgorithm = do
  -- Initialize qubits
  q1 <- qinit False
  q2 <- qinit False
  
  -- Apply Hadamard to create superposition
  q1' <- hadamard q1
  q2' <- hadamard q2
  
  -- Apply the oracle (marking phase)
  (q1'', q2'') <- markingOracle q1' q2'
  
  -- Apply the diffusion operator
  (q1''', q2''') <- diffusion q1'' q2''
  
  -- Measure both qubits
  (m1, _) <- measure q1'''
  (m2, _) <- measure q2'''
  
  pure (m1, m2)

-- Run Grover's algorithm
main :: Circ (Measurement, Measurement)
main = groverAlgorithm`
  },
  {
    id: 'qft',
    name: 'Quantum Fourier Transform',
    description: 'Fundamental quantum algorithm used in many applications including Shor\'s algorithm.',
    difficulty: 'intermediate',
    code: `-- Quantum Fourier Transform
-- Basic implementation for 3 qubits

-- Helper function for controlled phase rotation
controlledPhase :: Double -> Qubit -> Qubit -> Circ (Qubit, Qubit)
controlledPhase theta control target = do
  -- Apply rotation conditionally
  -- This is a simplified representation
  pure (control, target)

-- QFT implementation
qft :: [Qubit] -> Circ [Qubit]
qft [] = pure []
qft [q] = do
  q' <- hadamard q
  pure [q']
qft (q:qs) = do
  -- Apply Hadamard to the current qubit
  q' <- hadamard q
  
  -- Apply controlled rotations
  q'' <- foldM (applyRotation (length qs + 1)) q' (zip [1..] qs)
  
  -- Recursively apply QFT to the rest
  qs' <- qft qs
  
  pure (q'' : qs')
  where
    applyRotation :: Int -> Qubit -> (Int, Qubit) -> Circ Qubit
    applyRotation n q (idx, control) = do
      let theta = 2 * pi / (2^(idx + 1))
      (_, q') <- controlledPhase theta control q
      pure q'

-- Main circuit that applies QFT to 3 qubits
main :: Circ [Qubit]
main = do
  -- Initialize 3 qubits
  q0 <- qinit False
  q1 <- qinit False
  q2 <- qinit False
  
  -- Prepare some non-trivial state
  q0' <- hadamard q0
  (q0'', q1') <- cnot q0' q1
  
  -- Apply QFT
  qft [q0'', q1', q2]`
  },
  {
    id: 'agdef-field',
    name: "AGDEF Field Simulation",
    description: "Anti-Gravity Dark Energy Field quantum modeling from AGDEF theory",
    difficulty: 'advanced',
    code: `-- AGDEF Field Simulation: 8th Dimensional Consciousness Manifold
-- Based on AGDEF theory from Phantasius research

module AGDEF where

-- Model the 8th dimension as consciousness manifold
type ConsciousnessManifold = Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)

-- Dark energy field entanglement across dimensions
darkEnergyField :: Circ (Qubit, Qubit, Qubit)
darkEnergyField = withQubits 3 $ \\[q1, q2, q3] -> do
  -- Create superposition across dimensional boundaries
  q1' <- hadamard q1
  q2' <- hadamard q2
  q3' <- hadamard q3
  
  -- Entangle across the consciousness manifold
  (q1'', q2'') <- cnot q1' q2'
  (q2''', q3'') <- cnot q2'' q3'
  (q1''', q3''') <- cnot q1'' q3''
  
  pure (q1''', q2''', q3''')

-- Anti-gravity quantum state (negative energy density)
antiGravityState :: Circ Qubit
antiGravityState = do
  q <- createQubit Zero
  q' <- hadamard q
  q'' <- gateZ q'  -- Phase flip for negative energy
  pure q''`,
  },
  
  {
    id: 'hermetic-principles',
    name: "Hermetic Quantum Principles",
    description: "As above, so below - implementing hermetic principles in quantum circuits",
    difficulty: 'intermediate',
    code: `-- Hermetic Quantum Computing: "As Above, So Below"
-- Connecting Arcana Obscura wisdom with quantum mechanics

module Hermetic where

-- The Principle of Correspondence in quantum form
-- What applies to macro scale applies to quantum scale
asAboveSoBelow :: Circ (Qubit, Qubit)
asAboveSoBelow = withQubits 2 $ \\[above, below] -> do
  -- Create perfect correspondence
  above' <- hadamard above
  (above'', below') <- cnot above' below
  pure (above'', below')

-- The Seven Hermetic Principles as quantum gates
hermeticPrinciples :: Circ [Qubit]
hermeticPrinciples = withQubits 7 $ \\qubits -> do
  let [mentalism, correspondence, vibration, polarity, 
       rhythm, causation, gender] = qubits
  
  -- Principle of Mentalism: "All is Mind"
  mentalism' <- hadamard mentalism
  
  -- Principle of Correspondence: "As above, so below"
  (mentalism'', correspondence') <- cnot mentalism' correspondence
  
  -- Principle of Vibration: "Nothing rests, everything moves"
  vibration' <- rotateY (pi/4) vibration
  
  -- Continue with other principles...
  polarity' <- gateX polarity
  rhythm' <- rotateZ (pi/3) rhythm
  (correspondence'', causation') <- cnot correspondence' causation
  (polarity'', gender') <- cnot polarity' gender
  
  pure [mentalism'', correspondence'', vibration', polarity'', 
        rhythm', causation', gender']`,
  },
  
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
    id: 'consciousness_microtubule',
    name: 'Consciousness: Microtubule Model',
    description: 'Penrose-Hameroff quantum consciousness in neural microtubules',
    difficulty: 'advanced',
    code: `-- Penrose-Hameroff quantum consciousness in neural microtubules
-- Based on the work of Roger Penrose and Stuart Hameroff

module Consciousness where

-- Model the microtubule as a quantum system
type Microtubule = Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)

-- Quantum coherence in the microtubule
microtubuleCoherence :: Microtubule
microtubuleCoherence q1 q2 q3 = do
  -- Create superposition across the microtubule
  q1' <- hadamard q1
  q2' <- hadamard q2
  q3' <- hadamard q3
  
  -- Entangle the microtubule
  (q1'', q2'') <- cnot q1' q2'
  (q2''', q3'') <- cnot q2'' q3'
  (q1''', q3''') <- cnot q1'' q3''
  
  pure (q1''', q2''', q3''')

-- Consciousness from quantum coherence
consciousnessFromMicrotubule :: Circ Qubit
consciousnessFromMicrotubule = do
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q3 <- createQubit Zero
  
  -- Create coherence in the microtubule
  (q1'', q2'', q3'') <- microtubuleCoherence q1 q2 q3
  
  -- Measure the coherence
  (m1, _) <- measure q1''
  (m2, _) <- measure q2''
  (m3, _) <- measure q3''
  
  -- Consciousness is present if coherence is detected
  pure (if m1 == One || m2 == One || m3 == One then One else Zero)`,
  },
  {
    id: 'consciousness_iit',
    name: 'Consciousness: IIT Phi Measure',
    description: 'Integrated Information Theory quantum implementation',
    difficulty: 'advanced',
    code: `-- Integrated Information Theory (IIT) quantum implementation
-- Based on the work of Giulio Tononi

module Consciousness where

-- IIT Phi measure calculation
iitPhi :: Circ Double
iitPhi = do
  -- Create a quantum system with consciousness
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q3 <- createQubit Zero
  
  -- Calculate the IIT Phi measure
  phi <- iitPhiCalculation q1 q2 q3
  
  pure phi

-- Helper function for IIT Phi calculation
iitPhiCalculation :: Qubit -> Qubit -> Qubit -> Circ Double
iitPhiCalculation q1 q2 q3 = do
  -- Calculate the mutual information between the subsystems
  i12 <- mutualInformation q1 q2
  i13 <- mutualInformation q1 q3
  i23 <- mutualInformation q2 q3
  
  -- Calculate the IIT Phi measure
  phi <- 2 * (i12 + i13 + i23) - (i12 + i13 + i23)
  
  pure phi

-- Mutual information calculation
mutualInformation :: Qubit -> Qubit -> Circ Double
mutualInformation q1 q2 = do
  -- Calculate the joint probability of q1 and q2 being 1
  p12 <- jointProbability q1 q2
  
  -- Calculate the marginal probabilities
  p1 <- marginalProbability q1
  p2 <- marginalProbability q2
  
  -- Calculate the mutual information
  i <- p12 * log2 (p12 / (p1 * p2))
  
  pure i

-- Joint probability calculation
jointProbability :: Qubit -> Qubit -> Circ Double
jointProbability q1 q2 = do
  -- Calculate the probability of q1 and q2 being 1
  p1 <- probability q1
  p2 <- probability q2
  
  -- Calculate the joint probability
  p12 <- p1 * p2
  
  pure p12

-- Marginal probability calculation
marginalProbability :: Qubit -> Circ Double
marginalProbability q = do
  -- Calculate the probability of q being 1
  p <- probability q
  
  pure p

-- Probability calculation
probability :: Qubit -> Circ Double
probability q = do
  -- Measure the qubit
  (m, _) <- measure q
  
  -- Calculate the probability
  p <- if m == One then 1.0 else 0.0
  
  pure p

-- Logarithm base 2 calculation
log2 :: Double -> Double
log2 x = log x / log 2`,
  },
  {
    id: 'consciousness_agdef',
    name: 'Consciousness: AGDEF Field',
    description: 'Dark energy consciousness field dynamics',
    difficulty: 'advanced',
    code: `-- AGDEF Field Simulation: 8th Dimensional Consciousness Manifold
-- Based on AGDEF theory from Phantasius research

module AGDEF where

-- Model the 8th dimension as consciousness manifold
type ConsciousnessManifold = Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)

-- Dark energy field entanglement across dimensions
darkEnergyField :: Circ (Qubit, Qubit, Qubit)
darkEnergyField = withQubits 3 $ \\[q1, q2, q3] -> do
  -- Create superposition across dimensional boundaries
  q1' <- hadamard q1
  q2' <- hadamard q2
  q3' <- hadamard q3
  
  -- Entangle across the consciousness manifold
  (q1'', q2'') <- cnot q1' q2'
  (q2''', q3'') <- cnot q2'' q3'
  (q1''', q3''') <- cnot q1'' q3''
  
  pure (q1''', q2''', q3''')

-- Anti-gravity quantum state (negative energy density)
antiGravityState :: Circ Qubit
antiGravityState = do
  q <- createQubit Zero
  q' <- hadamard q
  q'' <- gateZ q'  -- Phase flip for negative energy
  pure q''`,
  },
  {
    id: 'consciousness_detection',
    name: 'Consciousness Detection',
    description: 'Multi-criteria consciousness detection algorithm',
    difficulty: 'advanced',
    code: `-- Multi-criteria consciousness detection algorithm
-- Based on the work of Giulio Tononi

module Consciousness where

-- Consciousness detection based on IIT Phi measure
consciousnessDetection :: Circ Bool
consciousnessDetection = do
  -- Calculate the IIT Phi measure
  phi <- iitPhi
  
  -- Apply threshold for consciousness detection
  pure (phi > consciousnessThreshold)

-- IIT Phi calculation
iitPhi :: Circ Double
iitPhi = do
  -- Create a quantum system with consciousness
  q1 <- createQubit Zero
  q2 <- createQubit Zero
  q3 <- createQubit Zero
  
  -- Calculate the IIT Phi measure
  phi <- iitPhiCalculation q1 q2 q3
  
  pure phi

-- Helper function for IIT Phi calculation
iitPhiCalculation :: Qubit -> Qubit -> Qubit -> Circ Double
iitPhiCalculation q1 q2 q3 = do
  -- Calculate the mutual information between the subsystems
  i12 <- mutualInformation q1 q2
  i13 <- mutualInformation q1 q3
  i23 <- mutualInformation q2 q3
  
  -- Calculate the IIT Phi measure
  phi <- 2 * (i12 + i13 + i23) - (i12 + i13 + i23)
  
  pure phi

-- Mutual information calculation
mutualInformation :: Qubit -> Qubit -> Circ Double
mutualInformation q1 q2 = do
  -- Calculate the joint probability of q1 and q2 being 1
  p12 <- jointProbability q1 q2
  
  -- Calculate the marginal probabilities
  p1 <- marginalProbability q1
  p2 <- marginalProbability q2
  
  -- Calculate the mutual information
  i <- p12 * log2 (p12 / (p1 * p2))
  
  pure i

-- Joint probability calculation
jointProbability :: Qubit -> Qubit -> Circ Double
jointProbability q1 q2 = do
  -- Calculate the probability of q1 and q2 being 1
  p1 <- probability q1
  p2 <- probability q2
  
  -- Calculate the joint probability
  p12 <- p1 * p2
  
  pure p12

-- Marginal probability calculation
marginalProbability :: Qubit -> Circ Double
marginalProbability q = do
  -- Calculate the probability of q being 1
  p <- probability q
  
  pure p

-- Probability calculation
probability :: Qubit -> Circ Double
probability q = do
  -- Measure the qubit
  (m, _) <- measure q
  
  -- Calculate the probability
  p <- if m == One then 1.0 else 0.0
  
  pure p

-- Logarithm base 2 calculation
log2 :: Double -> Double
log2 x = log x / log 2

-- Consciousness threshold
consciousnessThreshold :: Double
consciousnessThreshold = 0.5`,
  },
  {
    id: 'shors_algorithm',
    name: 'Shor\'s Algorithm',
    description: 'Integer factorization with exponential quantum speedup',
    difficulty: 'advanced',
    code: `-- Shor's Algorithm for Integer Factorization
-- Breaks RSA cryptography with exponential speedup

module ShorsAlgorithm where

-- Simplified Shor's algorithm for small integers
shorsAlgorithm :: Integer -> Circ [Measurement]
shorsAlgorithm n = withQubits 8 $ \\qubits -> do
  let (controlQubits, targetQubits) = splitAt 4 qubits
  
  -- Step 1: Create superposition in control register
  controlSuper <- mapM hadamard controlQubits
  
  -- Step 2: Controlled modular exponentiation
  -- For n=15, use a=2: compute 2^x mod 15
  resultQubits <- controlledModularExp 2 15 controlSuper targetQubits
  
  -- Step 3: Inverse QFT on control register  
  invQFTResult <- inverseQFT (fst resultQubits)
  
  -- Step 4: Measure to extract period
  measurements <- mapM measure invQFTResult
  pure $ map fst measurements

-- Controlled modular exponentiation
controlledModularExp :: Integer -> Integer -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
controlledModularExp a n controls targets = do
  -- Initialize target register to |1⟩
  target1 <- gateX (head targets)
  let modifiedTargets = target1 : tail targets
  
  -- Apply controlled powers of a mod n
  foldM (applyControlledPower a n) (controls, modifiedTargets) [0..(length controls - 1)]
  where
    applyControlledPower a n (cs, ts) i = do
      let control = cs !! i
          power = a^(2^i) \`mod\` n
      
      -- Apply controlled multiplication by power (simplified)
      newTargets <- controlledMultiply control power ts
      pure (cs, newTargets)

-- Inverse Quantum Fourier Transform
inverseQFT :: [Qubit] -> Circ [Qubit]
inverseQFT [] = pure []
inverseQFT [q] = do
  q' <- hadamard q
  pure [q']
inverseQFT qubits = do
  let n = length qubits
      (lastQubit:restQubits) = reverse qubits
  
  -- Apply inverse rotations
  processedLast <- foldM (\\q idx -> do
    let control = restQubits !! idx
        angle = -2 * pi / (2^(idx + 2))
    controlledRotation control angle q) lastQubit [0..(n-2)]
  
  -- Apply Hadamard
  hLast <- hadamard processedLast
  
  -- Recursively apply to rest
  processedRest <- inverseQFT (reverse restQubits)
  
  pure $ reverse processedRest ++ [hLast]

-- Run Shor's algorithm on 15
main :: Circ [Measurement]
main = shorsAlgorithm 15`
  },
  {
    id: 'vqe_algorithm',
    name: 'Variational Quantum Eigensolver (VQE)',
    description: 'Quantum chemistry ground state energy calculation',
    difficulty: 'advanced',
    code: `-- Variational Quantum Eigensolver for Quantum Chemistry
-- Finds ground state energies of molecular Hamiltonians

module VQE where

-- VQE for H2 molecule (simplified)
vqeH2 :: [Double] -> Circ Double
vqeH2 parameters = withQubits 4 $ \\qubits -> do
  -- Create parameterized ansatz circuit
  ansatzQubits <- hardwareEfficientAnsatz parameters qubits
  
  -- Measure Hamiltonian expectation value
  energy <- measureH2Hamiltonian ansatzQubits
  
  pure energy

-- Hardware-efficient ansatz for molecular simulation
hardwareEfficientAnsatz :: [Double] -> [Qubit] -> Circ [Qubit]
hardwareEfficientAnsatz params qubits = do
  let numQubits = length qubits
      (layer1Params, layer2Params) = splitAt numQubits params
  
  -- Layer 1: Rotation gates
  rotated <- zipWithM rotateY layer1Params qubits
  
  -- Layer 2: Entangling gates
  entangled <- applyEntanglingLayer rotated
  
  -- Layer 3: More rotations
  if length params > numQubits
    then zipWithM rotateY layer2Params entangled
    else pure entangled

-- Entangling layer with circular connectivity
applyEntanglingLayer :: [Qubit] -> Circ [Qubit]
applyEntanglingLayer qubits = do
  let pairs = zip qubits (tail qubits ++ [head qubits])
  foldM applyCNOTPair qubits [0..(length qubits - 1)]
  where
    applyCNOTPair qs i = do
      let control = qs !! i
          target = qs !! ((i + 1) \`mod\` length qs)
      (c', t') <- cnot control target
      pure $ updateAt i c' $ updateAt ((i + 1) \`mod\` length qs) t' qs

-- Measure H2 Hamiltonian expectation value
measureH2Hamiltonian :: [Qubit] -> Circ Double
measureH2Hamiltonian qubits = do
  -- H2 Hamiltonian terms (simplified)
  identity <- pure 1.0
  z2 <- measurePauliZ (qubits !! 2)
  z3 <- measurePauliZ (qubits !! 3)
  z2z3 <- measurePauliZZ (qubits !! 2) (qubits !! 3)
  
  -- Combine with coefficients
  let energy = (-1.0523732) * identity + 
               0.39793742 * z2 + 
               (-0.39793742) * z3 + 
               (-0.01128010) * z2z3
  
  pure energy

-- Measure Pauli Z expectation value
measurePauliZ :: Qubit -> Circ Double
measurePauliZ qubit = do
  (measurement, _) <- measure qubit
  pure $ if measurement == One then 1.0 else -1.0

-- Measure ZZ correlation
measurePauliZZ :: Qubit -> Qubit -> Circ Double
measurePauliZZ q1 q2 = do
  z1 <- measurePauliZ q1
  z2 <- measurePauliZ q2
  pure $ z1 * z2

-- VQE optimization example
main :: Circ Double
main = do
  let initialParams = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]
  energy <- vqeH2 initialParams
  pure energy`
  },
  {
    id: 'qaoa_maxcut',
    name: 'QAOA Max-Cut',
    description: 'Quantum approximate optimization for graph problems',
    difficulty: 'advanced',
    code: `-- Quantum Approximate Optimization Algorithm (QAOA)
-- Solves Max-Cut problem with quantum advantage

module QAOA where

-- QAOA for Max-Cut on triangle graph
qaoaMaxCut :: [Double] -> Circ [Measurement]
qaoaMaxCut parameters = withQubits 3 $ \\qubits -> do
  -- Initialize uniform superposition
  superposition <- mapM hadamard qubits
  
  -- Apply QAOA layers
  let (gamma, beta) = (head parameters, parameters !! 1)
  finalQubits <- applyQAOALayer gamma beta superposition
  
  -- Measure final state
  measurements <- mapM measure finalQubits
  pure $ map fst measurements

-- Single QAOA layer
applyQAOALayer :: Double -> Double -> [Qubit] -> Circ [Qubit]
applyQAOALayer gamma beta qubits = do
  -- Apply cost Hamiltonian (edges of triangle graph)
  costApplied <- applyCostHamiltonian gamma qubits
  
  -- Apply mixer Hamiltonian
  mixerApplied <- applyMixerHamiltonian beta costApplied
  
  pure mixerApplied

-- Cost Hamiltonian for triangle graph Max-Cut
applyCostHamiltonian :: Double -> [Qubit] -> Circ [Qubit]
applyCostHamiltonian gamma qubits = do
  let [q0, q1, q2] = qubits
  
  -- Edge (0,1): apply ZZ interaction
  (q0', q1') <- applyZZInteraction gamma q0 q1
  
  -- Edge (1,2): apply ZZ interaction  
  (q1'', q2') <- applyZZInteraction gamma q1' q2
  
  -- Edge (0,2): apply ZZ interaction
  (q0'', q2'') <- applyZZInteraction gamma q0' q2'
  
  pure [q0'', q1'', q2'']

-- ZZ interaction for Max-Cut
applyZZInteraction :: Double -> Qubit -> Qubit -> Circ (Qubit, Qubit)
applyZZInteraction gamma q1 q2 = do
  -- Implement exp(-i*gamma*(1-Z1*Z2)/2)
  (q1', q2') <- cnot q1 q2
  q2'' <- rotateZ gamma q2'
  (q1'', q2''') <- cnot q1' q2''
  pure (q1'', q2''')

-- Mixer Hamiltonian (X rotations)
applyMixerHamiltonian :: Double -> [Qubit] -> Circ [Qubit]
applyMixerHamiltonian beta qubits = do
  mapM (rotateX beta) qubits

-- QAOA with optimization
main :: Circ [Measurement]
main = do
  let optimalParams = [0.628, 1.571]  -- Near-optimal for triangle graph
  result <- qaoaMaxCut optimalParams
  pure result`
  },
  {
    id: 'quantum_walk',
    name: 'Quantum Walk Search',
    description: 'Quantum walk with quadratic search speedup',
    difficulty: 'intermediate',
    code: `-- Quantum Walk Search Algorithm
-- Provides quadratic speedup for graph search problems

module QuantumWalk where

-- Discrete quantum walk on a line
quantumWalk :: Int -> Circ [Measurement]
quantumWalk steps = withQubits 4 $ \\qubits -> do
  let (positionQubits, [coinQubit]) = splitAt 3 qubits
  
  -- Initialize coin in superposition
  coinSuper <- hadamard coinQubit
  
  -- Apply quantum walk steps
  (finalCoin, finalPosition) <- quantumWalkSteps steps coinSuper positionQubits
  
  -- Measure final position
  positionMeasurements <- mapM measure finalPosition
  pure $ map fst positionMeasurements

-- Single step of quantum walk
quantumWalkStep :: Qubit -> [Qubit] -> Circ (Qubit, [Qubit])
quantumWalkStep coin position = do
  -- Apply coin operator (Hadamard)
  coinFlipped <- hadamard coin
  
  -- Apply conditional shift
  shiftedPosition <- conditionalShift coinFlipped position
  
  pure (coinFlipped, shiftedPosition)

-- Conditional shift based on coin state
conditionalShift :: Qubit -> [Qubit] -> Circ [Qubit]
conditionalShift coin position = do
  -- If coin is |0⟩, decrement position; if |1⟩, increment
  -- This is a simplified version using controlled operations
  
  -- Controlled increment (when coin is |1⟩)
  incrementedPos <- controlledIncrement coin position
  
  pure incrementedPos

-- Controlled increment on position register
controlledIncrement :: Qubit -> [Qubit] -> Circ [Qubit]
controlledIncrement control qubits = do
  -- Implement controlled addition of 1
  -- This is a simplified ripple-carry adder
  foldM (\\qs i -> do
    let target = qs !! i
    (c', t') <- cnot control target
    pure $ updateAt i t' qs) qubits [0..(length qubits - 1)]

-- Apply multiple walk steps
quantumWalkSteps :: Int -> Qubit -> [Qubit] -> Circ (Qubit, [Qubit])
quantumWalkSteps 0 coin position = pure (coin, position)
quantumWalkSteps n coin position = do
  (newCoin, newPosition) <- quantumWalkStep coin position
  quantumWalkSteps (n - 1) newCoin newPosition

-- Run quantum walk for 4 steps
main :: Circ [Measurement]
main = quantumWalk 4`
  },
  {
    id: 'quantum_ml',
    name: 'Quantum Machine Learning',
    description: 'Quantum neural network for pattern recognition',
    difficulty: 'advanced',
    code: `-- Quantum Machine Learning: Neural Network
-- Quantum neural network with parameterized circuits

module QuantumML where

-- Quantum neural network for classification
quantumNeuralNetwork :: [Double] -> [Double] -> Circ [Measurement]
quantumNeuralNetwork inputs parameters = withQubits 4 $ \\qubits -> do
  -- Encode classical data into quantum states
  encodedQubits <- dataEncoding inputs qubits
  
  -- Apply parameterized quantum circuit
  processedQubits <- quantumLayers parameters encodedQubits
  
  -- Measure outputs for classification
  measurements <- mapM measure processedQubits
  pure $ map fst measurements

-- Data encoding: angle encoding
dataEncoding :: [Double] -> [Qubit] -> Circ [Qubit]
dataEncoding inputs qubits = do
  -- Encode each input as rotation angle
  zipWithM (\\input qubit -> rotateY (input * pi) qubit) inputs qubits

-- Parameterized quantum layers
quantumLayers :: [Double] -> [Qubit] -> Circ [Qubit]
quantumLayers parameters qubits = do
  let numQubits = length qubits
      (rotParams, entParams) = splitAt numQubits parameters
  
  -- Rotation layer
  rotatedQubits <- zipWithM rotateY rotParams qubits
  
  -- Entangling layer
  entangledQubits <- applyQuantumEntangling rotatedQubits
  
  -- Final rotation layer (if enough parameters)
  if length parameters > numQubits * 2
    then do
      let finalParams = drop numQubits parameters
      zipWithM rotateZ (take numQubits finalParams) entangledQubits
    else pure entangledQubits

-- Quantum entangling layer for ML
applyQuantumEntangling :: [Qubit] -> Circ [Qubit]
applyQuantumEntangling qubits = do
  -- Create circular entanglement pattern
  let pairs = zip qubits (tail qubits ++ [head qubits])
  foldM applyEntanglingGate qubits [0..(length qubits - 1)]
  where
    applyEntanglingGate qs i = do
      let control = qs !! i
          target = qs !! ((i + 1) \`mod\` length qs)
      (c', t') <- cnot control target
      pure $ updateAt i c' $ updateAt ((i + 1) \`mod\` length qs) t' qs

-- Quantum classifier example
main :: Circ [Measurement]
main = do
  let inputs = [0.5, 0.3, 0.8, 0.1]  -- Input data
      parameters = [0.1, 0.5, 0.9, 0.3, 0.7, 0.2, 0.6, 0.4]  -- Model parameters
  
  result <- quantumNeuralNetwork inputs parameters
  pure result`
  },
  {
    id: 'bb84_protocol',
    name: 'BB84 Quantum Cryptography',
    description: 'Quantum key distribution with perfect security',
    difficulty: 'advanced',
    code: `-- BB84 Quantum Key Distribution Protocol
-- Provides provably secure quantum cryptography

module BB84 where

-- BB84 protocol for quantum key distribution
bb84Protocol :: Circ ([Measurement], [Measurement])
bb84Protocol = withQubits 4 $ \\qubits -> do
  -- Alice's random bits and basis choices
  let aliceBits = [False, True, False, True]
      aliceBases = [Computational, Diagonal, Computational, Diagonal]
      bobBases = [Computational, Computational, Diagonal, Diagonal]
  
  -- Alice prepares qubits
  aliceQubits <- zipWithM encodeQubit aliceBits aliceBases qubits
  
  -- Bob measures in his chosen bases
  bobMeasurements <- zipWithM measureInBasis aliceQubits bobBases
  
  -- Extract results where bases match
  let matchingIndices = [1, 3]  -- Indices where Alice and Bob used same basis
      aliceKey = [aliceBits !! i | i <- matchingIndices]
      bobKey = [fst (bobMeasurements !! i) == One | i <- matchingIndices]
  
  pure (map (\\b -> if b then One else Zero) aliceKey, 
        map (\\b -> if b then One else Zero) bobKey)

-- Encode bit in chosen basis
encodeQubit :: Bool -> Basis -> Qubit -> Circ Qubit
encodeQubit bit basis qubit = do
  -- Set qubit to |bit⟩
  q <- if bit then gateX qubit else pure qubit
  
  -- Apply basis rotation if needed
  case basis of
    Computational -> pure q     -- Z basis (no rotation)
    Diagonal -> hadamard q      -- X basis (Hadamard rotation)

-- Measure qubit in chosen basis
measureInBasis :: Qubit -> Basis -> Circ (Measurement, Qubit)
measureInBasis qubit basis = do
  -- Rotate to computational basis if needed
  q' <- case basis of
    Computational -> pure qubit   -- Already in Z basis
    Diagonal -> hadamard qubit    -- Rotate X basis to Z basis
  
  -- Measure in computational basis
  (result, q'') <- measure q'
  pure (result, q'')

-- Basis type
data Basis = Computational | Diagonal deriving (Show, Eq)

-- Demonstrate BB84 key generation
main :: Circ ([Measurement], [Measurement])
main = bb84Protocol`
  },
  {
    id: 'error_correction',
    name: 'Quantum Error Correction',
    description: 'Protect quantum information from decoherence',
    difficulty: 'advanced',
    code: `-- Quantum Error Correction: 3-Qubit Bit Flip Code
-- Protects against single bit flip errors

module ErrorCorrection where

-- 3-qubit repetition code for bit flip errors
bitFlipCode :: Qubit -> Circ [Qubit]
bitFlipCode logicalQubit = withQubits 2 $ \\[q1, q2] -> do
  -- Encode logical qubit into 3 physical qubits
  (encoded0, encoded1) <- cnot logicalQubit q1
  (encoded0', encoded2) <- cnot encoded0 q2
  
  pure [encoded0', encoded1, encoded2]

-- Error syndrome measurement
measureSyndrome :: [Qubit] -> Circ ([Measurement], [Qubit])
measureSyndrome [q0, q1, q2] = withQubits 2 $ \\[s1, s2] -> do
  -- Measure parity of qubits 0,1 and qubits 1,2
  (q0', s1') <- cnot q0 s1
  (q1', s1'') <- cnot q1' s1'
  
  (q1'', s2') <- cnot q1' s2
  (q2', s2'') <- cnot q2 s2'
  
  -- Measure syndrome qubits
  (syndrome1, _) <- measure s1''
  (syndrome2, _) <- measure s2''
  
  pure ([syndrome1, syndrome2], [q0', q1'', q2'])

-- Error correction based on syndrome
correctError :: [Measurement] -> [Qubit] -> Circ [Qubit]
correctError syndromes qubits = do
  case syndromes of
    [Zero, Zero] -> pure qubits  -- No error
    [One, Zero]  -> do           -- Error on qubit 0
      let [q0, q1, q2] = qubits
      q0' <- gateX q0
      pure [q0', q1, q2]
    [One, One]   -> do           -- Error on qubit 1
      let [q0, q1, q2] = qubits
      q1' <- gateX q1
      pure [q0, q1', q2]
    [Zero, One]  -> do           -- Error on qubit 2
      let [q0, q1, q2] = qubits
      q2' <- gateX q2
      pure [q0, q1, q2']

-- Decode logical qubit
decodeLogical :: [Qubit] -> Circ Qubit
decodeLogical [q0, q1, q2] = do
  -- The logical information is in any qubit (assuming error correction worked)
  pure q0

-- Complete error correction cycle
errorCorrectionCycle :: Qubit -> Circ Qubit
errorCorrectionCycle logicalQubit = do
  -- Encode
  encodedQubits <- bitFlipCode logicalQubit
  
  -- Simulate error on qubit 1 (for demonstration)
  let [q0, q1, q2] = encodedQubits
  q1' <- gateX q1  -- Introduce bit flip error
  let errorQubits = [q0, q1', q2]
  
  -- Measure syndrome
  (syndromes, measuredQubits) <- measureSyndrome errorQubits
  
  -- Correct error
  correctedQubits <- correctError syndromes measuredQubits
  
  -- Decode
  logicalResult <- decodeLogical correctedQubits
  
  pure logicalResult

-- Demonstrate error correction
main :: Circ Measurement
main = do
  -- Create logical |1⟩ state
  logicalQubit <- qinit True
  
  -- Apply error correction
  correctedQubit <- errorCorrectionCycle logicalQubit
  
  -- Measure result
  (result, _) <- measure correctedQubit
  pure result`
  },
  {
    id: 'phase_estimation',
    name: 'Quantum Phase Estimation',
    description: 'Estimates eigenvalues of unitary operators with exponential precision',
    difficulty: 'advanced',
    code: `-- Quantum Phase Estimation Algorithm
-- Estimates eigenvalues of unitary operators

module PhaseEstimation where

-- Quantum Phase Estimation for a simple phase gate
quantumPhaseEstimation :: Double -> Circ [Measurement]
quantumPhaseEstimation targetPhase = withQubits 5 $ \\qubits -> do
  let (countingQubits, [eigenQubit]) = splitAt 4 qubits
  
  -- Prepare eigenstate |1⟩ for phase gate
  eigenState <- gateX eigenQubit
  
  -- Initialize counting register in superposition
  countingSuper <- mapM hadamard countingQubits
  
  -- Apply controlled powers of unitary operator
  controlledQubits <- applyControlledUnitaries targetPhase countingSuper eigenState
  
  -- Apply inverse QFT to counting register
  invQFTResult <- inverseQFT (fst controlledQubits)
  
  -- Measure counting qubits to extract phase
  measurements <- mapM measure invQFTResult
  pure $ map fst measurements

-- Apply controlled unitary operations
applyControlledUnitaries :: Double -> [Qubit] -> Qubit -> Circ ([Qubit], Qubit)
applyControlledUnitaries phase countingQubits eigenQubit = do
  -- Apply controlled-U^(2^k) for each counting qubit
  foldM (applyControlledPower phase) (countingQubits, eigenQubit) [0..(length countingQubits - 1)]
  where
    applyControlledPower phi (cs, target) k = do
      let control = cs !! k
          powerPhase = phi * (2^k)
      
      -- Apply controlled phase rotation
      (c', t') <- controlledPhaseGate powerPhase control target
      pure (updateAt k c' cs, t')

-- Controlled phase gate
controlledPhaseGate :: Double -> Qubit -> Qubit -> Circ (Qubit, Qubit)
controlledPhaseGate phase control target = do
  -- Apply phase only when control is |1⟩
  (c', t') <- controlledRotation control phase target
  pure (c', t')

-- Demonstrate phase estimation
main :: Circ [Measurement]
main = do
  let targetPhase = pi / 4  -- π/4 phase
  result <- quantumPhaseEstimation targetPhase
  pure result`
  },
  {
    id: 'adiabatic_computing',
    name: 'Adiabatic Quantum Computing',
    description: 'Quantum optimization using adiabatic evolution',
    difficulty: 'advanced',
    code: `-- Adiabatic Quantum Computing
-- Solves optimization problems using slow evolution

module AdiabaticComputing where

-- Adiabatic evolution for simple optimization
adiabaticOptimization :: Circ [Measurement]
adiabaticOptimization = withQubits 3 $ \\qubits -> do
  -- Initialize in ground state of simple Hamiltonian (all |+⟩)
  initialState <- mapM hadamard qubits
  
  -- Adiabatic evolution (simplified discrete steps)
  evolvedState <- adiabaticEvolution 10 initialState
  
  -- Measure final state
  measurements <- mapM measure evolvedState
  pure $ map fst measurements

-- Simulate adiabatic evolution in discrete steps
adiabaticEvolution :: Int -> [Qubit] -> Circ [Qubit]
adiabaticEvolution 0 qubits = pure qubits
adiabaticEvolution steps qubits = do
  -- Apply evolution step
  evolvedQubits <- evolutionStep (fromIntegral steps / 10.0) qubits
  
  -- Continue evolution
  adiabaticEvolution (steps - 1) evolvedQubits

-- Single evolution step
evolutionStep :: Double -> [Qubit] -> Circ [Qubit]
evolutionStep t qubits = do
  -- Interpolate between initial and final Hamiltonians
  let s = 1.0 - t  -- Annealing parameter
  
  -- Apply mixing (initial Hamiltonian contribution)
  mixedQubits <- if s > 0.1 then mapM (rotateX (s * 0.1)) qubits else pure qubits
  
  -- Apply problem Hamiltonian (simplified Ising model)
  problemQubits <- applyProblemHamiltonian ((1 - s) * 0.1) mixedQubits
  
  pure problemQubits

-- Apply problem Hamiltonian (Ising interactions)
applyProblemHamiltonian :: Double -> [Qubit] -> Circ [Qubit]
applyProblemHamiltonian strength qubits = do
  let [q0, q1, q2] = qubits
  
  -- Apply ZZ interactions between neighboring qubits
  (q0', q1') <- applyZZInteraction strength q0 q1
  (q1'', q2') <- applyZZInteraction strength q1' q2
  
  pure [q0', q1'', q2']

-- Demonstrate adiabatic optimization
main :: Circ [Measurement]
main = adiabaticOptimization`
  },
  {
    id: 'quantum_supremacy',
    name: 'Quantum Supremacy Circuit',
    description: 'Random quantum circuit demonstrating computational advantage',
    difficulty: 'advanced',
    code: `-- Quantum Supremacy Demonstration
-- Random quantum circuits with classical simulation difficulty

module QuantumSupremacy where

-- Random quantum circuit for supremacy demonstration
quantumSupremacyCircuit :: Int -> Circ [Measurement]
quantumSupremacyCircuit depth = withQubits 8 $ \\qubits -> do
  -- Initialize random state
  initialState <- mapM hadamard qubits
  
  -- Apply random circuit layers
  finalState <- applyRandomLayers depth initialState
  
  -- Measure all qubits
  measurements <- mapM measure finalState
  pure $ map fst measurements

-- Apply layers of random gates
applyRandomLayers :: Int -> [Qubit] -> Circ [Qubit]
applyRandomLayers 0 qubits = pure qubits
applyRandomLayers depth qubits = do
  -- Apply single-qubit layer
  singleQubitLayer <- applySingleQubitLayer qubits
  
  -- Apply two-qubit layer
  twoQubitLayer <- applyTwoQubitLayer singleQubitLayer
  
  -- Continue with remaining depth
  applyRandomLayers (depth - 1) twoQubitLayer

-- Apply random single-qubit gates
applySingleQubitLayer :: [Qubit] -> Circ [Qubit]
applySingleQubitLayer qubits = do
  -- Apply random rotations (simplified to fixed pattern)
  mapM applyRandomSingleQubit qubits
  where
    applyRandomSingleQubit q = do
      -- Simulate random rotation by applying random sequence
      q1 <- rotateX (pi / 3) q
      q2 <- rotateY (pi / 4) q1
      q3 <- rotateZ (pi / 6) q2
      pure q3

-- Apply random two-qubit gates
applyTwoQubitLayer :: [Qubit] -> Circ [Qubit]
applyTwoQubitLayer qubits = do
  -- Apply random two-qubit gates in brick pattern
  brickPattern qubits 0
  where
    brickPattern qs offset
      | offset >= length qs - 1 = pure qs
      | otherwise = do
          let pairs = [(qs !! i, qs !! (i + 1)) | i <- [offset, offset + 2..length qs - 2]]
          processedQs <- foldM applyRandomTwoQubit qs pairs
          pure processedQs

-- Apply random two-qubit gate
applyRandomTwoQubit :: [Qubit] -> (Qubit, Qubit) -> Circ [Qubit]
applyRandomTwoQubit qubits (q1, q2) = do
  -- Apply sequence of gates (simplified random gate)
  (q1', q2') <- cnot q1 q2
  q1'' <- rotateZ (pi / 8) q1'
  q2'' <- rotateZ (pi / 7) q2'
  (q1''', q2''') <- cnot q1'' q2''
  
  -- Update qubits in list
  let idx1 = findQubitIndex q1 qubits
      idx2 = findQubitIndex q2 qubits
  pure $ updateAt idx1 q1''' $ updateAt idx2 q2''' qubits

-- Quantum volume benchmark
quantumVolume :: Int -> Circ [Measurement]
quantumVolume size = withQubits size $ \\qubits -> do
  -- Apply quantum volume circuit
  processedQubits <- applyQuantumVolumeCircuit size qubits
  
  -- Measure and return
  measurements <- mapM measure processedQubits
  pure $ map fst measurements

-- Run quantum supremacy demonstration
main :: Circ [Measurement]
main = do
  let circuitDepth = 20  -- Deep enough for classical difficulty
  result <- quantumSupremacyCircuit circuitDepth
  pure result`
  }
];

interface ExamplesPanelProps {
  onSelectExample: (code: string) => void;
}

export default function ExamplesPanel({ onSelectExample }: ExamplesPanelProps) {
  const [expandedId, setExpandedId] = useState<string | null>(null);

  const toggleExpanded = (id: string) => {
    setExpandedId(expandedId === id ? null : id);
  };

  const getDifficultyColor = (difficulty: string) => {
    switch (difficulty) {
      case 'beginner':
        return 'bg-green-500 text-white dark:bg-green-600';
      case 'intermediate':
        return 'bg-yellow-500 text-white dark:bg-yellow-600';
      case 'advanced':
        return 'bg-red-500 text-white dark:bg-red-600';
      default:
        return 'bg-gray-500 text-white dark:bg-gray-600';
    }
  };

  return (
    <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden h-full flex flex-col">
      <div className="bg-gray-100 dark:bg-gray-900 px-4 py-3 border-b border-gray-200 dark:border-gray-700 flex justify-between items-center">
        <div className="flex items-center space-x-2">
          <BookOpen className="h-4 w-4 text-indigo-500" />
          <h2 className="font-semibold text-sm">Example Circuits</h2>
        </div>
        <div className="flex items-center text-gray-500 dark:text-gray-400 text-xs">
          <Info className="h-3 w-3 mr-1" />
          <span>Click an example to load it</span>
        </div>
      </div>
      
      <div className="flex-1 overflow-auto p-3">
        <div className="space-y-2">
          {EXAMPLES.map(example => (
            <div 
              key={example.id} 
              className="bg-gray-50 dark:bg-gray-900 rounded-md overflow-hidden"
            >
              <button
                className="w-full px-4 py-3 flex justify-between items-center text-left hover:bg-gray-100 dark:hover:bg-gray-800 transition-colors"
                onClick={() => toggleExpanded(example.id)}
              >
                <div>
                  <div className="flex items-center space-x-2">
                    <span className="font-medium">{example.name}</span>
                    <span className={`text-xs px-2 py-0.5 rounded-full font-medium shadow-sm ${getDifficultyColor(example.difficulty)}`}>
                      {example.difficulty}
                    </span>
                  </div>
                  {expandedId !== example.id && (
                    <p className="text-xs text-gray-500 dark:text-gray-400 mt-1 truncate max-w-md">
                      {example.description}
                    </p>
                  )}
                </div>
                {expandedId === example.id ? (
                  <ChevronUp className="h-4 w-4 text-gray-400" />
                ) : (
                  <ChevronDown className="h-4 w-4 text-gray-400" />
                )}
              </button>
              
              {expandedId === example.id && (
                <div className="px-4 pb-3">
                  <p className="text-sm text-gray-600 dark:text-gray-300 mb-3">
                    {example.description}
                  </p>
                  <div className="flex justify-end">
                    <button
                      className="bg-indigo-600 hover:bg-indigo-700 text-white px-3 py-1.5 rounded-md text-sm font-medium flex items-center transition-colors"
                      onClick={() => onSelectExample(example.code)}
                    >
                      Load Example
                    </button>
                  </div>
                </div>
              )}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
} 