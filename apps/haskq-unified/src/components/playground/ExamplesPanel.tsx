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