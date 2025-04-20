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
        return 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400';
      case 'intermediate':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400';
      case 'advanced':
        return 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400';
      default:
        return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-400';
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
                    <span className={`text-xs px-2 py-0.5 rounded-full ${getDifficultyColor(example.difficulty)}`}>
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
                      className="bg-indigo-600 hover:bg-indigo-700 text-white text-xs px-3 py-1.5 rounded flex items-center transition-colors"
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