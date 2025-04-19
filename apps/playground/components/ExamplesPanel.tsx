'use client'

import { useState } from 'react'
import { BookOpen, Info, ChevronDown, ChevronUp } from 'lucide-react'

interface Example {
  id: string
  name: string
  description: string
  difficulty: 'beginner' | 'intermediate' | 'advanced'
  code: string
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
    id: 'shors-algorithm',
    name: 'Shor\'s Algorithm',
    description: 'Exponential speedup for integer factorization, threatening classical cryptography.',
    difficulty: 'advanced',
    code: `-- Shor's Algorithm for period finding
-- This is a simplified demonstration version

-- Quantum Fourier Transform (QFT)
qft :: [Qubit] -> Circ [Qubit]
qft [] = pure []
qft (q:qs) = do
  -- Apply Hadamard to the first qubit
  q' <- hadamard q
  
  -- Apply controlled rotations
  q'' <- applyControlledRotations q' qs 2
  
  -- Recursively apply QFT to the rest
  qs' <- qft qs
  
  -- Return the transformed qubits
  pure (q'' : qs')
  where
    applyControlledRotations :: Qubit -> [Qubit] -> Int -> Circ Qubit
    applyControlledRotations q [] _ = pure q
    applyControlledRotations q (c:cs) n = do
      -- Apply controlled rotation
      q' <- controlledPhaseShift c q (2*pi / (2^n))
      -- Continue with remaining qubits
      applyControlledRotations q' cs (n+1)

-- Inverse QFT
iqft :: [Qubit] -> Circ [Qubit]
iqft qs = do
  -- Reverse the qubits
  let rqs = reverse qs
  -- Apply QFT
  rqs' <- qft rqs
  -- Reverse back
  pure (reverse rqs')

-- Modular exponentiation function (a^x mod N)
-- Simplified version for demonstration
modExp :: Int -> [Qubit] -> [Qubit] -> Int -> Circ ([Qubit], [Qubit])
modExp a x output n = do
  -- For each qubit in x
  (x', output') <- applyModExp x output 0
  pure (x', output')
  where
    applyModExp :: [Qubit] -> [Qubit] -> Int -> Circ ([Qubit], [Qubit])
    applyModExp [] out _ = pure ([], out)
    applyModExp (q:qs) out i = do
      -- If qubit is |1⟩, multiply output by a^(2^i) mod N
      (q', out') <- controlledModMul q out (modPow a (2^i) n)
      -- Continue with remaining qubits
      (qs', out'') <- applyModExp qs out' (i+1)
      pure (q':qs', out'')

-- Controlled modular multiplication
-- Simplified for demonstration
controlledModMul :: Qubit -> [Qubit] -> Int -> Circ (Qubit, [Qubit])
controlledModMul control output factor = do
  -- Apply quantum multiplication controlled by control qubit
  -- This is a simplified version
  pure (control, output)

-- Helper function: a^b mod n
modPow :: Int -> Int -> Int -> Int
modPow a b n = (a^b) \`mod\` n

-- Shor's algorithm for factoring N=15
shorsAlgorithm :: Circ [Measurement]
shorsAlgorithm = do
  -- For factoring N=15, we use a=7 (coprime to N)
  
  -- Initialize register x with 4 qubits in superposition
  xRegister <- withQubits 4 $ \\\\qs -> do
    -- Apply Hadamard to all qubits
    qs' <- mapM hadamard qs
    pure qs'
  
  -- Initialize output register with |1⟩
  outputRegister <- withQubits 4 $ \\\\qs -> do
    -- Set to |1⟩ (only first qubit is |1⟩, rest are |0⟩)
    first <- gateX (head qs)
    pure (first : tail qs)
  
  -- Apply modular exponentiation
  (xRegister', _) <- modExp 7 xRegister outputRegister 15
  
  -- Apply inverse QFT to the x register
  xRegister'' <- iqft xRegister'
  
  -- Measure all qubits in x register
  measurements <- mapM measureQubit xRegister''
  
  pure measurements
  where
    measureQubit :: Qubit -> Circ Measurement
    measureQubit q = do
      (m, _) <- measure q
      pure m

-- Main circuit
main :: Circ [Measurement]
main = shorsAlgorithm`
  }
]

interface ExamplesPanelProps {
  onSelectExample: (code: string) => void
}

export default function ExamplesPanel({ onSelectExample }: ExamplesPanelProps) {
  const [expandedId, setExpandedId] = useState<string | null>(null)

  const toggleExpanded = (id: string) => {
    setExpandedId(expandedId === id ? null : id)
  }

  return (
    <div style={{ 
      height: '100%', 
      display: 'flex', 
      flexDirection: 'column', 
      backgroundColor: '#2a2a2a',
      overflow: 'hidden'
    }} id="examples">
      <div style={{ 
        backgroundColor: '#1a1a1a', 
        padding: '8px 16px',
        borderBottom: '1px solid #333',
        fontSize: '14px',
        fontWeight: '500',
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center'
      }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <BookOpen style={{ height: '16px', width: '16px', color: '#0087ff' }} />
          <span>Example Circuits</span>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', color: '#999', fontSize: '12px' }}>
          <Info style={{ height: '12px', width: '12px', marginRight: '4px' }} />
          <span>Click an example to load it</span>
        </div>
      </div>
      <div style={{ flex: 1, overflowY: 'auto', padding: '8px' }}>
        <div style={{ display: 'grid', gridTemplateColumns: '1fr', gap: '8px' }}>
          {EXAMPLES.map(example => (
            <div 
              key={example.id} 
              style={{ backgroundColor: '#1a1a1a', borderRadius: '6px', overflow: 'hidden' }}
            >
              <button
                style={{
                  width: '100%',
                  padding: '12px 16px',
                  display: 'flex',
                  justifyContent: 'space-between',
                  alignItems: 'center',
                  backgroundColor: 'transparent',
                  border: 'none',
                  color: 'white',
                  textAlign: 'left',
                  cursor: 'pointer'
                }}
                onClick={() => toggleExpanded(example.id)}
              >
                <div>
                  <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                    <span style={{ fontWeight: '500' }}>{example.name}</span>
                    <span style={{ 
                      fontSize: '12px', 
                      padding: '2px 8px', 
                      borderRadius: '9999px',
                      backgroundColor: example.difficulty === 'beginner' 
                        ? 'rgba(34, 197, 94, 0.2)' 
                        : example.difficulty === 'intermediate'
                          ? 'rgba(234, 179, 8, 0.2)'
                          : 'rgba(239, 68, 68, 0.2)',
                      color: example.difficulty === 'beginner' 
                        ? '#4ade80' 
                        : example.difficulty === 'intermediate'
                          ? '#facc15'
                          : '#f87171'
                    }}>
                      {example.difficulty}
                    </span>
                  </div>
                  {expandedId !== example.id && (
                    <p style={{ 
                      fontSize: '12px', 
                      color: '#999', 
                      marginTop: '4px',
                      overflow: 'hidden',
                      textOverflow: 'ellipsis',
                      whiteSpace: 'nowrap'
                    }}>
                      {example.description}
                    </p>
                  )}
                </div>
                {expandedId === example.id ? (
                  <ChevronUp style={{ height: '16px', width: '16px', color: '#999' }} />
                ) : (
                  <ChevronDown style={{ height: '16px', width: '16px', color: '#999' }} />
                )}
              </button>
              
              {expandedId === example.id && (
                <div style={{ padding: '0 16px 12px 16px' }}>
                  <p style={{ fontSize: '14px', color: '#999', marginBottom: '12px' }}>
                    {example.description}
                  </p>
                  <div style={{ display: 'flex', justifyContent: 'flex-end' }}>
                    <button
                      style={{
                        backgroundColor: '#0087ff',
                        color: 'white',
                        border: 'none',
                        borderRadius: '6px',
                        padding: '6px 12px',
                        fontSize: '12px',
                        cursor: 'pointer',
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'center',
                        gap: '8px'
                      }}
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
  )
} 