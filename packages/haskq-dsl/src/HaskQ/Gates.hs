-- | HaskQ Gates: Quantum gate constructors and operations
module HaskQ.Gates
  ( -- * Basic Gates
    identity
  , hadamard
  , pauliX
  , pauliY
  , pauliZ
  
  -- * Two-Qubit Gates
  , cnot
  , cz
  , swap
  
  -- * Three-Qubit Gates
  , toffoli
  , fredkin
  
  -- * Rotation Gates
  , rx
  , ry
  , rz
  , phase
  , t
  , s
  
  -- * Compound Gates
  , universalGate
  , qft
  , inverseQft
  ) where

import HaskQ.Types

-- | Identity gate (no-op)
identity :: Qubit -> Gate
identity = Identity

-- | Hadamard gate: |0⟩ → (|0⟩ + |1⟩)/√2, |1⟩ → (|0⟩ - |1⟩)/√2
hadamard :: Qubit -> Gate
hadamard = Hadamard

-- | Pauli-X gate (bit flip): |0⟩ → |1⟩, |1⟩ → |0⟩
pauliX :: Qubit -> Gate
pauliX = PauliX

-- | Pauli-Y gate: |0⟩ → i|1⟩, |1⟩ → -i|0⟩
pauliY :: Qubit -> Gate
pauliY = PauliY

-- | Pauli-Z gate (phase flip): |0⟩ → |0⟩, |1⟩ → -|1⟩
pauliZ :: Qubit -> Gate
pauliZ = PauliZ

-- | Controlled-NOT gate
cnot :: Qubit -> Qubit -> Gate
cnot = CNOT

-- | Controlled-Z gate
cz :: Qubit -> Qubit -> Gate
cz = CZ

-- | SWAP gate (exchanges two qubits)
swap :: Qubit -> Qubit -> Circuit
swap q1 q2 = Circuit [cnot q1 q2, cnot q2 q1, cnot q1 q2]

-- | Toffoli gate (CCNOT - controlled-controlled-NOT)
toffoli :: Qubit -> Qubit -> Qubit -> Gate
toffoli = Toffoli

-- | Fredkin gate (CSWAP - controlled-SWAP)
fredkin :: Qubit -> Qubit -> Qubit -> Circuit
fredkin control q1 q2 = Circuit 
  [ cnot q2 q1
  , ControlledGate control (cnot q1 q2)
  , cnot q2 q1
  ]

-- | Rotation around X-axis
rx :: Double -> Qubit -> Gate
rx = RX

-- | Rotation around Y-axis
ry :: Double -> Qubit -> Gate
ry = RY

-- | Rotation around Z-axis
rz :: Double -> Qubit -> Gate
rz = RZ

-- | Phase gate (rotation around Z-axis)
phase :: Double -> Qubit -> Gate
phase = Phase

-- | T gate (π/4 phase rotation)
t :: Qubit -> Gate
t q = phase (pi/4) q

-- | S gate (π/2 phase rotation)
s :: Qubit -> Gate
s q = phase (pi/2) q

-- | Universal single-qubit gate (arbitrary rotation)
universalGate :: Double -> Double -> Double -> Qubit -> Circuit
universalGate alpha beta gamma q = Circuit
  [ rz alpha q
  , ry beta q
  , rz gamma q
  ]

-- | Quantum Fourier Transform for n qubits
qft :: [Qubit] -> Circuit
qft [] = Circuit []
qft (q:qs) = Circuit $
  [hadamard q] ++
  [ControlledGate qi (phase (pi / 2^k)) | (qi, k) <- zip qs [2..]] ++
  gates
  where
    Circuit gates = qft qs

-- | Inverse Quantum Fourier Transform
inverseQft :: [Qubit] -> Circuit
inverseQft qubits = Circuit $ reverse $ map invertGate gates
  where
    Circuit gates = qft (reverse qubits)
    
    invertGate :: Gate -> Gate
    invertGate (Hadamard q) = Hadamard q
    invertGate (ControlledGate c (Phase angle)) = ControlledGate c (Phase (-angle))
    invertGate (Phase angle) = Phase (-angle)
    invertGate g = g  -- Most gates are self-inverse 