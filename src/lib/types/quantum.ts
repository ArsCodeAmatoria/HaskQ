// Quantum Types for HaskQ
// TypeScript definitions matching the Haskell DSL and Rust WASM interfaces

export interface Qubit {
  qubit: number
}

export interface ComplexNumber {
  real: number
  imag: number
}

// Gate types matching the Rust WASM interface
export type Gate = 
  | { type: 'I'; target: Qubit }
  | { type: 'H'; target: Qubit }
  | { type: 'X'; target: Qubit }
  | { type: 'Y'; target: Qubit }
  | { type: 'Z'; target: Qubit }
  | { type: 'CNOT'; control: Qubit; target: Qubit }
  | { type: 'CZ'; control: Qubit; target: Qubit }
  | { type: 'Toffoli'; control1: Qubit; control2: Qubit; target: Qubit }
  | { type: 'RX'; angle: number; target: Qubit }
  | { type: 'RY'; angle: number; target: Qubit }
  | { type: 'RZ'; angle: number; target: Qubit }
  | { type: 'Phase'; angle: number; target: Qubit }
  | { type: 'Controlled'; control: Qubit; gate: Gate }
  | { type: 'Measure'; target: Qubit }

export interface Circuit {
  gates: Gate[]
}

export interface SimulationResult {
  amplitudes: ComplexNumber[]
  probabilities: number[]
  num_qubits: number
  measurement_results?: boolean[] | null
}

export interface MeasurementResult {
  qubit: Qubit
  outcome: boolean
  probability: number
}

export interface QuantumState {
  amplitudes: ComplexNumber[]
  numQubits: number
}

// Helper types for circuit construction
export type QubitState = 'zero' | 'one' | 'plus' | 'minus' | 'superposition'

export interface QubitStateDefinition {
  state: QubitState
  alpha?: number
  beta?: number
}

// Algorithm-specific types
export interface AlgorithmConfig {
  name: string
  qubits: number
  parameters?: Record<string, any>
}

export interface BellStateConfig extends AlgorithmConfig {
  name: 'bell'
  qubits: 2
}

export interface GHZStateConfig extends AlgorithmConfig {
  name: 'ghz'
  qubits: number
}

export interface GroverConfig extends AlgorithmConfig {
  name: 'grover'
  qubits: number
  parameters: {
    marked: number[]
    iterations?: number
  }
}

export interface DeutschConfig extends AlgorithmConfig {
  name: 'deutsch'
  qubits: 2
  parameters: {
    oracle: 'constant' | 'balanced'
  }
}

export type AlgorithmType = BellStateConfig | GHZStateConfig | GroverConfig | DeutschConfig

// Circuit builder helper functions
export const createQubit = (id: number): Qubit => ({ qubit: id })

export const createGate = {
  identity: (target: Qubit): Gate => ({ type: 'I', target }),
  hadamard: (target: Qubit): Gate => ({ type: 'H', target }),
  pauliX: (target: Qubit): Gate => ({ type: 'X', target }),
  pauliY: (target: Qubit): Gate => ({ type: 'Y', target }),
  pauliZ: (target: Qubit): Gate => ({ type: 'Z', target }),
  cnot: (control: Qubit, target: Qubit): Gate => ({ type: 'CNOT', control, target }),
  cz: (control: Qubit, target: Qubit): Gate => ({ type: 'CZ', control, target }),
  toffoli: (control1: Qubit, control2: Qubit, target: Qubit): Gate => 
    ({ type: 'Toffoli', control1, control2, target }),
  rx: (angle: number, target: Qubit): Gate => ({ type: 'RX', angle, target }),
  ry: (angle: number, target: Qubit): Gate => ({ type: 'RY', angle, target }),
  rz: (angle: number, target: Qubit): Gate => ({ type: 'RZ', angle, target }),
  phase: (angle: number, target: Qubit): Gate => ({ type: 'Phase', angle, target }),
  controlled: (control: Qubit, gate: Gate): Gate => ({ type: 'Controlled', control, gate }),
  measure: (target: Qubit): Gate => ({ type: 'Measure', target })
}

export const createCircuit = (gates: Gate[]): Circuit => ({ gates })

// Predefined quantum circuits
export const circuits = {
  bellState: (): Circuit => {
    const q0 = createQubit(0)
    const q1 = createQubit(1)
    return createCircuit([
      createGate.hadamard(q0),
      createGate.cnot(q0, q1)
    ])
  },

  ghzState: (numQubits: number): Circuit => {
    const qubits = Array.from({ length: numQubits }, (_, i) => createQubit(i))
    const gates: Gate[] = [createGate.hadamard(qubits[0])]
    
    for (let i = 1; i < numQubits; i++) {
      gates.push(createGate.cnot(qubits[0], qubits[i]))
    }
    
    return createCircuit(gates)
  },

  deutschAlgorithm: (oracle: 'constant' | 'balanced'): Circuit => {
    const x = createQubit(0)
    const y = createQubit(1)
    
    const gates: Gate[] = [
      createGate.pauliX(y),
      createGate.hadamard(x),
      createGate.hadamard(y)
    ]
    
    // Oracle implementation
    if (oracle === 'balanced') {
      gates.push(createGate.cnot(x, y))
    }
    // For constant oracle, do nothing (identity)
    
    gates.push(createGate.hadamard(x))
    
    return createCircuit(gates)
  },

  groverIteration: (numQubits: number, marked: number[]): Circuit => {
    const qubits = Array.from({ length: numQubits }, (_, i) => createQubit(i))
    const gates: Gate[] = []
    
    // Initialize superposition
    qubits.forEach(q => gates.push(createGate.hadamard(q)))
    
    // Oracle (mark target states)
    marked.forEach(target => {
      if (target < numQubits) {
        gates.push(createGate.phase(Math.PI, qubits[target]))
      }
    })
    
    // Diffuser
    qubits.forEach(q => gates.push(createGate.hadamard(q)))
    qubits.forEach(q => gates.push(createGate.pauliX(q)))
    
    // Multi-controlled Z (simplified for 2-3 qubits)
    if (numQubits === 2) {
      gates.push(createGate.cz(qubits[0], qubits[1]))
    } else if (numQubits === 3) {
      gates.push(createGate.toffoli(qubits[0], qubits[1], qubits[2]))
    }
    
    qubits.forEach(q => gates.push(createGate.pauliX(q)))
    qubits.forEach(q => gates.push(createGate.hadamard(q)))
    
    return createCircuit(gates)
  }
} 