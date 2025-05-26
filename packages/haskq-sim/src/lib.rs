use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};
use nalgebra::{DMatrix, DVector};
use num_complex::Complex64;
use std::collections::HashMap;

#[cfg(feature = "console_error_panic_hook")]
pub use console_error_panic_hook::set_once as set_panic_hook;

// Import console.log for debugging
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

// Quantum types matching Haskell DSL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Qubit {
    pub qubit: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Gate {
    I { target: Qubit },
    H { target: Qubit },
    X { target: Qubit },
    Y { target: Qubit },
    Z { target: Qubit },
    CNOT { control: Qubit, target: Qubit },
    CZ { control: Qubit, target: Qubit },
    Toffoli { control1: Qubit, control2: Qubit, target: Qubit },
    RX { angle: f64, target: Qubit },
    RY { angle: f64, target: Qubit },
    RZ { angle: f64, target: Qubit },
    Phase { angle: f64, target: Qubit },
    Controlled { control: Qubit, gate: Box<Gate> },
    Measure { target: Qubit },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Circuit {
    pub gates: Vec<Gate>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulationResult {
    pub amplitudes: Vec<ComplexNumber>,
    pub probabilities: Vec<f64>,
    pub num_qubits: usize,
    pub measurement_results: Option<Vec<bool>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplexNumber {
    pub real: f64,
    pub imag: f64,
}

impl From<Complex64> for ComplexNumber {
    fn from(c: Complex64) -> Self {
        ComplexNumber {
            real: c.re,
            imag: c.im,
        }
    }
}

impl Into<Complex64> for ComplexNumber {
    fn into(self) -> Complex64 {
        Complex64::new(self.real, self.imag)
    }
}

pub struct QuantumSimulator {
    num_qubits: usize,
    state: DVector<Complex64>,
}

impl QuantumSimulator {
    pub fn new(num_qubits: usize) -> Self {
        let size = 1 << num_qubits;
        let mut state = DVector::zeros(size);
        state[0] = Complex64::new(1.0, 0.0); // |000...0⟩
        
        QuantumSimulator {
            num_qubits,
            state,
        }
    }

    pub fn apply_gate(&mut self, gate: &Gate) {
        match gate {
            Gate::I { .. } => {
                // Identity gate - do nothing
            },
            Gate::H { target } => {
                self.apply_hadamard(target.qubit);
            },
            Gate::X { target } => {
                self.apply_pauli_x(target.qubit);
            },
            Gate::Y { target } => {
                self.apply_pauli_y(target.qubit);
            },
            Gate::Z { target } => {
                self.apply_pauli_z(target.qubit);
            },
            Gate::CNOT { control, target } => {
                self.apply_cnot(control.qubit, target.qubit);
            },
            Gate::CZ { control, target } => {
                self.apply_cz(control.qubit, target.qubit);
            },
            Gate::RX { angle, target } => {
                self.apply_rx(*angle, target.qubit);
            },
            Gate::RY { angle, target } => {
                self.apply_ry(*angle, target.qubit);
            },
            Gate::RZ { angle, target } => {
                self.apply_rz(*angle, target.qubit);
            },
            Gate::Phase { angle, target } => {
                self.apply_phase(*angle, target.qubit);
            },
            Gate::Toffoli { control1, control2, target } => {
                self.apply_toffoli(control1.qubit, control2.qubit, target.qubit);
            },
            Gate::Controlled { control, gate } => {
                self.apply_controlled_gate(control.qubit, gate);
            },
            Gate::Measure { target } => {
                // Measurement handled separately
                console_log!("Measurement gate encountered for qubit {}", target.qubit);
            },
        }
    }

    fn apply_hadamard(&mut self, target: usize) {
        let inv_sqrt2 = 1.0 / 2.0_f64.sqrt();
        self.apply_single_qubit_gate(target, |state| {
            let new_state = [
                [Complex64::new(inv_sqrt2, 0.0), Complex64::new(inv_sqrt2, 0.0)],
                [Complex64::new(inv_sqrt2, 0.0), Complex64::new(-inv_sqrt2, 0.0)],
            ];
            new_state
        });
    }

    fn apply_pauli_x(&mut self, target: usize) {
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(0.0, 0.0), Complex64::new(1.0, 0.0)],
                [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
            ]
        });
    }

    fn apply_pauli_y(&mut self, target: usize) {
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(0.0, 0.0), Complex64::new(0.0, -1.0)],
                [Complex64::new(0.0, 1.0), Complex64::new(0.0, 0.0)],
            ]
        });
    }

    fn apply_pauli_z(&mut self, target: usize) {
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
                [Complex64::new(0.0, 0.0), Complex64::new(-1.0, 0.0)],
            ]
        });
    }

    fn apply_rx(&mut self, angle: f64, target: usize) {
        let cos_half = (angle / 2.0).cos();
        let sin_half = (angle / 2.0).sin();
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(cos_half, 0.0), Complex64::new(0.0, -sin_half)],
                [Complex64::new(0.0, -sin_half), Complex64::new(cos_half, 0.0)],
            ]
        });
    }

    fn apply_ry(&mut self, angle: f64, target: usize) {
        let cos_half = (angle / 2.0).cos();
        let sin_half = (angle / 2.0).sin();
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(cos_half, 0.0), Complex64::new(-sin_half, 0.0)],
                [Complex64::new(sin_half, 0.0), Complex64::new(cos_half, 0.0)],
            ]
        });
    }

    fn apply_rz(&mut self, angle: f64, target: usize) {
        let exp_neg = Complex64::new(0.0, -angle / 2.0).exp();
        let exp_pos = Complex64::new(0.0, angle / 2.0).exp();
        self.apply_single_qubit_gate(target, |_| {
            [
                [exp_neg, Complex64::new(0.0, 0.0)],
                [Complex64::new(0.0, 0.0), exp_pos],
            ]
        });
    }

    fn apply_phase(&mut self, angle: f64, target: usize) {
        let phase = Complex64::new(0.0, angle).exp();
        self.apply_single_qubit_gate(target, |_| {
            [
                [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
                [Complex64::new(0.0, 0.0), phase],
            ]
        });
    }

    fn apply_cnot(&mut self, control: usize, target: usize) {
        let size = self.state.len();
        let mut new_state = self.state.clone();
        
        for i in 0..size {
            let control_bit = (i >> control) & 1;
            if control_bit == 1 {
                let target_bit = (i >> target) & 1;
                let new_target_bit = 1 - target_bit;
                let j = (i & !(1 << target)) | (new_target_bit << target);
                new_state[j] = self.state[i];
                new_state[i] = Complex64::new(0.0, 0.0);
            }
        }
        
        self.state = new_state;
    }

    fn apply_cz(&mut self, control: usize, target: usize) {
        let size = self.state.len();
        
        for i in 0..size {
            let control_bit = (i >> control) & 1;
            let target_bit = (i >> target) & 1;
            if control_bit == 1 && target_bit == 1 {
                self.state[i] *= -1.0;
            }
        }
    }

    fn apply_toffoli(&mut self, control1: usize, control2: usize, target: usize) {
        let size = self.state.len();
        let mut new_state = self.state.clone();
        
        for i in 0..size {
            let control1_bit = (i >> control1) & 1;
            let control2_bit = (i >> control2) & 1;
            if control1_bit == 1 && control2_bit == 1 {
                let target_bit = (i >> target) & 1;
                let new_target_bit = 1 - target_bit;
                let j = (i & !(1 << target)) | (new_target_bit << target);
                new_state[j] = self.state[i];
                new_state[i] = Complex64::new(0.0, 0.0);
            }
        }
        
        self.state = new_state;
    }

    fn apply_controlled_gate(&mut self, control: usize, gate: &Gate) {
        // Simplified controlled gate implementation
        // In a full implementation, this would decompose the controlled gate
        match gate.as_ref() {
            Gate::X { target } => self.apply_cnot(control, target.qubit),
            Gate::Z { target } => self.apply_cz(control, target.qubit),
            _ => console_log!("Controlled gate not yet implemented for {:?}", gate),
        }
    }

    fn apply_single_qubit_gate<F>(&mut self, target: usize, gate_matrix: F)
    where
        F: Fn(&DVector<Complex64>) -> [[Complex64; 2]; 2],
    {
        let matrix = gate_matrix(&self.state);
        let size = self.state.len();
        let mut new_state = DVector::zeros(size);
        
        for i in 0..size {
            let target_bit = (i >> target) & 1;
            let other_bits = i & !(1 << target);
            
            let j0 = other_bits;
            let j1 = other_bits | (1 << target);
            
            if target_bit == 0 {
                new_state[j0] += matrix[0][0] * self.state[j0] + matrix[0][1] * self.state[j1];
            } else {
                new_state[j1] += matrix[1][0] * self.state[j0] + matrix[1][1] * self.state[j1];
            }
        }
        
        self.state = new_state;
    }

    pub fn get_amplitudes(&self) -> Vec<ComplexNumber> {
        self.state.iter().map(|&c| c.into()).collect()
    }

    pub fn get_probabilities(&self) -> Vec<f64> {
        self.state.iter().map(|c| c.norm_sqr()).collect()
    }

    pub fn measure_all(&mut self) -> Vec<bool> {
        let probabilities = self.get_probabilities();
        let mut rng = js_sys::Math::random();
        
        // Simple measurement simulation - find which basis state
        let mut cumulative = 0.0;
        let mut measured_state = 0;
        
        for (i, &prob) in probabilities.iter().enumerate() {
            cumulative += prob;
            if rng <= cumulative {
                measured_state = i;
                break;
            }
        }
        
        // Convert measured state to bit string
        (0..self.num_qubits)
            .map(|i| ((measured_state >> i) & 1) == 1)
            .collect()
    }
}

#[wasm_bindgen]
pub fn simulate_circuit(json: &str) -> String {
    set_panic_hook();
    
    console_log!("Simulating circuit with JSON: {}", json);
    
    let circuit: Circuit = match serde_json::from_str(json) {
        Ok(c) => c,
        Err(e) => {
            console_log!("Failed to parse circuit JSON: {}", e);
            return format!("{{\"error\": \"Failed to parse circuit: {}\"}}", e);
        }
    };
    
    // Determine number of qubits from gates
    let mut max_qubit = 0;
    for gate in &circuit.gates {
        match gate {
            Gate::I { target } | Gate::H { target } | Gate::X { target } |
            Gate::Y { target } | Gate::Z { target } | Gate::RX { target, .. } |
            Gate::RY { target, .. } | Gate::RZ { target, .. } |
            Gate::Phase { target, .. } | Gate::Measure { target } => {
                max_qubit = max_qubit.max(target.qubit);
            },
            Gate::CNOT { control, target } | Gate::CZ { control, target } => {
                max_qubit = max_qubit.max(control.qubit).max(target.qubit);
            },
            Gate::Toffoli { control1, control2, target } => {
                max_qubit = max_qubit.max(control1.qubit).max(control2.qubit).max(target.qubit);
            },
            Gate::Controlled { control, gate: _ } => {
                max_qubit = max_qubit.max(control.qubit);
            },
        }
    }
    
    let num_qubits = max_qubit + 1;
    console_log!("Creating simulator with {} qubits", num_qubits);
    
    let mut simulator = QuantumSimulator::new(num_qubits);
    
    for gate in &circuit.gates {
        simulator.apply_gate(gate);
    }
    
    let result = SimulationResult {
        amplitudes: simulator.get_amplitudes(),
        probabilities: simulator.get_probabilities(),
        num_qubits,
        measurement_results: None,
    };
    
    match serde_json::to_string(&result) {
        Ok(json) => json,
        Err(e) => {
            console_log!("Failed to serialize result: {}", e);
            format!("{{\"error\": \"Serialization failed: {}\"}}", e)
        }
    }
}

#[wasm_bindgen]
pub fn measure_circuit(json: &str) -> String {
    set_panic_hook();
    
    let circuit: Circuit = match serde_json::from_str(json) {
        Ok(c) => c,
        Err(e) => return format!("{{\"error\": \"Failed to parse circuit: {}\"}}", e),
    };
    
    // Run simulation and measure
    let mut max_qubit = 0;
    for gate in &circuit.gates {
        // ... same qubit counting logic ...
    }
    
    let num_qubits = max_qubit + 1;
    let mut simulator = QuantumSimulator::new(num_qubits);
    
    for gate in &circuit.gates {
        simulator.apply_gate(gate);
    }
    
    let measurements = simulator.measure_all();
    
    let result = SimulationResult {
        amplitudes: simulator.get_amplitudes(),
        probabilities: simulator.get_probabilities(),
        num_qubits,
        measurement_results: Some(measurements),
    };
    
    serde_json::to_string(&result).unwrap_or_else(|e| {
        format!("{{\"error\": \"Serialization failed: {}\"}}", e)
    })
} 