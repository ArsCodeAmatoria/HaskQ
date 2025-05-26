//! HaskQ Core - High-Performance Quantum Computing Engine
//! 
//! This library provides the computational backbone for the HaskQ Framework,
//! offering high-performance quantum simulation, state manipulation, and
//! gate operations optimized for speed and memory efficiency.

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_double, c_int, c_void};
use std::slice;

use nalgebra::{Complex, DMatrix, DVector};
use num_complex::Complex64;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod quantum;
pub mod gates;
pub mod algorithms;
pub mod ffi;
pub mod parallel;

pub use quantum::*;
pub use gates::*;
pub use algorithms::*;

/// Error types for HaskQ Core operations
#[derive(Error, Debug)]
pub enum HaskQError {
    #[error("Invalid qubit index: {0}")]
    InvalidQubit(usize),
    #[error("Dimension mismatch: expected {expected}, got {actual}")]
    DimensionMismatch { expected: usize, actual: usize },
    #[error("Invalid gate parameter: {0}")]
    InvalidGateParameter(String),
    #[error("Memory allocation failed")]
    MemoryError,
    #[error("Simulation error: {0}")]
    SimulationError(String),
}

pub type Result<T> = std::result::Result<T, HaskQError>;

/// Complex number representation for FFI
#[repr(C)]
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ComplexF64 {
    pub real: f64,
    pub imag: f64,
}

impl From<Complex64> for ComplexF64 {
    fn from(c: Complex64) -> Self {
        Self { real: c.re, imag: c.im }
    }
}

impl Into<Complex64> for ComplexF64 {
    fn into(self) -> Complex64 {
        Complex64::new(self.real, self.imag)
    }
}

/// Quantum simulator state representation
#[derive(Debug, Clone)]
pub struct QuantumState {
    /// State vector amplitudes
    pub amplitudes: DVector<Complex64>,
    /// Number of qubits
    pub num_qubits: usize,
    /// Whether the state is normalized
    pub normalized: bool,
}

impl QuantumState {
    /// Create a new quantum state with n qubits in |0...0⟩
    pub fn new(num_qubits: usize) -> Self {
        let size = 1 << num_qubits;
        let mut amplitudes = DVector::zeros(size);
        amplitudes[0] = Complex64::new(1.0, 0.0);
        
        Self {
            amplitudes,
            num_qubits,
            normalized: true,
        }
    }

    /// Create quantum state from amplitudes
    pub fn from_amplitudes(amplitudes: Vec<Complex64>) -> Result<Self> {
        let size = amplitudes.len();
        if !size.is_power_of_two() {
            return Err(HaskQError::DimensionMismatch { 
                expected: 1, 
                actual: size 
            });
        }
        
        let num_qubits = size.trailing_zeros() as usize;
        let amplitudes = DVector::from_vec(amplitudes);
        
        Ok(Self {
            amplitudes,
            num_qubits,
            normalized: false,
        })
    }

    /// Normalize the quantum state
    pub fn normalize(&mut self) {
        let norm = self.amplitudes.norm();
        if norm > 1e-10 {
            self.amplitudes /= norm;
            self.normalized = true;
        }
    }

    /// Get probability of measuring a specific basis state
    pub fn probability(&self, state: usize) -> f64 {
        if state >= self.amplitudes.len() {
            0.0
        } else {
            self.amplitudes[state].norm_sqr()
        }
    }

    /// Get all measurement probabilities
    pub fn probabilities(&self) -> Vec<f64> {
        self.amplitudes.iter().map(|a| a.norm_sqr()).collect()
    }

    /// Measure all qubits and collapse the state
    pub fn measure_all(&mut self) -> Vec<bool> {
        let probabilities = self.probabilities();
        let rand_val: f64 = rand::random();
        
        let mut cumulative = 0.0;
        let mut measured_state = 0;
        
        for (i, &prob) in probabilities.iter().enumerate() {
            cumulative += prob;
            if rand_val <= cumulative {
                measured_state = i;
                break;
            }
        }
        
        // Collapse to measured state
        self.amplitudes.fill(Complex64::new(0.0, 0.0));
        self.amplitudes[measured_state] = Complex64::new(1.0, 0.0);
        self.normalized = true;
        
        // Convert to bit string
        (0..self.num_qubits)
            .map(|i| (measured_state >> i) & 1 == 1)
            .collect()
    }

    /// Apply a single-qubit gate
    pub fn apply_single_gate(&mut self, target: usize, gate_matrix: &[[Complex64; 2]; 2]) -> Result<()> {
        if target >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(target));
        }

        let size = self.amplitudes.len();
        let mut new_amplitudes = self.amplitudes.clone();
        
        // Parallel application for large states
        if size > 1024 {
            (0..size).into_par_iter().for_each(|i| {
                let target_bit = (i >> target) & 1;
                let other_bits = i & !(1 << target);
                
                let state_0 = other_bits;
                let state_1 = other_bits | (1 << target);
                
                if target_bit == 0 {
                    new_amplitudes[state_0] = gate_matrix[0][0] * self.amplitudes[state_0] 
                                            + gate_matrix[0][1] * self.amplitudes[state_1];
                } else {
                    new_amplitudes[state_1] = gate_matrix[1][0] * self.amplitudes[state_0] 
                                            + gate_matrix[1][1] * self.amplitudes[state_1];
                }
            });
        } else {
            for i in 0..size {
                let target_bit = (i >> target) & 1;
                let other_bits = i & !(1 << target);
                
                let state_0 = other_bits;
                let state_1 = other_bits | (1 << target);
                
                if target_bit == 0 {
                    new_amplitudes[state_0] = gate_matrix[0][0] * self.amplitudes[state_0] 
                                            + gate_matrix[0][1] * self.amplitudes[state_1];
                } else {
                    new_amplitudes[state_1] = gate_matrix[1][0] * self.amplitudes[state_0] 
                                            + gate_matrix[1][1] * self.amplitudes[state_1];
                }
            }
        }
        
        self.amplitudes = new_amplitudes;
        self.normalized = false;
        Ok(())
    }

    /// Apply a two-qubit gate
    pub fn apply_two_qubit_gate(&mut self, control: usize, target: usize, 
                               gate_fn: impl Fn(usize, usize, &mut DVector<Complex64>)) -> Result<()> {
        if control >= self.num_qubits || target >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(std::cmp::max(control, target)));
        }
        
        gate_fn(control, target, &mut self.amplitudes);
        self.normalized = false;
        Ok(())
    }
}

/// High-performance quantum simulator
#[derive(Debug)]
pub struct QuantumSimulator {
    /// Current quantum state
    pub state: QuantumState,
    /// Execution statistics
    pub stats: SimulationStats,
}

#[derive(Debug, Default)]
pub struct SimulationStats {
    pub gates_applied: usize,
    pub measurements_taken: usize,
    pub total_time_ns: u128,
}

impl QuantumSimulator {
    /// Create a new quantum simulator
    pub fn new(num_qubits: usize) -> Self {
        Self {
            state: QuantumState::new(num_qubits),
            stats: SimulationStats::default(),
        }
    }

    /// Get current state amplitudes
    pub fn get_amplitudes(&self) -> &DVector<Complex64> {
        &self.state.amplitudes
    }

    /// Get measurement probabilities
    pub fn get_probabilities(&self) -> Vec<f64> {
        self.state.probabilities()
    }

    /// Reset simulator to initial state
    pub fn reset(&mut self) {
        self.state = QuantumState::new(self.state.num_qubits);
        self.stats = SimulationStats::default();
    }

    /// Clone the current state
    pub fn clone_state(&self) -> QuantumState {
        self.state.clone()
    }

    /// Set state from external amplitudes
    pub fn set_state(&mut self, amplitudes: Vec<Complex64>) -> Result<()> {
        self.state = QuantumState::from_amplitudes(amplitudes)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quantum_state_creation() {
        let state = QuantumState::new(2);
        assert_eq!(state.num_qubits, 2);
        assert_eq!(state.amplitudes.len(), 4);
        assert_eq!(state.amplitudes[0], Complex64::new(1.0, 0.0));
        assert!(state.normalized);
    }

    #[test]
    fn test_probabilities() {
        let state = QuantumState::new(2);
        let probs = state.probabilities();
        assert_eq!(probs, vec![1.0, 0.0, 0.0, 0.0]);
    }

    #[test]
    fn test_simulator_creation() {
        let sim = QuantumSimulator::new(3);
        assert_eq!(sim.state.num_qubits, 3);
        assert_eq!(sim.get_amplitudes().len(), 8);
    }
} 