//! High-performance quantum gate implementations
//! 
//! This module provides optimized implementations of all quantum gates
//! with support for parallel execution and memory-efficient operations.

use nalgebra::DVector;
use num_complex::Complex64;
#[cfg(feature = "parallel")]
use rayon::prelude::*;
use std::f64::consts::PI;

use crate::{QuantumState, Result, HaskQError};

/// Single-qubit gate matrices
pub struct SingleQubitGates;

impl SingleQubitGates {
    /// Identity gate matrix
    pub const I: [[Complex64; 2]; 2] = [
        [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
        [Complex64::new(0.0, 0.0), Complex64::new(1.0, 0.0)],
    ];

    /// Pauli-X gate matrix (bit flip)
    pub const X: [[Complex64; 2]; 2] = [
        [Complex64::new(0.0, 0.0), Complex64::new(1.0, 0.0)],
        [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
    ];

    /// Pauli-Y gate matrix
    pub const Y: [[Complex64; 2]; 2] = [
        [Complex64::new(0.0, 0.0), Complex64::new(0.0, -1.0)],
        [Complex64::new(0.0, 1.0), Complex64::new(0.0, 0.0)],
    ];

    /// Pauli-Z gate matrix (phase flip)
    pub const Z: [[Complex64; 2]; 2] = [
        [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
        [Complex64::new(0.0, 0.0), Complex64::new(-1.0, 0.0)],
    ];

    /// Hadamard gate matrix
    pub fn h() -> [[Complex64; 2]; 2] {
        let inv_sqrt2 = 1.0 / 2.0_f64.sqrt();
        [
            [Complex64::new(inv_sqrt2, 0.0), Complex64::new(inv_sqrt2, 0.0)],
            [Complex64::new(inv_sqrt2, 0.0), Complex64::new(-inv_sqrt2, 0.0)],
        ]
    }

    /// S gate (π/2 phase rotation)
    pub const S: [[Complex64; 2]; 2] = [
        [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
        [Complex64::new(0.0, 0.0), Complex64::new(0.0, 1.0)],
    ];

    /// T gate (π/4 phase rotation)
    pub fn t() -> [[Complex64; 2]; 2] {
        let phase = Complex64::new(0.0, PI / 4.0).exp();
        [
            [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
            [Complex64::new(0.0, 0.0), phase],
        ]
    }

    /// Rotation around X-axis
    pub fn rx(angle: f64) -> [[Complex64; 2]; 2] {
        let cos_half = (angle / 2.0).cos();
        let sin_half = (angle / 2.0).sin();
        [
            [Complex64::new(cos_half, 0.0), Complex64::new(0.0, -sin_half)],
            [Complex64::new(0.0, -sin_half), Complex64::new(cos_half, 0.0)],
        ]
    }

    /// Rotation around Y-axis
    pub fn ry(angle: f64) -> [[Complex64; 2]; 2] {
        let cos_half = (angle / 2.0).cos();
        let sin_half = (angle / 2.0).sin();
        [
            [Complex64::new(cos_half, 0.0), Complex64::new(-sin_half, 0.0)],
            [Complex64::new(sin_half, 0.0), Complex64::new(cos_half, 0.0)],
        ]
    }

    /// Rotation around Z-axis
    pub fn rz(angle: f64) -> [[Complex64; 2]; 2] {
        let exp_neg = Complex64::new(0.0, -angle / 2.0).exp();
        let exp_pos = Complex64::new(0.0, angle / 2.0).exp();
        [
            [exp_neg, Complex64::new(0.0, 0.0)],
            [Complex64::new(0.0, 0.0), exp_pos],
        ]
    }

    /// Phase gate
    pub fn phase(angle: f64) -> [[Complex64; 2]; 2] {
        let phase = Complex64::new(0.0, angle).exp();
        [
            [Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
            [Complex64::new(0.0, 0.0), phase],
        ]
    }
}

/// Two-qubit gate operations
pub struct TwoQubitGates;

impl TwoQubitGates {
    /// Apply CNOT gate (control, target)
    pub fn cnot(control: usize, target: usize, amplitudes: &mut DVector<Complex64>) {
        let size = amplitudes.len();
        let mut new_amplitudes = amplitudes.clone();
        
        #[cfg(feature = "parallel")]
        if size > 1024 {
            // Parallel execution for large states
            (0..size).into_par_iter().for_each(|i| {
                let control_bit = (i >> control) & 1;
                if control_bit == 1 {
                    let target_bit = (i >> target) & 1;
                    let new_target_bit = 1 - target_bit;
                    let j = (i & !(1 << target)) | (new_target_bit << target);
                    new_amplitudes[j] = amplitudes[i];
                    new_amplitudes[i] = Complex64::new(0.0, 0.0);
                }
            });
        } else {
            for i in 0..size {
                let control_bit = (i >> control) & 1;
                if control_bit == 1 {
                    let target_bit = (i >> target) & 1;
                    let new_target_bit = 1 - target_bit;
                    let j = (i & !(1 << target)) | (new_target_bit << target);
                    new_amplitudes[j] = amplitudes[i];
                    new_amplitudes[i] = Complex64::new(0.0, 0.0);
                }
            }
        }
        
        #[cfg(not(feature = "parallel"))]
        {
            for i in 0..size {
                let control_bit = (i >> control) & 1;
                if control_bit == 1 {
                    let target_bit = (i >> target) & 1;
                    let new_target_bit = 1 - target_bit;
                    let j = (i & !(1 << target)) | (new_target_bit << target);
                    new_amplitudes[j] = amplitudes[i];
                    new_amplitudes[i] = Complex64::new(0.0, 0.0);
                }
            }
        }
        
        *amplitudes = new_amplitudes;
    }

    /// Apply CZ gate (control, target)
    pub fn cz(control: usize, target: usize, amplitudes: &mut DVector<Complex64>) {
        let size = amplitudes.len();
        
        #[cfg(feature = "parallel")]
        if size > 1024 {
            (0..size).into_par_iter().for_each(|i| {
                let control_bit = (i >> control) & 1;
                let target_bit = (i >> target) & 1;
                if control_bit == 1 && target_bit == 1 {
                    amplitudes[i] *= -1.0;
                }
            });
        } else {
            for i in 0..size {
                let control_bit = (i >> control) & 1;
                let target_bit = (i >> target) & 1;
                if control_bit == 1 && target_bit == 1 {
                    amplitudes[i] *= -1.0;
                }
            }
        }
        
        #[cfg(not(feature = "parallel"))]
        {
            for i in 0..size {
                let control_bit = (i >> control) & 1;
                let target_bit = (i >> target) & 1;
                if control_bit == 1 && target_bit == 1 {
                    amplitudes[i] *= -1.0;
                }
            }
        }
    }

    /// Apply SWAP gate
    pub fn swap(qubit1: usize, qubit2: usize, amplitudes: &mut DVector<Complex64>) {
        Self::cnot(qubit1, qubit2, amplitudes);
        Self::cnot(qubit2, qubit1, amplitudes);
        Self::cnot(qubit1, qubit2, amplitudes);
    }

    /// Apply controlled-phase gate
    pub fn cphase(control: usize, target: usize, angle: f64, amplitudes: &mut DVector<Complex64>) {
        let phase = Complex64::new(0.0, angle).exp();
        let size = amplitudes.len();
        
        for i in 0..size {
            let control_bit = (i >> control) & 1;
            let target_bit = (i >> target) & 1;
            if control_bit == 1 && target_bit == 1 {
                amplitudes[i] *= phase;
            }
        }
    }
}

/// Three-qubit gate operations
pub struct ThreeQubitGates;

impl ThreeQubitGates {
    /// Apply Toffoli gate (CCX)
    pub fn toffoli(control1: usize, control2: usize, target: usize, 
                   amplitudes: &mut DVector<Complex64>) {
        let size = amplitudes.len();
        let mut new_amplitudes = amplitudes.clone();
        
        for i in 0..size {
            let control1_bit = (i >> control1) & 1;
            let control2_bit = (i >> control2) & 1;
            
            if control1_bit == 1 && control2_bit == 1 {
                let target_bit = (i >> target) & 1;
                let new_target_bit = 1 - target_bit;
                let j = (i & !(1 << target)) | (new_target_bit << target);
                new_amplitudes[j] = amplitudes[i];
                new_amplitudes[i] = Complex64::new(0.0, 0.0);
            }
        }
        
        *amplitudes = new_amplitudes;
    }

    /// Apply Fredkin gate (CSWAP)
    pub fn fredkin(control: usize, target1: usize, target2: usize, 
                   amplitudes: &mut DVector<Complex64>) {
        let size = amplitudes.len();
        let mut new_amplitudes = amplitudes.clone();
        
        for i in 0..size {
            let control_bit = (i >> control) & 1;
            
            if control_bit == 1 {
                let target1_bit = (i >> target1) & 1;
                let target2_bit = (i >> target2) & 1;
                
                if target1_bit != target2_bit {
                    let j = i ^ (1 << target1) ^ (1 << target2);
                    new_amplitudes[j] = amplitudes[i];
                    new_amplitudes[i] = Complex64::new(0.0, 0.0);
                }
            }
        }
        
        *amplitudes = new_amplitudes;
    }
}

/// High-level gate application interface
pub trait GateOps {
    fn apply_i(&mut self, target: usize) -> Result<()>;
    fn apply_x(&mut self, target: usize) -> Result<()>;
    fn apply_y(&mut self, target: usize) -> Result<()>;
    fn apply_z(&mut self, target: usize) -> Result<()>;
    fn apply_h(&mut self, target: usize) -> Result<()>;
    fn apply_s(&mut self, target: usize) -> Result<()>;
    fn apply_t(&mut self, target: usize) -> Result<()>;
    fn apply_rx(&mut self, angle: f64, target: usize) -> Result<()>;
    fn apply_ry(&mut self, angle: f64, target: usize) -> Result<()>;
    fn apply_rz(&mut self, angle: f64, target: usize) -> Result<()>;
    fn apply_phase(&mut self, angle: f64, target: usize) -> Result<()>;
    fn apply_cnot(&mut self, control: usize, target: usize) -> Result<()>;
    fn apply_cz(&mut self, control: usize, target: usize) -> Result<()>;
    fn apply_swap(&mut self, qubit1: usize, qubit2: usize) -> Result<()>;
    fn apply_toffoli(&mut self, control1: usize, control2: usize, target: usize) -> Result<()>;
    fn apply_fredkin(&mut self, control: usize, target1: usize, target2: usize) -> Result<()>;
}

impl GateOps for QuantumState {
    fn apply_i(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::I)
    }

    fn apply_x(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::X)
    }

    fn apply_y(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::Y)
    }

    fn apply_z(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::Z)
    }

    fn apply_h(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::h())
    }

    fn apply_s(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::S)
    }

    fn apply_t(&mut self, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::t())
    }

    fn apply_rx(&mut self, angle: f64, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::rx(angle))
    }

    fn apply_ry(&mut self, angle: f64, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::ry(angle))
    }

    fn apply_rz(&mut self, angle: f64, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::rz(angle))
    }

    fn apply_phase(&mut self, angle: f64, target: usize) -> Result<()> {
        self.apply_single_gate(target, &SingleQubitGates::phase(angle))
    }

    fn apply_cnot(&mut self, control: usize, target: usize) -> Result<()> {
        self.apply_two_qubit_gate(control, target, TwoQubitGates::cnot)
    }

    fn apply_cz(&mut self, control: usize, target: usize) -> Result<()> {
        self.apply_two_qubit_gate(control, target, TwoQubitGates::cz)
    }

    fn apply_swap(&mut self, qubit1: usize, qubit2: usize) -> Result<()> {
        self.apply_two_qubit_gate(qubit1, qubit2, TwoQubitGates::swap)
    }

    fn apply_toffoli(&mut self, control1: usize, control2: usize, target: usize) -> Result<()> {
        if control1 >= self.num_qubits || control2 >= self.num_qubits || target >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(format!(
                "Qubit index out of bounds: max index is {}", 
                std::cmp::max(control1, std::cmp::max(control2, target))
            )));
        }
        
        ThreeQubitGates::toffoli(control1, control2, target, &mut self.amplitudes);
        self.normalized = false;
        Ok(())
    }

    fn apply_fredkin(&mut self, control: usize, target1: usize, target2: usize) -> Result<()> {
        if control >= self.num_qubits || target1 >= self.num_qubits || target2 >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(format!(
                "Qubit index out of bounds: max index is {}", 
                std::cmp::max(control, std::cmp::max(target1, target2))
            )));
        }
        
        ThreeQubitGates::fredkin(control, target1, target2, &mut self.amplitudes);
        self.normalized = false;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hadamard_gate() {
        let mut state = QuantumState::new(1);
        state.apply_h(0).unwrap();
        
        let probs = state.probabilities();
        assert!((probs[0] - 0.5).abs() < 1e-10);
        assert!((probs[1] - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_cnot_gate() {
        let mut state = QuantumState::new(2);
        state.apply_h(0).unwrap();
        state.apply_cnot(0, 1).unwrap();
        
        let probs = state.probabilities();
        // Should create Bell state: |00⟩ + |11⟩
        assert!((probs[0] - 0.5).abs() < 1e-10); // |00⟩
        assert!((probs[1] - 0.0).abs() < 1e-10); // |01⟩
        assert!((probs[2] - 0.0).abs() < 1e-10); // |10⟩
        assert!((probs[3] - 0.5).abs() < 1e-10); // |11⟩
    }

    #[test]
    fn test_pauli_x_gate() {
        let mut state = QuantumState::new(1);
        state.apply_x(0).unwrap();
        
        let probs = state.probabilities();
        assert!((probs[0] - 0.0).abs() < 1e-10); // |0⟩
        assert!((probs[1] - 1.0).abs() < 1e-10); // |1⟩
    }
} 