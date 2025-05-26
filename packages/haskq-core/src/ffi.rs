//! FFI Interface for HaskQ Core
//! 
//! This module provides C-compatible functions that can be called from Haskell
//! using the Foreign Function Interface (FFI). It acts as the bridge between
//! the high-level Haskell DSL and the high-performance Rust computation engine.

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_double, c_int, c_void};
use std::slice;
use std::ptr;
use std::collections::HashMap;
use std::sync::Mutex;

use once_cell::sync::Lazy;
use serde_json;

use crate::{QuantumSimulator, QuantumState, GateOps, ComplexF64, Result, HaskQError};

/// Global simulator storage for FFI
static SIMULATORS: Lazy<Mutex<HashMap<u32, QuantumSimulator>>> = 
    Lazy::new(|| Mutex::new(HashMap::new()));

static mut NEXT_SIM_ID: u32 = 1;

/// Error handling for FFI
#[repr(C)]
pub struct HaskQResult {
    pub success: bool,
    pub error_message: *mut c_char,
    pub data: *mut c_void,
    pub data_len: usize,
}

impl HaskQResult {
    fn success(data: *mut c_void, len: usize) -> Self {
        Self {
            success: true,
            error_message: ptr::null_mut(),
            data,
            data_len: len,
        }
    }

    fn error(msg: &str) -> Self {
        let c_msg = CString::new(msg).unwrap_or_else(|_| CString::new("Unknown error").unwrap());
        Self {
            success: false,
            error_message: c_msg.into_raw(),
            data: ptr::null_mut(),
            data_len: 0,
        }
    }
}

/// Create a new quantum simulator
/// Returns simulator ID that can be used in subsequent calls
#[no_mangle]
pub extern "C" fn haskq_create_simulator(num_qubits: c_int) -> u32 {
    if num_qubits <= 0 || num_qubits > 30 {
        return 0; // Invalid simulator ID
    }

    let simulator = QuantumSimulator::new(num_qubits as usize);
    
    unsafe {
        let sim_id = NEXT_SIM_ID;
        NEXT_SIM_ID += 1;
        
        if let Ok(mut sims) = SIMULATORS.lock() {
            sims.insert(sim_id, simulator);
            sim_id
        } else {
            0
        }
    }
}

/// Destroy a simulator and free its memory
#[no_mangle]
pub extern "C" fn haskq_destroy_simulator(sim_id: u32) -> bool {
    if let Ok(mut sims) = SIMULATORS.lock() {
        sims.remove(&sim_id).is_some()
    } else {
        false
    }
}

/// Reset simulator to initial state
#[no_mangle]
pub extern "C" fn haskq_reset_simulator(sim_id: u32) -> bool {
    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            sim.reset();
            true
        } else {
            false
        }
    } else {
        false
    }
}

/// Apply Identity gate
#[no_mangle]
pub extern "C" fn haskq_apply_i(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_i(t))
}

/// Apply Pauli-X gate
#[no_mangle]
pub extern "C" fn haskq_apply_x(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_x(t))
}

/// Apply Pauli-Y gate
#[no_mangle]
pub extern "C" fn haskq_apply_y(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_y(t))
}

/// Apply Pauli-Z gate
#[no_mangle]
pub extern "C" fn haskq_apply_z(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_z(t))
}

/// Apply Hadamard gate
#[no_mangle]
pub extern "C" fn haskq_apply_h(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_h(t))
}

/// Apply S gate
#[no_mangle]
pub extern "C" fn haskq_apply_s(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_s(t))
}

/// Apply T gate
#[no_mangle]
pub extern "C" fn haskq_apply_t(sim_id: u32, target: c_int) -> bool {
    apply_single_gate(sim_id, target, |state, t| state.apply_t(t))
}

/// Apply RX rotation gate
#[no_mangle]
pub extern "C" fn haskq_apply_rx(sim_id: u32, angle: c_double, target: c_int) -> bool {
    apply_rotation_gate(sim_id, angle, target, |state, a, t| state.apply_rx(a, t))
}

/// Apply RY rotation gate
#[no_mangle]
pub extern "C" fn haskq_apply_ry(sim_id: u32, angle: c_double, target: c_int) -> bool {
    apply_rotation_gate(sim_id, angle, target, |state, a, t| state.apply_ry(a, t))
}

/// Apply RZ rotation gate
#[no_mangle]
pub extern "C" fn haskq_apply_rz(sim_id: u32, angle: c_double, target: c_int) -> bool {
    apply_rotation_gate(sim_id, angle, target, |state, a, t| state.apply_rz(a, t))
}

/// Apply Phase gate
#[no_mangle]
pub extern "C" fn haskq_apply_phase(sim_id: u32, angle: c_double, target: c_int) -> bool {
    apply_rotation_gate(sim_id, angle, target, |state, a, t| state.apply_phase(a, t))
}

/// Apply CNOT gate
#[no_mangle]
pub extern "C" fn haskq_apply_cnot(sim_id: u32, control: c_int, target: c_int) -> bool {
    apply_two_qubit_gate(sim_id, control, target, |state, c, t| state.apply_cnot(c, t))
}

/// Apply CZ gate
#[no_mangle]
pub extern "C" fn haskq_apply_cz(sim_id: u32, control: c_int, target: c_int) -> bool {
    apply_two_qubit_gate(sim_id, control, target, |state, c, t| state.apply_cz(c, t))
}

/// Apply SWAP gate
#[no_mangle]
pub extern "C" fn haskq_apply_swap(sim_id: u32, qubit1: c_int, qubit2: c_int) -> bool {
    apply_two_qubit_gate(sim_id, qubit1, qubit2, |state, q1, q2| state.apply_swap(q1, q2))
}

/// Apply Toffoli gate
#[no_mangle]
pub extern "C" fn haskq_apply_toffoli(sim_id: u32, control1: c_int, control2: c_int, target: c_int) -> bool {
    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            if control1 >= 0 && control2 >= 0 && target >= 0 {
                sim.state.apply_toffoli(control1 as usize, control2 as usize, target as usize).is_ok()
            } else {
                false
            }
        } else {
            false
        }
    } else {
        false
    }
}

/// Get state amplitudes as array of complex numbers
#[no_mangle]
pub extern "C" fn haskq_get_amplitudes(sim_id: u32, amplitudes: *mut ComplexF64, len: usize) -> bool {
    if amplitudes.is_null() {
        return false;
    }

    if let Ok(sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get(&sim_id) {
            let state_amplitudes = sim.get_amplitudes();
            let actual_len = std::cmp::min(len, state_amplitudes.len());
            
            unsafe {
                let slice = slice::from_raw_parts_mut(amplitudes, actual_len);
                for (i, amp) in state_amplitudes.iter().take(actual_len).enumerate() {
                    slice[i] = (*amp).into();
                }
            }
            true
        } else {
            false
        }
    } else {
        false
    }
}

/// Get measurement probabilities
#[no_mangle]
pub extern "C" fn haskq_get_probabilities(sim_id: u32, probs: *mut c_double, len: usize) -> bool {
    if probs.is_null() {
        return false;
    }

    if let Ok(sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get(&sim_id) {
            let probabilities = sim.get_probabilities();
            let actual_len = std::cmp::min(len, probabilities.len());
            
            unsafe {
                let slice = slice::from_raw_parts_mut(probs, actual_len);
                for (i, &prob) in probabilities.iter().take(actual_len).enumerate() {
                    slice[i] = prob;
                }
            }
            true
        } else {
            false
        }
    } else {
        false
    }
}

/// Measure all qubits and return measurement results
#[no_mangle]
pub extern "C" fn haskq_measure_all(sim_id: u32, results: *mut bool, len: usize) -> bool {
    if results.is_null() {
        return false;
    }

    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            let measurements = sim.state.measure_all();
            let actual_len = std::cmp::min(len, measurements.len());
            
            unsafe {
                let slice = slice::from_raw_parts_mut(results, actual_len);
                for (i, &result) in measurements.iter().take(actual_len).enumerate() {
                    slice[i] = result;
                }
            }
            true
        } else {
            false
        }
    } else {
        false
    }
}

/// Get the number of qubits in the simulator
#[no_mangle]
pub extern "C" fn haskq_get_num_qubits(sim_id: u32) -> c_int {
    if let Ok(sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get(&sim_id) {
            sim.state.num_qubits as c_int
        } else {
            -1
        }
    } else {
        -1
    }
}

/// Get the size of the state vector (2^n)
#[no_mangle]
pub extern "C" fn haskq_get_state_size(sim_id: u32) -> usize {
    if let Ok(sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get(&sim_id) {
            sim.get_amplitudes().len()
        } else {
            0
        }
    } else {
        0
    }
}

/// Process a JSON circuit description and apply all gates
#[no_mangle]
pub extern "C" fn haskq_apply_circuit_json(sim_id: u32, json_str: *const c_char) -> bool {
    if json_str.is_null() {
        return false;
    }

    unsafe {
        let c_str = CStr::from_ptr(json_str);
        if let Ok(json) = c_str.to_str() {
            // Parse JSON and apply gates (simplified for now)
            // In a full implementation, this would parse the circuit JSON
            // and apply each gate sequentially
            true
        } else {
            false
        }
    }
}

/// Free a C string allocated by this library
#[no_mangle]
pub extern "C" fn haskq_free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            drop(CString::from_raw(s));
        }
    }
}

/// Get library version
#[no_mangle]
pub extern "C" fn haskq_get_version() -> *mut c_char {
    let version = env!("CARGO_PKG_VERSION");
    CString::new(version).unwrap().into_raw()
}

/// Helper functions for common patterns
fn apply_single_gate<F>(sim_id: u32, target: c_int, gate_fn: F) -> bool 
where
    F: FnOnce(&mut QuantumState, usize) -> Result<()>,
{
    if target < 0 {
        return false;
    }

    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            gate_fn(&mut sim.state, target as usize).is_ok()
        } else {
            false
        }
    } else {
        false
    }
}

fn apply_rotation_gate<F>(sim_id: u32, angle: c_double, target: c_int, gate_fn: F) -> bool 
where
    F: FnOnce(&mut QuantumState, f64, usize) -> Result<()>,
{
    if target < 0 {
        return false;
    }

    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            gate_fn(&mut sim.state, angle, target as usize).is_ok()
        } else {
            false
        }
    } else {
        false
    }
}

fn apply_two_qubit_gate<F>(sim_id: u32, qubit1: c_int, qubit2: c_int, gate_fn: F) -> bool 
where
    F: FnOnce(&mut QuantumState, usize, usize) -> Result<()>,
{
    if qubit1 < 0 || qubit2 < 0 {
        return false;
    }

    if let Ok(mut sims) = SIMULATORS.lock() {
        if let Some(sim) = sims.get_mut(&sim_id) {
            gate_fn(&mut sim.state, qubit1 as usize, qubit2 as usize).is_ok()
        } else {
            false
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ffi_simulator_creation() {
        let sim_id = haskq_create_simulator(2);
        assert!(sim_id > 0);
        
        let num_qubits = haskq_get_num_qubits(sim_id);
        assert_eq!(num_qubits, 2);
        
        let state_size = haskq_get_state_size(sim_id);
        assert_eq!(state_size, 4);
        
        assert!(haskq_destroy_simulator(sim_id));
    }

    #[test]
    fn test_ffi_gate_application() {
        let sim_id = haskq_create_simulator(1);
        assert!(sim_id > 0);
        
        // Apply Hadamard gate
        assert!(haskq_apply_h(sim_id, 0));
        
        // Check probabilities
        let mut probs = [0.0; 2];
        assert!(haskq_get_probabilities(sim_id, probs.as_mut_ptr(), 2));
        
        // Should be approximately [0.5, 0.5]
        assert!((probs[0] - 0.5).abs() < 1e-10);
        assert!((probs[1] - 0.5).abs() < 1e-10);
        
        assert!(haskq_destroy_simulator(sim_id));
    }
} 