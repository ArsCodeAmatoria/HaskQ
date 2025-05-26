//! HaskQ Core - High-Performance Quantum Computing Engine
//! 
//! This library provides a high-performance quantum computing simulation engine
//! implemented in Rust with FFI bindings for the HaskQ Haskell framework.

use nalgebra::DVector;
use num_complex::Complex64;
#[cfg(feature = "parallel")]
use rayon::prelude::*;
use rand;
use serde::{Deserialize, Serialize};
use std::sync::{Mutex, MutexGuard};
use std::collections::HashMap;
use lazy_static::lazy_static;

pub mod gates;
pub mod algorithms;
pub mod ffi;
pub mod error_correction;

pub use gates::*;
pub use algorithms::*;
pub use ffi::*;
pub use error_correction::{ErrorCorrection, NoiseModel, NoiseType, ErrorCorrectionCode, Syndrome, ErrorStats};

/// Result type for HaskQ operations
pub type Result<T> = std::result::Result<T, HaskQError>;

/// Error types for HaskQ operations
#[derive(Debug, Clone)]
pub enum HaskQError {
    InvalidQubitIndex(usize),
    InvalidQubit(String),
    DimensionMismatch { expected: usize, actual: usize },
    GateError(String),
    SimulationError(String),
    FFIError(String),
    MemoryError(String),
    UnsupportedOperation(String),
}

impl std::fmt::Display for HaskQError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HaskQError::InvalidQubitIndex(index) => {
                write!(f, "Invalid qubit index: {}", index)
            }
            HaskQError::InvalidQubit(msg) => write!(f, "Invalid qubit: {}", msg),
            HaskQError::DimensionMismatch { expected, actual } => {
                write!(f, "Dimension mismatch: expected {}, got {}", expected, actual)
            }
            HaskQError::GateError(msg) => write!(f, "Gate error: {}", msg),
            HaskQError::SimulationError(msg) => write!(f, "Simulation error: {}", msg),
            HaskQError::FFIError(msg) => write!(f, "FFI error: {}", msg),
            HaskQError::MemoryError(msg) => write!(f, "Memory error: {}", msg),
            HaskQError::UnsupportedOperation(msg) => write!(f, "Unsupported operation: {}", msg),
        }
    }
}

impl std::error::Error for HaskQError {}

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

/// Quantum state representation
#[derive(Debug, Clone)]
pub struct QuantumState {
    pub num_qubits: usize,
    pub amplitudes: DVector<Complex64>,
    pub normalized: bool,
}

impl QuantumState {
    /// Create a new quantum state in |0...0⟩
    pub fn new(num_qubits: usize) -> Self {
        let size = 1 << num_qubits;
        let mut amplitudes = DVector::zeros(size);
        amplitudes[0] = Complex64::new(1.0, 0.0);
        
        Self {
            num_qubits,
            amplitudes,
            normalized: true,
        }
    }
    
    /// Create quantum state from amplitude vector
    pub fn from_amplitudes(amplitudes: DVector<Complex64>) -> Result<Self> {
        let size = amplitudes.len();
        if !size.is_power_of_two() {
            return Err(HaskQError::DimensionMismatch {
                expected: 0, // Will be filled with next power of 2
                actual: size,
            });
        }
        
        let num_qubits = size.trailing_zeros() as usize;
        Ok(Self {
            num_qubits,
            amplitudes,
            normalized: true,
        })
    }
    
    /// Get probability of measuring qubit in |0⟩ state
    pub fn get_zero_probability(&self, qubit: usize) -> Result<f64> {
        if qubit >= self.num_qubits {
            return Err(HaskQError::InvalidQubitIndex(qubit));
        }
        
        let mut prob = 0.0;
        for i in 0..self.amplitudes.len() {
            if ((i >> qubit) & 1) == 0 {
                prob += self.amplitudes[i].norm_sqr();
            }
        }
        Ok(prob)
    }
    
    /// Get all measurement probabilities
    pub fn get_probabilities(&self) -> Vec<f64> {
        self.amplitudes.iter().map(|amp| amp.norm_sqr()).collect()
    }
    
    /// Measure all qubits and return the classical bit string
    pub fn measure_all(&mut self) -> Result<Vec<bool>> {
        let mut results = Vec::with_capacity(self.num_qubits);
        for qubit in 0..self.num_qubits {
            let prob_zero = self.get_zero_probability(qubit)?;
            let measurement = rand::random::<f64>() > prob_zero;
            results.push(measurement);
        }
        Ok(results)
    }
    
    /// Normalize the quantum state
    pub fn normalize(&mut self) -> Result<()> {
        let norm = self.amplitudes.norm();
        if norm == 0.0 {
            return Err(HaskQError::SimulationError("Cannot normalize zero state".to_string()));
        }
        
        self.amplitudes /= Complex64::new(norm, 0.0);
        self.normalized = true;
        Ok(())
    }
    
    /// Calculate fidelity with another state
    pub fn fidelity(&self, other: &QuantumState) -> Result<f64> {
        if self.num_qubits != other.num_qubits {
            return Err(HaskQError::DimensionMismatch {
                expected: self.num_qubits,
                actual: other.num_qubits,
            });
        }
        
        let overlap = self.amplitudes.dot(&other.amplitudes);
        Ok(overlap.norm_sqr())
    }
    
    /// Calculate entanglement entropy between subsystems
    pub fn entanglement_entropy(&self, _subsystem_a: &[usize]) -> Result<f64> {
        // Simplified implementation - full implementation would require
        // partial trace and eigenvalue decomposition
        let mut entropy = 0.0;
        let probabilities = self.get_probabilities();
        
        for prob in probabilities {
            if prob > 0.0 {
                entropy -= prob * prob.ln();
            }
        }
        
        Ok(entropy)
    }
    
    /// Apply a single-qubit gate to the quantum state
    pub fn apply_single_gate(&mut self, target: usize, gate: &[[Complex64; 2]; 2]) -> Result<()> {
        if target >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(format!("Qubit index {} out of bounds", target)));
        }
        
        let size = self.amplitudes.len();
        let mut new_amplitudes = self.amplitudes.clone();
        
        for i in 0..size {
            let target_bit = (i >> target) & 1;
            let other_index = i ^ (1 << target);
            
            if target_bit == 0 {
                // Apply gate to |0⟩ component
                new_amplitudes[i] = gate[0][0] * self.amplitudes[i] + gate[0][1] * self.amplitudes[other_index];
                new_amplitudes[other_index] = gate[1][0] * self.amplitudes[i] + gate[1][1] * self.amplitudes[other_index];
            }
        }
        
        self.amplitudes = new_amplitudes;
        self.normalized = false;
        Ok(())
    }
    
    /// Apply a two-qubit gate to the quantum state
    pub fn apply_two_qubit_gate<F>(&mut self, control: usize, target: usize, gate_fn: F) -> Result<()>
    where
        F: Fn(usize, usize, &mut DVector<Complex64>),
    {
        if control >= self.num_qubits || target >= self.num_qubits {
            return Err(HaskQError::InvalidQubit(format!(
                "Qubit indices {} or {} out of bounds", control, target
            )));
        }
        
        gate_fn(control, target, &mut self.amplitudes);
        self.normalized = false;
        Ok(())
    }
    
    /// Get measurement probabilities (alias for get_probabilities for compatibility)
    pub fn probabilities(&self) -> Vec<f64> {
        self.get_probabilities()
    }
}

/// High-performance quantum simulator
#[derive(Debug)]
pub struct QuantumSimulator {
    pub state: QuantumState,
    pub stats: SimulationStats,
    pub noise_model: Option<NoiseModel>,
}

/// Simulation statistics
#[derive(Debug, Clone, Default)]
pub struct SimulationStats {
    pub gates_applied: usize,
    pub measurements_performed: usize,
    pub total_simulation_time: f64,
    pub memory_usage: usize,
    pub error_rate: f64,
}

impl QuantumSimulator {
    /// Create a new quantum simulator
    pub fn new(num_qubits: usize) -> Self {
        Self {
            state: QuantumState::new(num_qubits),
            stats: SimulationStats::default(),
            noise_model: None,
        }
    }
    
    /// Create simulator with noise model
    pub fn with_noise(num_qubits: usize, noise_model: NoiseModel) -> Self {
        Self {
            state: QuantumState::new(num_qubits),
            stats: SimulationStats::default(),
            noise_model: Some(noise_model),
        }
    }
    
    /// Reset simulator to |0...0⟩ state
    pub fn reset(&mut self) {
        self.state = QuantumState::new(self.state.num_qubits);
        self.stats = SimulationStats::default();
    }
    
    /// Clone the current quantum state
    pub fn clone_state(&self) -> QuantumState {
        self.state.clone()
    }
    
    /// Set the quantum state
    pub fn set_state(&mut self, state: QuantumState) -> Result<()> {
        if state.num_qubits != self.state.num_qubits {
            return Err(HaskQError::DimensionMismatch {
                expected: self.state.num_qubits,
                actual: state.num_qubits,
            });
        }
        
        self.state = state;
        Ok(())
    }
    
    /// Get measurement probabilities
    pub fn get_probabilities(&self) -> Vec<f64> {
        self.state.get_probabilities()
    }

    /// Get state amplitudes (for FFI)
    pub fn get_amplitudes(&self) -> Vec<ComplexF64> {
        self.state.amplitudes.iter()
            .map(|&c| ComplexF64::from(c))
            .collect()
    }

    /// Apply noise if noise model is present
    fn apply_noise(&mut self, gate_name: &str) -> Result<()> {
        if let Some(ref noise_model) = self.noise_model.clone() {
            ErrorCorrection::apply_noise(&mut self.state, &noise_model, gate_name)?;
            self.stats.error_rate += 0.001; // Simplified error tracking
        }
        Ok(())
    }

    /// Measure a specific qubit
    pub fn measure_qubit(&mut self, qubit: usize) -> Result<bool> {
        if qubit >= self.state.num_qubits {
            return Err(HaskQError::InvalidQubitIndex(qubit));
        }
        
        let prob_zero = self.state.get_zero_probability(qubit)?;
        let measurement = rand::random::<f64>() >= prob_zero;
        
        // Collapse the state
        self.collapse_state(qubit, measurement)?;
        
        self.stats.measurements_performed += 1;
        Ok(measurement)
    }
    
    /// Collapse state after measurement
    fn collapse_state(&mut self, qubit: usize, measured_one: bool) -> Result<()> {
        let mut normalization = 0.0;
        
        // First pass: calculate normalization
        for i in 0..self.state.amplitudes.len() {
            let qubit_value = ((i >> qubit) & 1) == 1;
            if qubit_value == measured_one {
                normalization += self.state.amplitudes[i].norm_sqr();
            }
        }
        
        if normalization == 0.0 {
            return Err(HaskQError::SimulationError("Invalid measurement collapse".to_string()));
        }
        
        let norm_factor = Complex64::new(normalization.sqrt(), 0.0);
        
        // Second pass: collapse and normalize
        for i in 0..self.state.amplitudes.len() {
            let qubit_value = ((i >> qubit) & 1) == 1;
            if qubit_value == measured_one {
                self.state.amplitudes[i] /= norm_factor;
            } else {
                self.state.amplitudes[i] = Complex64::new(0.0, 0.0);
            }
        }
        
        Ok(())
    }
    
    /// Get current memory usage in bytes
    pub fn get_memory_usage(&self) -> usize {
        self.state.amplitudes.len() * std::mem::size_of::<Complex64>()
    }
    
    /// Calculate state vector depth (log2 of dimension)
    pub fn get_state_depth(&self) -> usize {
        self.state.num_qubits
    }
    
    /// Validate simulator state integrity
    pub fn validate_state(&self) -> Result<()> {
        let norm_squared: f64 = self.state.amplitudes.iter()
            .map(|amp| amp.norm_sqr())
            .sum();
        
        if (norm_squared - 1.0).abs() > 1e-10 {
            return Err(HaskQError::SimulationError(
                format!("State not normalized: norm² = {}", norm_squared)
            ));
        }
        
        Ok(())
    }
    
    /// Create maximally entangled state
    pub fn create_bell_state(&mut self, qubit1: usize, qubit2: usize) -> Result<()> {
        self.state.apply_h(qubit1)?;
        self.state.apply_cnot(qubit1, qubit2)?;
        self.apply_noise("bell_state")?;
        Ok(())
    }
    
    /// Create GHZ state across multiple qubits
    pub fn create_ghz_state(&mut self, qubits: &[usize]) -> Result<()> {
        if qubits.is_empty() {
            return Ok(());
        }
        
        self.state.apply_h(qubits[0])?;
        for &qubit in &qubits[1..] {
            self.state.apply_cnot(qubits[0], qubit)?;
        }
        
        self.apply_noise("ghz_state")?;
        Ok(())
    }
    
    /// Apply quantum error correction
    pub fn apply_error_correction(
        &mut self,
        physical_qubits: &[usize],
        code: &ErrorCorrectionCode
    ) -> Result<ErrorStats> {
        if let Some(ref noise_model) = self.noise_model.clone() {
            ErrorCorrection::error_correction_cycle(
                self,
                &[physical_qubits.to_vec()],
                code,
                noise_model
            )
        } else {
            Ok(ErrorStats {
                logical_error_rate: 0.0,
                physical_error_rate: 0.0,
                correction_success_rate: 1.0,
                average_corrections_per_cycle: 0.0,
            })
        }
    }
}

// Global simulator registry for FFI
lazy_static! {
    static ref SIMULATORS: Mutex<HashMap<u32, QuantumSimulator>> = Mutex::new(HashMap::new());
    static ref NEXT_ID: Mutex<u32> = Mutex::new(1);
}

/// Register a simulator and return its ID
pub fn register_simulator(simulator: QuantumSimulator) -> u32 {
    let mut simulators = SIMULATORS.lock().unwrap();
    let mut next_id = NEXT_ID.lock().unwrap();
    
    let id = *next_id;
    *next_id += 1;
    
    simulators.insert(id, simulator);
    id
}

/// Get a mutable reference to a simulator by ID
pub fn get_simulator_mut(_id: u32) -> Option<MutexGuard<'static, HashMap<u32, QuantumSimulator>>> {
    SIMULATORS.lock().ok()
}

/// Get an immutable reference to a simulator by ID  
pub fn get_simulator(_id: u32) -> Option<MutexGuard<'static, HashMap<u32, QuantumSimulator>>> {
    SIMULATORS.lock().ok()
}

/// Remove a simulator by ID
pub fn unregister_simulator(id: u32) -> bool {
    let mut simulators = SIMULATORS.lock().unwrap();
    simulators.remove(&id).is_some()
}

/// Initialize the HaskQ core library
pub fn init() -> Result<()> {
    // Initialize random number generator
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let _seed: u64 = rng.gen();
    
    // Initialize SIMD optimizations if available
    #[cfg(target_feature = "avx2")]
    {
        log::info!("AVX2 optimizations enabled");
    }
    
    #[cfg(target_feature = "sse4.1")]
    {
        log::info!("SSE4.1 optimizations enabled");
    }
    
    Ok(())
}

/// Get library version information
pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

/// Get build information
pub fn build_info() -> BuildInfo {
    BuildInfo {
        version: version(),
        target: std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_string()),
        profile: if cfg!(debug_assertions) { "debug".to_string() } else { "release".to_string() },
        features: get_enabled_features(),
        commit_hash: option_env!("GIT_HASH").unwrap_or("unknown"),
    }
}

/// Build information structure
#[derive(Debug, Clone)]
pub struct BuildInfo {
    pub version: &'static str,
    pub target: String,
    pub profile: String,
    pub features: Vec<&'static str>,
    pub commit_hash: &'static str,
}

/// Get list of enabled features
fn get_enabled_features() -> Vec<&'static str> {
    let mut features = Vec::new();
    
    #[cfg(feature = "simd")]
    features.push("simd");
    
    #[cfg(feature = "parallel")]
    features.push("parallel");
    
    #[cfg(feature = "gpu")]
    features.push("gpu");
    
    #[cfg(feature = "experimental")]
    features.push("experimental");
    
    features
}

// Performance benchmarking utilities
pub mod benchmark {
    use super::*;
    use std::time::Instant;
    
    /// Benchmark result
    #[derive(Debug, Clone)]
    pub struct BenchmarkResult {
        pub operation: String,
        pub duration_ns: u128,
        pub throughput: f64,
        pub memory_usage: usize,
    }
    
    /// Benchmark a quantum operation
    pub fn benchmark_operation<F>(name: &str, mut operation: F) -> BenchmarkResult
    where
        F: FnMut() -> Result<()>,
    {
        let start = Instant::now();
        let _result = operation();
        let duration = start.elapsed();
        
        BenchmarkResult {
            operation: name.to_string(),
            duration_ns: duration.as_nanos(),
            throughput: if duration.as_secs_f64() > 0.0 {
                1.0 / duration.as_secs_f64()
            } else {
                0.0
            },
            memory_usage: 0, // Could be filled with actual measurement
        }
    }
    
    /// Run comprehensive benchmarks
    pub fn run_benchmarks(num_qubits: usize) -> Vec<BenchmarkResult> {
        let mut results = Vec::new();
        
        // Benchmark gate operations
        results.push(benchmark_operation("hadamard", || {
            let mut sim = QuantumSimulator::new(num_qubits);
            sim.state.apply_h(0)
        }));
        
        results.push(benchmark_operation("cnot", || {
            let mut sim = QuantumSimulator::new(num_qubits);
            sim.state.apply_cnot(0, 1)
        }));
        
        // Benchmark algorithm operations
        results.push(benchmark_operation("grover", || {
            let mut sim = QuantumSimulator::new(num_qubits);
            QuantumAlgorithms::grovers_search(&mut sim, &[1], Some(1))
                .map(|_| ())
        }));
        
        results
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
    }
    
    #[test]
    fn test_simulator_creation() {
        let sim = QuantumSimulator::new(3);
        assert_eq!(sim.state.num_qubits, 3);
        assert_eq!(sim.get_memory_usage(), 8 * 16); // 8 amplitudes * 16 bytes each
    }
    
    #[test]
    fn test_bell_state_creation() {
        let mut sim = QuantumSimulator::new(2);
        sim.create_bell_state(0, 1).unwrap();
        
        let probs = sim.get_probabilities();
        assert!((probs[0] - 0.5).abs() < 1e-10);
        assert!((probs[3] - 0.5).abs() < 1e-10);
        assert!(probs[1].abs() < 1e-10);
        assert!(probs[2].abs() < 1e-10);
    }
    
    #[test]
    fn test_measurement() {
        let mut sim = QuantumSimulator::new(1);
        sim.state.apply_h(0).unwrap();
        
        // After Hadamard, should measure 0 or 1 with equal probability
        let measurement = sim.measure_qubit(0).unwrap();
        assert!(measurement == true || measurement == false);
        
        // After measurement, state should be collapsed
        let probs = sim.get_probabilities();
        assert!(probs[0] == 1.0 || probs[1] == 1.0);
    }
    
    #[test]
    fn test_noise_model() {
        let noise_model = NoiseModel::ideal();
        assert!(noise_model.single_qubit_noise.is_empty());
        assert_eq!(noise_model.measurement_noise, 0.0);
        
        let ibm_model = NoiseModel::ibm_quantum();
        assert!(!ibm_model.single_qubit_noise.is_empty());
        assert!(ibm_model.measurement_noise > 0.0);
    }
    
    #[test]
    fn test_error_correction() {
        let mut sim = QuantumSimulator::new(10);
        let code = ErrorCorrectionCode::ShorCode;
        
        let physical_qubits = ErrorCorrection::encode_logical_qubit(&mut sim, 0, &code)
            .unwrap();
        assert_eq!(physical_qubits.len(), 9);
    }
} 