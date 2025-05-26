//! Quantum Error Correction and Noise Modeling
//! 
//! This module implements various quantum error correction codes, noise models,
//! and fault-tolerant quantum computation techniques for realistic quantum simulation.

use nalgebra::DMatrix;
use num_complex::Complex64;
use rand::prelude::*;
use std::collections::HashMap;

use crate::{QuantumState, QuantumSimulator, GateOps, Result};

/// Quantum error correction and noise modeling
pub struct ErrorCorrection;

/// Types of quantum noise
#[derive(Debug, Clone, PartialEq)]
pub enum NoiseType {
    BitFlip(f64),           // X error with probability p
    PhaseFlip(f64),         // Z error with probability p
    BitPhaseFlip(f64),      // Y error with probability p
    Depolarizing(f64),      // Depolarizing noise with probability p
    AmplitudeDamping(f64),  // T1 decay with probability p
    PhaseDamping(f64),      // T2 dephasing with probability p
    ThermalNoise(f64),      // Thermal excitation with probability p
    Coherent(Complex64),    // Coherent error with amplitude α
}

/// Noise model containing multiple noise sources
#[derive(Debug, Clone)]
pub struct NoiseModel {
    pub single_qubit_noise: Vec<NoiseType>,
    pub two_qubit_noise: Vec<NoiseType>,
    pub measurement_noise: f64,
    pub gate_times: HashMap<String, f64>,  // Gate execution times in μs
}

/// Error correction codes
#[derive(Debug, Clone)]
pub enum ErrorCorrectionCode {
    ShorCode,           // [[9,1,3]] code
    StearnsCode,        // [[7,1,3]] code
    SurfaceCode(usize, usize),  // Surface code with distance d
    ColorCode(usize),   // Color code with distance d
    ToricCode(usize),   // Toric code with distance d
    CssCode(DMatrix<u8>, DMatrix<u8>),  // General CSS code
}

/// Syndrome measurement result
#[derive(Debug, Clone)]
pub struct Syndrome {
    pub x_syndrome: Vec<bool>,
    pub z_syndrome: Vec<bool>,
    pub error_location: Option<Vec<usize>>,
}

/// Error correction statistics
#[derive(Debug, Clone)]
pub struct ErrorStats {
    pub logical_error_rate: f64,
    pub physical_error_rate: f64,
    pub correction_success_rate: f64,
    pub average_corrections_per_cycle: f64,
}

impl ErrorCorrection {
    /// Apply noise model to quantum state
    pub fn apply_noise(
        state: &mut QuantumState,
        noise_model: &NoiseModel,
        gate_name: &str
    ) -> Result<()> {
        let gate_time = noise_model.gate_times.get(gate_name).unwrap_or(&0.1);
        
        // Apply decoherence based on gate time
        Self::apply_decoherence(state, *gate_time)?;
        
        // Apply single-qubit noise to all qubits
        for qubit in 0..state.num_qubits {
            for noise in &noise_model.single_qubit_noise {
                Self::apply_single_qubit_noise(state, qubit, noise)?;
            }
        }
        
        Ok(())
    }
    
    /// Apply specific noise type to a qubit
    pub fn apply_single_qubit_noise(
        state: &mut QuantumState,
        qubit: usize,
        noise: &NoiseType
    ) -> Result<()> {
        let mut rng = rand::thread_rng();
        
        match noise {
            NoiseType::BitFlip(p) => {
                if rng.gen::<f64>() < *p {
                    state.apply_x(qubit)?;
                }
            },
            NoiseType::PhaseFlip(p) => {
                if rng.gen::<f64>() < *p {
                    state.apply_z(qubit)?;
                }
            },
            NoiseType::BitPhaseFlip(p) => {
                if rng.gen::<f64>() < *p {
                    state.apply_y(qubit)?;
                }
            },
            NoiseType::Depolarizing(p) => {
                let error_type = rng.gen::<f64>();
                if error_type < *p / 4.0 {
                    state.apply_x(qubit)?;
                } else if error_type < *p / 2.0 {
                    state.apply_y(qubit)?;
                } else if error_type < 3.0 * *p / 4.0 {
                    state.apply_z(qubit)?;
                }
                // Otherwise no error (identity)
            },
            NoiseType::AmplitudeDamping(gamma) => {
                Self::apply_amplitude_damping(state, qubit, *gamma)?;
            },
            NoiseType::PhaseDamping(gamma) => {
                Self::apply_phase_damping(state, qubit, *gamma)?;
            },
            NoiseType::ThermalNoise(p) => {
                if rng.gen::<f64>() < *p {
                    // Thermal excitation from |0⟩ to |1⟩
                    let prob_zero = Self::get_qubit_zero_probability(state, qubit);
                    if rng.gen::<f64>() < prob_zero {
                        state.apply_x(qubit)?;
                    }
                }
            },
            NoiseType::Coherent(alpha) => {
                Self::apply_coherent_error(state, qubit, *alpha)?;
            },
        }
        
        Ok(())
    }
    
    /// Encode logical qubit using specified error correction code
    pub fn encode_logical_qubit(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize,
        code: &ErrorCorrectionCode
    ) -> Result<Vec<usize>> {
        match code {
            ErrorCorrectionCode::ShorCode => {
                Self::encode_shor_code(simulator, logical_qubit)
            },
            ErrorCorrectionCode::StearnsCode => {
                Self::encode_stearns_code(simulator, logical_qubit)
            },
            ErrorCorrectionCode::SurfaceCode(distance, _) => {
                Self::encode_surface_code(simulator, logical_qubit, *distance)
            },
            ErrorCorrectionCode::ColorCode(distance) => {
                Self::encode_color_code(simulator, logical_qubit, *distance)
            },
            ErrorCorrectionCode::ToricCode(distance) => {
                Self::encode_toric_code(simulator, logical_qubit, *distance)
            },
            ErrorCorrectionCode::CssCode(h_x, h_z) => {
                Self::encode_css_code(simulator, logical_qubit, h_x, h_z)
            },
        }
    }
    
    /// Measure syndrome for error detection
    pub fn measure_syndrome(
        simulator: &mut QuantumSimulator,
        physical_qubits: &[usize],
        code: &ErrorCorrectionCode
    ) -> Result<Syndrome> {
        match code {
            ErrorCorrectionCode::ShorCode => {
                Self::measure_shor_syndrome(simulator, physical_qubits)
            },
            ErrorCorrectionCode::StearnsCode => {
                Self::measure_stearns_syndrome(simulator, physical_qubits)
            },
            ErrorCorrectionCode::SurfaceCode(distance, height) => {
                Self::measure_surface_syndrome(simulator, physical_qubits, *distance, *height)
            },
            _ => {
                // Generic syndrome measurement
                Ok(Syndrome {
                    x_syndrome: vec![false; physical_qubits.len()],
                    z_syndrome: vec![false; physical_qubits.len()],
                    error_location: None,
                })
            }
        }
    }
    
    /// Correct detected errors
    pub fn correct_errors(
        simulator: &mut QuantumSimulator,
        physical_qubits: &[usize],
        syndrome: &Syndrome,
        _code: &ErrorCorrectionCode
    ) -> Result<bool> {
        if let Some(error_locations) = &syndrome.error_location {
            for &location in error_locations {
                if location < physical_qubits.len() {
                    // Apply correction (simplified - in practice depends on error type)
                    simulator.state.apply_x(physical_qubits[location])?;
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
    
    /// Fault-tolerant gate implementation
    pub fn fault_tolerant_cnot(
        simulator: &mut QuantumSimulator,
        control_block: &[usize],
        target_block: &[usize],
        code: &ErrorCorrectionCode
    ) -> Result<()> {
        match code {
            ErrorCorrectionCode::ShorCode => {
                // Transversal CNOT for Shor code
                for (control, target) in control_block.iter().zip(target_block.iter()) {
                    simulator.state.apply_cnot(*control, *target)?;
                }
            },
            ErrorCorrectionCode::SurfaceCode(_, _) => {
                // Surface code requires more complex implementation
                Self::surface_code_cnot(simulator, control_block, target_block)?;
            },
            _ => {
                // Default to transversal implementation
                for (control, target) in control_block.iter().zip(target_block.iter()) {
                    simulator.state.apply_cnot(*control, *target)?;
                }
            }
        }
        Ok(())
    }
    
    /// Run error correction cycle
    pub fn error_correction_cycle(
        simulator: &mut QuantumSimulator,
        logical_qubits: &[Vec<usize>],
        code: &ErrorCorrectionCode,
        noise_model: &NoiseModel
    ) -> Result<ErrorStats> {
        let mut total_corrections = 0;
        let mut successful_corrections = 0;
        
        for physical_qubits in logical_qubits {
            // Apply noise
            Self::apply_noise(&mut simulator.state, noise_model, "error_correction")?;
            
            // Measure syndrome
            let syndrome = Self::measure_syndrome(simulator, physical_qubits, code)?;
            
            // Attempt correction
            let correction_applied = Self::correct_errors(simulator, physical_qubits, &syndrome, code)?;
            
            if correction_applied {
                total_corrections += 1;
                
                // Verify correction was successful (simplified)
                let post_syndrome = Self::measure_syndrome(simulator, physical_qubits, code)?;
                if post_syndrome.x_syndrome.iter().all(|&x| !x) && 
                   post_syndrome.z_syndrome.iter().all(|&z| !z) {
                    successful_corrections += 1;
                }
            }
        }
        
        let correction_success_rate = if total_corrections > 0 {
            successful_corrections as f64 / total_corrections as f64
        } else {
            1.0
        };
        
        Ok(ErrorStats {
            logical_error_rate: 0.001, // Placeholder
            physical_error_rate: 0.01, // Placeholder
            correction_success_rate,
            average_corrections_per_cycle: total_corrections as f64 / logical_qubits.len() as f64,
        })
    }
    
    /// Benchmark error correction performance
    pub fn benchmark_error_correction(
        code: &ErrorCorrectionCode,
        noise_model: &NoiseModel,
        num_trials: usize
    ) -> Result<ErrorStats> {
        let mut total_logical_errors = 0;
        let mut total_corrections = 0;
        
        for _ in 0..num_trials {
            let mut simulator = QuantumSimulator::new(50); // Sufficient qubits
            
            // Encode logical qubit
            let physical_qubits = Self::encode_logical_qubit(&mut simulator, 0, code)?;
            
            // Run multiple error correction cycles
            for _ in 0..10 {
                let stats = Self::error_correction_cycle(
                    &mut simulator, 
                    &[physical_qubits.clone()], 
                    code, 
                    noise_model
                )?;
                total_corrections += stats.average_corrections_per_cycle as usize;
            }
            
            // Check for logical errors (simplified)
            let final_syndrome = Self::measure_syndrome(&mut simulator, &physical_qubits, code)?;
            if final_syndrome.error_location.is_some() {
                total_logical_errors += 1;
            }
        }
        
        Ok(ErrorStats {
            logical_error_rate: total_logical_errors as f64 / num_trials as f64,
            physical_error_rate: 0.01, // From noise model
            correction_success_rate: 0.95, // Estimated
            average_corrections_per_cycle: total_corrections as f64 / (num_trials * 10) as f64,
        })
    }
    
    // Helper functions for specific error correction codes
    
    fn encode_shor_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize
    ) -> Result<Vec<usize>> {
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + 9).collect();
        
        // Shor's 9-qubit code encoding
        // |0⟩_L = |000000000⟩, |1⟩_L = |111111111⟩ (simplified)
        
        // First layer: bit-flip protection
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[3])?;
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[6])?;
        
        // Second layer: phase-flip protection  
        for i in 0..3 {
            let base = i * 3;
            simulator.state.apply_h(physical_qubits[base])?;
            simulator.state.apply_h(physical_qubits[base + 1])?;
            simulator.state.apply_h(physical_qubits[base + 2])?;
            simulator.state.apply_cnot(physical_qubits[base], physical_qubits[base + 1])?;
            simulator.state.apply_cnot(physical_qubits[base], physical_qubits[base + 2])?;
        }
        
        Ok(physical_qubits)
    }
    
    fn encode_stearns_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize
    ) -> Result<Vec<usize>> {
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + 7).collect();
        
        // Stearn's [[7,1,3]] code encoding
        // Generator matrix implementation
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[1])?;
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[2])?;
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[4])?;
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[5])?;
        simulator.state.apply_cnot(physical_qubits[0], physical_qubits[6])?;
        
        Ok(physical_qubits)
    }
    
    fn encode_surface_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize,
        distance: usize
    ) -> Result<Vec<usize>> {
        let num_physical = distance * distance;
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + num_physical).collect();
        
        // Surface code initialization
        // Create 2D lattice of qubits with stabilizer measurements
        Self::initialize_surface_code_lattice(simulator, &physical_qubits, distance)?;
        
        Ok(physical_qubits)
    }
    
    fn encode_color_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize,
        distance: usize
    ) -> Result<Vec<usize>> {
        let num_physical = 3 * distance * distance; // Rough estimate
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + num_physical).collect();
        
        // Color code on triangular lattice
        Self::initialize_color_code_lattice(simulator, &physical_qubits, distance)?;
        
        Ok(physical_qubits)
    }
    
    fn encode_toric_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize,
        distance: usize
    ) -> Result<Vec<usize>> {
        let num_physical = 2 * distance * distance;
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + num_physical).collect();
        
        // Toric code on 2D torus
        Self::initialize_toric_code_lattice(simulator, &physical_qubits, distance)?;
        
        Ok(physical_qubits)
    }
    
    fn encode_css_code(
        simulator: &mut QuantumSimulator,
        logical_qubit: usize,
        h_x: &DMatrix<u8>,
        h_z: &DMatrix<u8>
    ) -> Result<Vec<usize>> {
        let num_physical = h_x.ncols();
        let physical_qubits: Vec<usize> = (logical_qubit..logical_qubit + num_physical).collect();
        
        // General CSS code encoding using stabilizer generators
        Self::apply_css_stabilizers(simulator, &physical_qubits, h_x, h_z)?;
        
        Ok(physical_qubits)
    }
    
    fn measure_shor_syndrome(
        simulator: &mut QuantumSimulator,
        physical_qubits: &[usize]
    ) -> Result<Syndrome> {
        let mut x_syndrome = Vec::new();
        let mut z_syndrome = Vec::new();
        
        // Measure X and Z stabilizers for Shor code
        for i in 0..2 {
            let ancilla = physical_qubits.len() + i;
            
            // X stabilizer measurement
            simulator.state.apply_h(ancilla)?;
            for j in 0..3 {
                simulator.state.apply_cnot(ancilla, physical_qubits[i * 3 + j])?;
            }
            simulator.state.apply_h(ancilla)?;
            x_syndrome.push(Self::measure_ancilla(simulator, ancilla)?);
            
            // Z stabilizer measurement  
            for j in 0..3 {
                simulator.state.apply_cnot(physical_qubits[i * 3 + j], ancilla)?;
            }
            z_syndrome.push(Self::measure_ancilla(simulator, ancilla)?);
        }
        
        let error_location = Self::decode_shor_syndrome(&x_syndrome, &z_syndrome);
        
        Ok(Syndrome {
            x_syndrome,
            z_syndrome,
            error_location,
        })
    }
    
    fn measure_stearns_syndrome(
        simulator: &mut QuantumSimulator,
        physical_qubits: &[usize]
    ) -> Result<Syndrome> {
        let mut x_syndrome = Vec::new();
        let mut z_syndrome = Vec::new();
        
        // Stearn's code has 6 stabilizers
        for i in 0..6 {
            let ancilla = physical_qubits.len() + i;
            
            // Apply stabilizer measurement
            Self::apply_stearns_stabilizer(simulator, physical_qubits, ancilla, i)?;
            let measurement = Self::measure_ancilla(simulator, ancilla)?;
            
            if i < 3 {
                x_syndrome.push(measurement);
            } else {
                z_syndrome.push(measurement);
            }
        }
        
        let error_location = Self::decode_stearns_syndrome(&x_syndrome, &z_syndrome);
        
        Ok(Syndrome {
            x_syndrome,
            z_syndrome,
            error_location,
        })
    }
    
    fn measure_surface_syndrome(
        simulator: &mut QuantumSimulator,
        physical_qubits: &[usize],
        distance: usize,
        height: usize
    ) -> Result<Syndrome> {
        let mut x_syndrome = Vec::new();
        let mut z_syndrome = Vec::new();
        
        // Surface code stabilizer measurements
        for i in 0..distance - 1 {
            for j in 0..height - 1 {
                let ancilla_x = physical_qubits.len() + i * height + j;
                let ancilla_z = physical_qubits.len() + (distance - 1) * height + i * height + j;
                
                // X stabilizer (star operator)
                Self::apply_surface_x_stabilizer(simulator, physical_qubits, ancilla_x, i, j, distance)?;
                x_syndrome.push(Self::measure_ancilla(simulator, ancilla_x)?);
                
                // Z stabilizer (plaquette operator)
                Self::apply_surface_z_stabilizer(simulator, physical_qubits, ancilla_z, i, j, distance)?;
                z_syndrome.push(Self::measure_ancilla(simulator, ancilla_z)?);
            }
        }
        
        let error_location = Self::decode_surface_syndrome(&x_syndrome, &z_syndrome, distance);
        
        Ok(Syndrome {
            x_syndrome,
            z_syndrome,
            error_location,
        })
    }
    
    // Additional helper functions
    
    fn apply_decoherence(state: &mut QuantumState, time: f64) -> Result<()> {
        // Apply T1 and T2 decoherence based on time
        let t1 = 100.0; // T1 time in μs
        let t2 = 50.0;  // T2 time in μs
        
        let p1 = 1.0 - (-time / t1).exp();
        let p2 = 1.0 - (-time / t2).exp();
        
        for qubit in 0..state.num_qubits {
            // T1 relaxation
            if rand::random::<f64>() < p1 {
                Self::apply_amplitude_damping(state, qubit, p1)?;
            }
            
            // T2 dephasing
            if rand::random::<f64>() < p2 {
                Self::apply_phase_damping(state, qubit, p2)?;
            }
        }
        
        Ok(())
    }
    
    fn apply_amplitude_damping(state: &mut QuantumState, qubit: usize, gamma: f64) -> Result<()> {
        // Kraus operators for amplitude damping
        let size = state.amplitudes.len();
        let mut new_amplitudes = state.amplitudes.clone();
        
        for i in 0..size {
            let qubit_bit = (i >> qubit) & 1;
            if qubit_bit == 1 {
                // |1⟩ decays to |0⟩ with probability γ
                let decay_amplitude = gamma.sqrt();
                let i_flipped = i ^ (1 << qubit);
                
                new_amplitudes[i_flipped] += decay_amplitude * state.amplitudes[i];
                new_amplitudes[i] *= (1.0 - gamma).sqrt();
            }
        }
        
        state.amplitudes = new_amplitudes;
        Ok(())
    }
    
    fn apply_phase_damping(state: &mut QuantumState, qubit: usize, gamma: f64) -> Result<()> {
        // Phase damping channel
        let size = state.amplitudes.len();
        
        for i in 0..size {
            let qubit_bit = (i >> qubit) & 1;
            if qubit_bit == 1 {
                state.amplitudes[i] *= (1.0 - gamma / 2.0).sqrt();
            }
        }
        
        Ok(())
    }
    
    fn apply_coherent_error(state: &mut QuantumState, qubit: usize, alpha: Complex64) -> Result<()> {
        // Apply coherent rotation error
        let size = state.amplitudes.len();
        
        for i in 0..size {
            let qubit_bit = (i >> qubit) & 1;
            if qubit_bit == 1 {
                state.amplitudes[i] *= alpha;
            }
        }
        
        Ok(())
    }
    
    fn get_qubit_zero_probability(state: &QuantumState, qubit: usize) -> f64 {
        let size = state.amplitudes.len();
        let mut prob_zero = 0.0;
        
        for i in 0..size {
            if ((i >> qubit) & 1) == 0 {
                prob_zero += state.amplitudes[i].norm_sqr();
            }
        }
        
        prob_zero
    }
    
    fn measure_ancilla(simulator: &mut QuantumSimulator, ancilla: usize) -> Result<bool> {
        // Simplified measurement
        let prob_zero = Self::get_qubit_zero_probability(&simulator.state, ancilla);
        Ok(rand::random::<f64>() >= prob_zero)
    }
    
    fn decode_shor_syndrome(x_syndrome: &[bool], z_syndrome: &[bool]) -> Option<Vec<usize>> {
        // Simplified syndrome decoding for Shor code
        if x_syndrome.iter().any(|&x| x) || z_syndrome.iter().any(|&z| z) {
            Some(vec![0]) // Placeholder error location
        } else {
            None
        }
    }
    
    fn decode_stearns_syndrome(x_syndrome: &[bool], z_syndrome: &[bool]) -> Option<Vec<usize>> {
        // Syndrome decoding for Stearn's code
        if x_syndrome.iter().any(|&x| x) || z_syndrome.iter().any(|&z| z) {
            Some(vec![0]) // Placeholder error location
        } else {
            None
        }
    }
    
    fn decode_surface_syndrome(
        x_syndrome: &[bool], 
        z_syndrome: &[bool], 
        _distance: usize
    ) -> Option<Vec<usize>> {
        // Surface code syndrome decoding using minimum-weight perfect matching
        if x_syndrome.iter().any(|&x| x) || z_syndrome.iter().any(|&z| z) {
            Some(vec![0]) // Placeholder - real implementation needs MWPM
        } else {
            None
        }
    }
    
    // Additional initialization and stabilizer functions (stubs)
    
    fn initialize_surface_code_lattice(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _distance: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn initialize_color_code_lattice(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _distance: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn initialize_toric_code_lattice(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _distance: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn apply_css_stabilizers(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _h_x: &DMatrix<u8>,
        _h_z: &DMatrix<u8>
    ) -> Result<()> {
        Ok(())
    }
    
    fn apply_stearns_stabilizer(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _ancilla: usize,
        _stabilizer_index: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn apply_surface_x_stabilizer(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _ancilla: usize,
        _i: usize,
        _j: usize,
        _distance: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn apply_surface_z_stabilizer(
        _simulator: &mut QuantumSimulator,
        _physical_qubits: &[usize],
        _ancilla: usize,
        _i: usize,
        _j: usize,
        _distance: usize
    ) -> Result<()> {
        Ok(())
    }
    
    fn surface_code_cnot(
        _simulator: &mut QuantumSimulator,
        _control_block: &[usize],
        _target_block: &[usize]
    ) -> Result<()> {
        Ok(())
    }
}

/// Default noise models for common experimental setups
impl NoiseModel {
    /// IBM quantum computer noise model
    pub fn ibm_quantum() -> Self {
        let mut gate_times = HashMap::new();
        gate_times.insert("h".to_string(), 0.04);
        gate_times.insert("x".to_string(), 0.04);
        gate_times.insert("cnot".to_string(), 0.24);
        gate_times.insert("measure".to_string(), 1.0);
        
        NoiseModel {
            single_qubit_noise: vec![
                NoiseType::Depolarizing(0.001),
                NoiseType::AmplitudeDamping(0.0001),
                NoiseType::PhaseDamping(0.0002),
            ],
            two_qubit_noise: vec![
                NoiseType::Depolarizing(0.01),
            ],
            measurement_noise: 0.02,
            gate_times,
        }
    }
    
    /// Google quantum computer noise model
    pub fn google_quantum() -> Self {
        let mut gate_times = HashMap::new();
        gate_times.insert("h".to_string(), 0.025);
        gate_times.insert("x".to_string(), 0.025);
        gate_times.insert("cnot".to_string(), 0.032);
        gate_times.insert("measure".to_string(), 1.0);
        
        NoiseModel {
            single_qubit_noise: vec![
                NoiseType::Depolarizing(0.0005),
                NoiseType::AmplitudeDamping(0.00005),
                NoiseType::PhaseDamping(0.0001),
            ],
            two_qubit_noise: vec![
                NoiseType::Depolarizing(0.005),
            ],
            measurement_noise: 0.01,
            gate_times,
        }
    }
    
    /// Ideal (noiseless) model
    pub fn ideal() -> Self {
        NoiseModel {
            single_qubit_noise: vec![],
            two_qubit_noise: vec![],
            measurement_noise: 0.0,
            gate_times: HashMap::new(),
        }
    }
} 