#ifndef HASKQ_CORE_H
#define HASKQ_CORE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Type definitions
typedef uint32_t haskq_simulator_id_t;

typedef struct {
    double real;
    double imag;
} haskq_complex_t;

typedef struct {
    bool success;
    char* error_message;
    void* data;
    size_t data_len;
} haskq_result_t;

// Simulator management
haskq_simulator_id_t haskq_create_simulator(int num_qubits);
bool haskq_destroy_simulator(haskq_simulator_id_t sim_id);
bool haskq_reset_simulator(haskq_simulator_id_t sim_id);

// Single-qubit gates
bool haskq_apply_i(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_x(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_y(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_z(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_h(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_s(haskq_simulator_id_t sim_id, int target);
bool haskq_apply_t(haskq_simulator_id_t sim_id, int target);

// Rotation gates
bool haskq_apply_rx(haskq_simulator_id_t sim_id, double angle, int target);
bool haskq_apply_ry(haskq_simulator_id_t sim_id, double angle, int target);
bool haskq_apply_rz(haskq_simulator_id_t sim_id, double angle, int target);
bool haskq_apply_phase(haskq_simulator_id_t sim_id, double angle, int target);

// Two-qubit gates
bool haskq_apply_cnot(haskq_simulator_id_t sim_id, int control, int target);
bool haskq_apply_cz(haskq_simulator_id_t sim_id, int control, int target);
bool haskq_apply_swap(haskq_simulator_id_t sim_id, int qubit1, int qubit2);

// Three-qubit gates
bool haskq_apply_toffoli(haskq_simulator_id_t sim_id, int control1, int control2, int target);

// State access
int haskq_get_num_qubits(haskq_simulator_id_t sim_id);
size_t haskq_get_state_size(haskq_simulator_id_t sim_id);
bool haskq_get_amplitudes(haskq_simulator_id_t sim_id, haskq_complex_t* amplitudes, size_t len);
bool haskq_get_probabilities(haskq_simulator_id_t sim_id, double* probabilities, size_t len);
bool haskq_measure_all(haskq_simulator_id_t sim_id, bool* results, size_t len);

// Circuit processing
bool haskq_apply_circuit_json(haskq_simulator_id_t sim_id, const char* json_str);

// Utility functions
char* haskq_get_version(void);
void haskq_free_string(char* s);

#ifdef __cplusplus
}
#endif

#endif // HASKQ_CORE_H 