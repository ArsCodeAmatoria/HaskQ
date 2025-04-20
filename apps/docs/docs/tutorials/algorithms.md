---
sidebar_position: 2
---

# Quantum Algorithms in HaskQ

This page provides an overview of quantum algorithms available in HaskQ and links to more detailed tutorials for each algorithm.

## Core Quantum Algorithms

HaskQ provides implementations of several fundamental quantum algorithms:

| Algorithm | Description | Tutorial |
|-----------|-------------|----------|
| **Grover's Algorithm** | Search algorithm that provides quadratic speedup for finding elements in an unstructured database | [Grover's Algorithm Tutorial](./grover) |
| **Quantum Fourier Transform** | Quantum version of the discrete Fourier transform, a key component in many quantum algorithms | [QFT Tutorial](./qft) |
| **Quantum Phase Estimation** | Estimates eigenvalues of a unitary operator | [Advanced Algorithms](./advanced-algorithms#quantum-phase-estimation) |
| **Shor's Algorithm** | Integer factorization algorithm with exponential speedup | [Advanced Algorithms](./advanced-algorithms#shors-algorithm) |

## Variational Algorithms

HaskQ is also expanding to support hybrid quantum-classical variational algorithms:

| Algorithm | Description | Tutorial |
|-----------|-------------|----------|
| **QAOA** | Quantum Approximate Optimization Algorithm for combinatorial optimization | [Advanced Algorithms](./advanced-algorithms#quantum-approximate-optimization-algorithm-qaoa) |
| **VQE** | Variational Quantum Eigensolver for finding ground state energies | [Advanced Algorithms](./advanced-algorithms#variational-quantum-eigensolver-vqe) |
| **VQLS** | Variational Quantum Linear Solver | [Hybrid Algorithms](./hybrid-algorithms) |

## Getting Started with Algorithms

To get started with quantum algorithms in HaskQ:

1. Make sure you understand the [core concepts](../core-concepts/quantum-computing-basics) of quantum computing
2. Explore the [Bell States tutorial](./bell-states) to understand quantum entanglement
3. Dive into the [Grover's Algorithm tutorial](./grover) for a practical example
4. Learn about the [Quantum Fourier Transform](./qft), the building block for many other algorithms

## Running Algorithm Examples

HaskQ includes executable examples for several algorithms. You can run them with:

```bash
# Run Grover's algorithm example with 3 qubits
cabal run haskq-grover -- 3

# Run QFT example with 4 qubits
cabal run haskq-qft -- 4
```

## Implementing Your Own Algorithms

HaskQ's linear type system and quantum circuit abstraction make it an excellent platform for implementing your own quantum algorithms. The tutorials above provide examples you can build upon.

For advanced algorithm implementation, consider exploring:
- [Linear types in HaskQ](../core-concepts/linear-types)
- [Circuit composition](../core-concepts/circuit-composition)
- [Simulation](../core-concepts/simulation) for testing your algorithms 