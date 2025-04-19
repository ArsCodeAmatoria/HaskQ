---
sidebar_position: 1
---

# Quantum Computing Basics

This document provides a brief introduction to the fundamental concepts of quantum computing.

## Qubits

Unlike classical bits, which can be either 0 or 1, quantum bits (qubits) can exist in a superposition of both states:

$$|\psi\rangle = \alpha|0\rangle + \beta|1\rangle$$

where $\alpha$ and $\beta$ are complex numbers satisfying $|\alpha|^2 + |\beta|^2 = 1$.

## Quantum Gates

In quantum computing, operations are performed using quantum gates, which are unitary operations that act on qubits. Some fundamental gates include:

- **Hadamard (H)**: Creates a superposition
- **Pauli-X**: Quantum equivalent of the NOT gate
- **Pauli-Z**: Phase flip gate
- **CNOT**: Two-qubit controlled-NOT gate

## Measurement

When a qubit is measured, it collapses to either 0 or 1 with probabilities determined by $|\alpha|^2$ and $|\beta|^2$. This collapse is irreversible.

## Entanglement

Entanglement is a quantum phenomenon where the states of multiple qubits become correlated in such a way that the state of one qubit cannot be described independently of the others.

## Quantum Algorithms

Quantum algorithms leverage superposition, entanglement, and interference to solve certain problems more efficiently than classical algorithms:

- **Deutsch-Jozsa**: Determines if a function is constant or balanced
- **Grover's Search**: Searches an unsorted database in $O(\sqrt{N})$ time
- **Shor's Algorithm**: Factors integers in polynomial time

## No-Cloning Theorem

The no-cloning theorem states that it is impossible to create an identical copy of an unknown quantum state. This theorem has profound implications for quantum computing and is enforced in HaskQ through linear types.

## Quantum Circuits

Quantum algorithms are typically expressed as quantum circuits, which are sequences of quantum gates applied to qubits. In HaskQ, circuits are expressed through the `Circ` monad, which provides a clean and type-safe way to compose quantum operations.

## Further Reading

To deepen your understanding of quantum computing, consider exploring:

- [Nielsen and Chuang's "Quantum Computation and Quantum Information"](https://www.cambridge.org/core/books/quantum-computation-and-quantum-information/01E10196D0A682A6AEFFEA52D53BE9AE)
- [Quantum Country](https://quantum.country/)
- [Qiskit Textbook](https://qiskit.org/textbook) 