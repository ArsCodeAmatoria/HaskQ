---
sidebar_position: 1
---

# Quantum Computing Basics

Before diving into HaskQ, let's review the fundamental concepts of quantum computing.

## Qubits

While classical bits can be either 0 or 1, a quantum bit or **qubit** can exist in a superposition of both states:

$$|\psi\rangle = \alpha|0\rangle + \beta|1\rangle$$

where $\alpha$ and $\beta$ are complex numbers with $|\alpha|^2 + |\beta|^2 = 1$.

In HaskQ, qubits are represented by the `Qubit` type, which is handled linearly to prevent violations of the no-cloning theorem.

## Quantum Gates

Quantum gates are unitary operations that manipulate qubits. Some fundamental gates include:

### Single-Qubit Gates

- **Hadamard (H)**: Creates superposition
  $$H = \frac{1}{\sqrt{2}}\begin{pmatrix} 1 & 1 \\ 1 & -1 \end{pmatrix}$$

- **Pauli-X**: Quantum equivalent of NOT gate
  $$X = \begin{pmatrix} 0 & 1 \\ 1 & 0 \end{pmatrix}$$

- **Pauli-Y**:
  $$Y = \begin{pmatrix} 0 & -i \\ i & 0 \end{pmatrix}$$

- **Pauli-Z**: Phase flip
  $$Z = \begin{pmatrix} 1 & 0 \\ 0 & -1 \end{pmatrix}$$

### Multi-Qubit Gates

- **CNOT** (Controlled-NOT): Flips the target qubit if the control qubit is $|1\rangle$
  $$CNOT = \begin{pmatrix} 1 & 0 & 0 & 0 \\ 0 & 1 & 0 & 0 \\ 0 & 0 & 0 & 1 \\ 0 & 0 & 1 & 0 \end{pmatrix}$$

- **SWAP**: Exchanges the states of two qubits
  $$SWAP = \begin{pmatrix} 1 & 0 & 0 & 0 \\ 0 & 0 & 1 & 0 \\ 0 & 1 & 0 & 0 \\ 0 & 0 & 0 & 1 \end{pmatrix}$$

## Measurement

When we measure a qubit, its superposition collapses to a classical state. A qubit in state $\alpha|0\rangle + \beta|1\rangle$ will collapse to:
- $|0\rangle$ with probability $|\alpha|^2$
- $|1\rangle$ with probability $|\beta|^2$

In HaskQ, measurement is represented by the `measure` function, which returns a `Measurement` result along with the post-measurement qubit.

## Quantum Circuit Model

Quantum algorithms are typically expressed as quantum circuits, consisting of:
1. Initialization of qubits (usually to $|0\rangle$)
2. Application of quantum gates
3. Measurement of qubits

HaskQ uses the `Circ` monad to represent quantum circuits as first-class values that can be composed and manipulated.

## Entanglement

Entanglement is a quantum phenomenon where qubits become correlated in such a way that the state of one qubit cannot be described independently of the state of the others.

A Bell state is a simple example of a maximally entangled state:

$$|\Phi^+\rangle = \frac{1}{\sqrt{2}}(|00\rangle + |11\rangle)$$

In HaskQ, you can create entangled states using controlled operations like CNOT after creating superpositions with gates like Hadamard.

## No-Cloning Theorem

The no-cloning theorem is a fundamental principle of quantum mechanics that states it is impossible to create an identical copy of an arbitrary unknown quantum state.

HaskQ enforces this theorem at the type level using linear types, making it impossible to duplicate a qubit in your code. 