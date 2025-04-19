---
sidebar_position: 6
---

# HaskQ Playground

The HaskQ Playground is an interactive environment where you can write, test, and visualize quantum circuits directly in your browser.

## Overview

The playground provides an intuitive interface for experimenting with HaskQ code without needing to install anything locally.

## Features

- Code editor with syntax highlighting
- Real-time circuit visualization
- State vector visualization
- Pre-built example circuits
- Share your circuits via URL

## Getting Started

Visit the [HaskQ Playground](http://localhost:3003/playground) to start experimenting with quantum circuits right away.

## Examples

Here are some simple circuits you can try in the playground:

### Bell State

```haskell
bell :: Circ [Bit]
bell = do
  q1 <- qubit
  q2 <- qubit
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  measure_all [q1'', q2']
```

### Quantum Teleportation

```haskell
teleport :: Circ [Bit]
teleport = do
  -- Coming soon
``` 