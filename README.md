# HaskQ: Quantum Circuits, Purely Functional

HaskQ is a functional quantum programming toolkit that brings together the elegance of Haskell and the power of quantum computing. The project aims to provide a type-safe, purely functional approach to quantum circuit design and simulation.

## Project Structure

- **packages/**
  - **haskq-core** - A Haskell library implementing a functional quantum DSL
  - **haskq-simulator** - A matrix-based quantum simulator written in Haskell

- **apps/**
  - **landing** - Project website built with Next.js
  - **docs** - Documentation site built with Docusaurus
  - **playground** - Interactive quantum circuit builder and simulator

## Core Features

- **Type-Safe Circuit Construction**: Linear types ensure no-cloning principle
- **Functional Composition**: Monad-based circuit composition
- **Simulation**: Matrix-based quantum state simulation
- **Visualization**: Circuit diagrams and state visualization tools

## Getting Started

```bash
# Clone the repository
git clone https://github.com/your-username/haskq.git
cd haskq

# Install dependencies
npm install

# Build all packages and apps
npm run build

# Start the development server for all apps
npm run dev
```

## Philosophy

HaskQ aims to bring the rigor and expressiveness of functional programming to quantum computing. By leveraging Haskell's powerful type system, especially linear types, we enforce quantum mechanical laws at compile time, making it impossible to construct invalid quantum programs.

Our goal is to provide an intuitive and safe interface for quantum programming, suitable for both educational purposes and research experimentation.

## Contributing

Contributions are welcome! Please check out our [contributing guidelines](CONTRIBUTING.md) for details.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details. 