# HaskQ Installation Guide

This guide explains how to set up and install the HaskQ quantum programming toolkit.

## Prerequisites

Before installing HaskQ, ensure you have the following tools installed:

### For All Components

- Git
- Node.js (v18 or later)
- npm (v9 or later)

### For Haskell Packages

- GHC (Glasgow Haskell Compiler) version 9.2 or later
- Cabal 3.6 or later
- Stack (optional)

## Installation Steps

### 1. Clone the Repository

```bash
git clone https://github.com/ArsCodeAmatoria/HaskQ.git
cd HaskQ
```

### 2. Install JavaScript Dependencies

```bash
npm install
```

### 3. Build the Haskell Packages

#### Using Cabal

```bash
# Navigate to the core package
cd packages/haskq-core
cabal build

# Navigate to the simulator package
cd ../haskq-simulator
cabal build
```

#### Using Stack (alternative)

If you prefer using Stack, you can add a `stack.yaml` file to the root of the repository and build all packages at once:

```bash
stack build
```

### 4. Run the Development Server

To start the development servers for the web applications:

```bash
# From the root directory
npm run dev
```

This will start the following servers:
- Landing page: http://localhost:3000
- Documentation: http://localhost:3001
- Playground: http://localhost:3002

## Building for Production

To build all packages and applications for production:

```bash
npm run build
```

## Running the Quantum Simulator

After building the `haskq-simulator` package, you can run it:

```bash
cd packages/haskq-simulator
cabal run haskq-sim -- --circuit bell --output ascii
```

Options:
- `--circuit` or `-c`: Circuit to simulate (bell, ghz, teleport, deutsch)
- `--qubits` or `-q`: Number of qubits (default: 2)
- `--output` or `-o`: Output format (ascii or json, default: ascii)
- `--file` or `-f`: Output file (optional, defaults to stdout)

## Troubleshooting

### Common Issues

- **Linear Types Extension**: Ensure you're using GHC 9.2 or later which has the `LinearTypes` extension enabled.
- **Node.js Dependencies**: If you encounter errors with Node.js dependencies, try clearing the cache with `npm cache clean --force` and reinstalling with `npm install`.

### Get Help

If you encounter issues not covered in this guide, please:
1. Check the [Documentation](https://docs.haskq.org)
2. Open an issue on [GitHub](https://github.com/ArsCodeAmatoria/HaskQ/issues) 