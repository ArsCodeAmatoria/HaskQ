---
sidebar_position: 3
---

# Installation

This guide covers how to install and set up HaskQ on your system.

## Prerequisites

Before installing HaskQ, ensure you have the following tools installed:

### For All Components

- Git
- Node.js (v18 or later)
- npm (v9 or later)

### For Haskell Packages

- GHC (Glasgow Haskell Compiler) version 9.2 or later with LinearTypes extension
- Cabal 3.6 or later
- Stack (optional)

## Installation Methods

### Method 1: From Source (Recommended for Development)

Clone the repository and build from source:

```bash
# Clone the repository
git clone https://github.com/ArsCodeAmatoria/HaskQ.git
cd HaskQ

# Install dependencies
npm install

# Build all packages
npm run build
```

### Method 2: Using Cabal (For Haskell Packages Only)

```bash
# Install core package
cabal install haskq-core

# Install simulator package
cabal install haskq-simulator
```

### Method 3: Using Stack (For Haskell Packages Only)

Create a `stack.yaml` file with the following content:

```yaml
resolver: lts-20.11  # or a more recent resolver
packages:
- packages/haskq-core
- packages/haskq-simulator
extra-deps:
- linear-base-0.3.0
```

Then run:

```bash
stack build
```

## Verifying Installation

After installation, verify that everything is working correctly:

```bash
# Run the simulator CLI
cabal run haskq-sim -- --circuit bell --output ascii

# Start the development servers
npm run dev
```

You should see:
- The simulator outputs a Bell state circuit representation
- The web applications start on their respective ports

## Directory Structure

After installation, your project structure should look like this:

```
HaskQ/
├── apps/
│   ├── landing/     # Landing page (Next.js)
│   ├── docs/        # Documentation (Docusaurus)
│   └── playground/  # Interactive playground (Next.js)
├── packages/
│   ├── haskq-core/      # Core quantum DSL
│   └── haskq-simulator/ # Quantum simulator
└── ... (root configuration files)
```

## Common Issues

### Missing LinearTypes Extension

If you encounter an error about the `LinearTypes` extension, ensure you're using GHC 9.2 or later:

```bash
ghc --version
```

If you have an older version, update your GHC installation.

### Dependency Issues

If you have dependency issues, you might need to update your package index:

```bash
cabal update
```

## Next Steps

Now that you have HaskQ installed, proceed to the [Getting Started](getting-started.md) guide to create your first quantum circuit. 