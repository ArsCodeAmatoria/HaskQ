---
sidebar_position: 4
---

# Project Structure

Understanding the structure of the HaskQ project will help you navigate the codebase and contribute effectively.

## Overview

HaskQ is organized as a monorepo using Turborepo, containing both Haskell packages and JavaScript applications. Here's a high-level view of the project structure:

```
HaskQ/
├── apps/                  # Web applications
├── packages/              # Haskell libraries
├── package.json           # Root package.json for JS dependencies
├── turbo.json             # Turborepo configuration
└── README.md              # Project documentation
```

## Haskell Packages

The `packages/` directory contains the Haskell libraries that form the core of HaskQ:

```
packages/
├── haskq-core/            # Core quantum DSL
│   ├── src/
│   │   └── HaskQ/
│   │       └── Core/
│   │           ├── Types.hs        # Fundamental types (Qubit, Gate, etc.)
│   │           ├── Gates.hs        # Quantum gate implementations
│   │           ├── Circuit.hs      # Circuit composition utilities
│   │           ├── Measurement.hs  # Measurement operations
│   │           └── Examples.hs     # Example circuits
│   ├── haskq-core.cabal   # Cabal package configuration
│   └── LICENSE            # License file
└── haskq-simulator/       # Quantum simulator
    ├── src/
    │   └── HaskQ/
    │       └── Simulator/
    │           ├── StateVector.hs  # State vector representation
    │           ├── Gates.hs        # Matrix implementations of gates
    │           ├── Circuit.hs      # Circuit simulation engine
    │           └── Visualizer.hs   # Circuit visualization tools
    ├── app/
    │   └── Main.hs        # Command-line simulator application
    ├── haskq-simulator.cabal
    └── LICENSE
```

### Core Design Principles

The Haskell packages follow these design principles:

1. **Type Safety**: Linear types enforce quantum mechanics principles at compile time
2. **Composition**: Monadic design allows for clean circuit composition
3. **Separation of Concerns**: Clear separation between the DSL and simulation implementation
4. **Extensibility**: Designed to be extended with new gates and algorithms

## Web Applications

The `apps/` directory contains the web applications that provide user interfaces to HaskQ:

```
apps/
├── landing/              # Landing page
│   ├── app/              # Next.js app directory
│   ├── public/           # Static assets
│   ├── package.json
│   └── tailwind.config.js
├── docs/                 # Documentation site
│   ├── docs/             # Markdown documentation
│   │   ├── intro.md
│   │   ├── getting-started.md
│   │   ├── core-concepts/
│   │   └── tutorials/
│   ├── src/
│   ├── docusaurus.config.js
│   └── package.json
└── playground/           # Interactive playground
    ├── app/              # Next.js app directory
    ├── public/           # Static assets
    ├── package.json
    └── tailwind.config.js
```

## Build System

HaskQ uses a combination of build systems:

1. **Turborepo**: Orchestrates the build process for the entire monorepo
2. **Cabal/Stack**: Builds the Haskell packages
3. **Next.js/Docusaurus**: Builds the web applications

The build process is defined in the root `turbo.json` file and the scripts in the root `package.json`.

## Development Workflow

When developing HaskQ, you'll typically:

1. Make changes to the Haskell packages in `packages/`
2. Build the packages using Cabal or Stack
3. Run the simulator to test your changes
4. Update documentation in `apps/docs/docs/`
5. Run the web applications to test the UI

## Configuration Files

Key configuration files in the project:

- `turbo.json`: Defines the build pipeline for the monorepo
- `package.json`: Root package configuration with workspace setup
- `*.cabal`: Haskell package configuration
- `docusaurus.config.js`: Documentation site configuration
- `tailwind.config.js`: UI styling configuration for web apps

## Next Steps

Now that you understand the project structure, you can:

1. Explore the [API Reference](category/api) to learn about specific modules
2. Check out the [Tutorials](category/tutorials) to see how to use HaskQ
3. Start [Contributing](contributing) to the project 