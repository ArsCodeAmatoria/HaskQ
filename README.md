# HaskQ - Quantum Computing Library

A hybrid Haskell + Rust quantum computing library with type-safe circuit composition and high-performance WebAssembly simulation.

## 🚀 Features

- **Type-Safe DSL**: Haskell domain-specific language for quantum circuit composition
- **High-Performance Simulation**: Rust-based quantum simulator compiled to WebAssembly
- **Browser Integration**: Seamless integration with Next.js and modern web frameworks
- **Linear Types**: Compile-time enforcement of quantum no-cloning theorem
- **Rich Algorithm Library**: Built-in implementations of major quantum algorithms

## 📦 Architecture

```
HaskQ Ecosystem
├── packages/haskq-dsl/     # Haskell DSL for circuit composition
├── packages/haskq-sim/     # Rust WASM simulation engine
├── apps/haskq-unified/     # Next.js web interface
└── docs/                   # Documentation and examples
```

### Component Roles

| Component | Language | Purpose |
|-----------|----------|---------|
| **HaskQ DSL** | Haskell | Type-safe circuit definition, symbolic manipulation |
| **HaskQ Sim** | Rust → WASM | High-performance quantum simulation |
| **Web Interface** | TypeScript/Next.js | Interactive playground and documentation |

## 🛠️ Installation

### Prerequisites

- **Haskell**: GHC 9.0+ with Stack or Cabal
- **Rust**: 1.70+ with `wasm-pack` for WebAssembly compilation
- **Node.js**: 18+ for web interface

### Quick Start

1. **Clone the repository**
   ```bash
   git clone https://github.com/HaskQ/haskq-unified.git
   cd haskq-unified
   ```

2. **Build Haskell DSL**
   ```bash
   cd packages/haskq-dsl
   stack build
   stack install
   ```

3. **Build Rust WASM Module**
   ```bash
   cd packages/haskq-sim
   wasm-pack build --target web
   ```

4. **Start Web Interface**
   ```bash
   cd apps/haskq-unified
   npm install
   npm run dev
   ```

## 📚 Usage Examples

### Haskell DSL

```haskell
{-# LANGUAGE OverloadedStrings #-}
import HaskQ.Core

-- Bell state circuit
bellState :: Circuit
bellState = withQubits 2 $ \[q1, q2] -> sequential
  [ hadamard q1
  , cnot q1 q2
  ]

-- Grover's algorithm
groverSearch :: [Int] -> Int -> Circuit
groverSearch marked n = withQubits n $ \qubits -> 
  sequential $ concat $ replicate iterations $
    groverIteration marked qubits
  where
    iterations = floor $ pi / 4 * sqrt (fromIntegral $ 2^n)

-- Export to JSON for WASM
main :: IO ()
main = saveCircuit "bell.json" bellState
```

### TypeScript Integration

```typescript
import { simulateCircuit, circuits } from '@/lib/haskq-wasm'

// Create and simulate Bell state
const bellCircuit = circuits.bellState()
const result = await simulateCircuit(bellCircuit)

console.log('Amplitudes:', result.amplitudes)
console.log('Probabilities:', result.probabilities)

// Custom circuit
const customCircuit = {
  gates: [
    { type: 'H', target: { qubit: 0 } },
    { type: 'CNOT', control: { qubit: 0 }, target: { qubit: 1 } },
    { type: 'RZ', angle: Math.PI/4, target: { qubit: 1 } }
  ]
}

const customResult = await simulateCircuit(customCircuit)
```

### CLI Usage

```bash
# Generate quantum circuits
haskq-cli generate --algorithm bell --qubits 2 --output bell.json
haskq-cli generate --algorithm grover --qubits 3 --output grover.json

# Analyze Haskell files
haskq-cli analyze src/MyQuantumAlgorithm.hs

# Export to different formats
haskq-cli export bell.json --format qasm --output bell.qasm
haskq-cli export grover.json --format latex --output grover.tex
```

## 🧮 Supported Quantum Gates

### Single-Qubit Gates
- **Pauli Gates**: X, Y, Z
- **Hadamard**: H
- **Rotation Gates**: RX(θ), RY(θ), RZ(θ)
- **Phase Gates**: S, T, Phase(θ)

### Multi-Qubit Gates
- **CNOT**: Controlled-NOT
- **CZ**: Controlled-Z
- **Toffoli**: Controlled-Controlled-NOT
- **Fredkin**: Controlled-SWAP

### Composite Operations
- **QFT**: Quantum Fourier Transform
- **Controlled Gates**: Any gate can be controlled
- **Circuit Composition**: Sequential and parallel composition

## 🔬 Quantum Algorithms

Built-in implementations include:

- **Bell States**: Maximally entangled two-qubit states
- **GHZ States**: Multi-qubit entanglement
- **Deutsch Algorithm**: Quantum function evaluation
- **Grover Search**: Quadratic speedup for search
- **Quantum Fourier Transform**: Basis for many algorithms
- **Variational Quantum Eigensolver (VQE)**: Quantum chemistry
- **Quantum Approximate Optimization (QAOA)**: Combinatorial optimization

## 🌐 Web Interface

The Next.js web interface provides:

- **Interactive Playground**: Visual circuit editor with real-time simulation
- **Algorithm Gallery**: Pre-built quantum algorithms with explanations
- **Documentation**: Comprehensive guides and API reference
- **Performance Metrics**: Simulation timing and resource usage
- **Export Options**: Download circuits in multiple formats

## 🔧 Development

### Project Structure

```
packages/haskq-dsl/
├── src/
│   ├── HaskQ/
│   │   ├── Core.hs          # Main DSL interface
│   │   ├── Types.hs         # Quantum data types
│   │   ├── Gates.hs         # Gate constructors
│   │   └── Export.hs        # JSON serialization
│   └── app/
│       └── Main.hs          # CLI application
├── test/                    # Test suite
└── haskq-dsl.cabal         # Package configuration

packages/haskq-sim/
├── src/
│   └── lib.rs              # WASM simulation engine
├── Cargo.toml              # Rust configuration
└── pkg/                    # Generated WASM output

apps/haskq-unified/
├── src/
│   ├── app/                # Next.js pages
│   ├── components/         # React components
│   └── lib/
│       ├── haskq-wasm.ts   # WASM integration
│       └── types/          # TypeScript definitions
└── package.json            # Node.js configuration
```

### Building from Source

1. **Haskell Development**
   ```bash
   cd packages/haskq-dsl
   stack test                # Run tests
   stack haddock            # Generate documentation
   stack ghci               # Interactive development
   ```

2. **Rust Development**
   ```bash
   cd packages/haskq-sim
   cargo test               # Run tests
   cargo bench              # Performance benchmarks
   wasm-pack build --dev    # Development build
   ```

3. **Web Development**
   ```bash
   cd apps/haskq-unified
   npm run dev              # Development server
   npm run build            # Production build
   npm run test             # Run tests
   ```

## 🚀 Deployment

### Production Build

```bash
# Build all components
./scripts/build-all.sh

# Deploy to Vercel
vercel deploy --prod
```

### Docker Deployment

```bash
docker build -t haskq .
docker run -p 3000:3000 haskq
```

## 📊 Performance

### Simulation Benchmarks

| Qubits | States | Simulation Time | Memory Usage |
|--------|--------|----------------|--------------|
| 10     | 1,024  | 1.2ms          | 16KB         |
| 15     | 32,768 | 45ms           | 512KB        |
| 20     | 1M     | 1.8s           | 16MB         |

### WASM vs Native Performance

- **WASM**: ~80% of native Rust performance
- **JavaScript**: ~10x slower than WASM
- **Memory**: WASM uses 2x less memory than JavaScript

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Workflow

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

### Code Style

- **Haskell**: Follow [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide)
- **Rust**: Use `cargo fmt` and `cargo clippy`
- **TypeScript**: Use Prettier and ESLint

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🔗 Related Projects

- **[Phantasius](https://phantasius.vercel.app/)**: AGDEF Theory & Consciousness Dynamics
- **[Romulus](https://romulus-rouge.vercel.app/)**: Modified Gravity & Dark Matter Research
- **[Arcana Obscura](https://arcana-obscura.vercel.app/)**: Esoteric Wisdom & Hermetic Principles

## 📞 Support

- **Documentation**: [https://haskq-unified.vercel.app/docs](https://haskq-unified.vercel.app/docs)
- **Issues**: [GitHub Issues](https://github.com/HaskQ/haskq-unified/issues)
- **Discussions**: [GitHub Discussions](https://github.com/HaskQ/haskq-unified/discussions)

---

**HaskQ** - Bridging the gap between theoretical quantum computing and practical implementation through the power of functional programming and high-performance simulation. 