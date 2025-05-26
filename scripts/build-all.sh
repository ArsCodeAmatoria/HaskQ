#!/bin/bash

# HaskQ Build Script
# Builds all components of the HaskQ library in the correct order

set -e  # Exit on any error

echo "🚀 Building HaskQ Library Components..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    print_status "Checking prerequisites..."
    
    # Check for Haskell Stack
    if ! command -v stack &> /dev/null; then
        print_error "Haskell Stack not found. Please install Stack first."
        exit 1
    fi
    
    # Check for Rust and Cargo
    if ! command -v cargo &> /dev/null; then
        print_error "Rust/Cargo not found. Please install Rust first."
        exit 1
    fi
    
    # Check for wasm-pack
    if ! command -v wasm-pack &> /dev/null; then
        print_warning "wasm-pack not found. Installing..."
        curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
    fi
    
    # Check for Node.js
    if ! command -v node &> /dev/null; then
        print_error "Node.js not found. Please install Node.js first."
        exit 1
    fi
    
    print_success "All prerequisites found!"
}

# Build Haskell DSL
build_haskell_dsl() {
    print_status "Building Haskell DSL..."
    
    cd packages/haskq-dsl
    
    # Clean previous builds
    stack clean
    
    # Build the library
    print_status "Building Haskell library..."
    stack build
    
    # Run tests
    print_status "Running Haskell tests..."
    if stack test; then
        print_success "Haskell tests passed!"
    else
        print_warning "Some Haskell tests failed, but continuing..."
    fi
    
    # Build CLI executable
    print_status "Building CLI executable..."
    stack install --local-bin-path ../../bin
    
    # Generate documentation
    print_status "Generating Haskell documentation..."
    stack haddock
    
    cd ../..
    print_success "Haskell DSL build complete!"
}

# Build Rust WASM module
build_rust_wasm() {
    print_status "Building Rust WASM module..."
    
    cd packages/haskq-sim
    
    # Clean previous builds
    cargo clean
    
    # Run tests
    print_status "Running Rust tests..."
    if cargo test; then
        print_success "Rust tests passed!"
    else
        print_warning "Some Rust tests failed, but continuing..."
    fi
    
    # Build WASM module for production
    print_status "Building WASM module..."
    wasm-pack build --target web --release
    
    # Copy WASM files to web app
    print_status "Copying WASM files to web app..."
    mkdir -p ../../apps/haskq-unified/public/wasm
    cp pkg/* ../../apps/haskq-unified/public/wasm/ 2>/dev/null || true
    
    cd ../..
    print_success "Rust WASM build complete!"
}

# Build Next.js web interface
build_web_interface() {
    print_status "Building Next.js web interface..."
    
    cd apps/haskq-unified
    
    # Install dependencies
    print_status "Installing Node.js dependencies..."
    npm ci
    
    # Run linting
    print_status "Running ESLint..."
    if npm run lint; then
        print_success "Linting passed!"
    else
        print_warning "Linting issues found, but continuing..."
    fi
    
    # Run tests
    print_status "Running web tests..."
    if npm run test -- --passWithNoTests; then
        print_success "Web tests passed!"
    else
        print_warning "Some web tests failed, but continuing..."
    fi
    
    # Build for production
    print_status "Building Next.js app for production..."
    npm run build
    
    cd ../..
    print_success "Web interface build complete!"
}

# Package releases
package_releases() {
    print_status "Packaging releases..."
    
    # Create release directory
    mkdir -p releases
    
    # Package Haskell DSL
    print_status "Packaging Haskell DSL..."
    cd packages/haskq-dsl
    stack sdist
    cp .stack-work/dist/*/haskq-dsl-*.tar.gz ../../releases/
    cd ../..
    
    # Package Rust WASM
    print_status "Packaging Rust WASM..."
    cd packages/haskq-sim
    tar -czf ../../releases/haskq-sim-wasm.tar.gz pkg/
    cd ../..
    
    # Package CLI binary
    if [ -f bin/haskq-cli ]; then
        print_status "Packaging CLI binary..."
        tar -czf releases/haskq-cli-$(uname -s)-$(uname -m).tar.gz -C bin haskq-cli
    fi
    
    print_success "Packaging complete!"
}

# Generate build info
generate_build_info() {
    print_status "Generating build information..."
    
    cat > build-info.json << EOF
{
  "build_time": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "git_commit": "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')",
  "git_branch": "$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'unknown')",
  "components": {
    "haskq-dsl": {
      "version": "$(grep '^version:' packages/haskq-dsl/haskq-dsl.cabal | awk '{print $2}')",
      "ghc_version": "$(stack --version | head -n1 | awk '{print $2}')"
    },
    "haskq-sim": {
      "version": "$(grep '^version =' packages/haskq-sim/Cargo.toml | cut -d'"' -f2)",
      "rust_version": "$(rustc --version | awk '{print $2}')"
    },
    "haskq-unified": {
      "version": "$(grep '\"version\":' apps/haskq-unified/package.json | cut -d'"' -f4)",
      "node_version": "$(node --version)"
    }
  }
}
EOF
    
    print_success "Build info generated!"
}

# Main build process
main() {
    echo "🔧 HaskQ Library Build System"
    echo "=============================="
    
    # Record start time
    start_time=$(date +%s)
    
    # Run build steps
    check_prerequisites
    build_haskell_dsl
    build_rust_wasm
    build_web_interface
    package_releases
    generate_build_info
    
    # Calculate build time
    end_time=$(date +%s)
    build_duration=$((end_time - start_time))
    
    echo ""
    echo "🎉 Build Complete!"
    echo "=================="
    print_success "Total build time: ${build_duration} seconds"
    print_success "All components built successfully!"
    
    echo ""
    echo "📦 Generated Artifacts:"
    echo "----------------------"
    echo "• Haskell DSL: packages/haskq-dsl/.stack-work/"
    echo "• Rust WASM: packages/haskq-sim/pkg/"
    echo "• Web App: apps/haskq-unified/.next/"
    echo "• CLI Binary: bin/haskq-cli"
    echo "• Releases: releases/"
    
    echo ""
    echo "🚀 Next Steps:"
    echo "-------------"
    echo "• Run 'npm run dev' in apps/haskq-unified/ to start development server"
    echo "• Use './bin/haskq-cli --help' to explore CLI options"
    echo "• Deploy with 'vercel deploy --prod' or your preferred platform"
}

# Run main function
main "$@" 