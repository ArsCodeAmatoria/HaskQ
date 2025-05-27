'use client'

import React from 'react'
import Link from 'next/link'
import { 
  Download, 
  Terminal, 
  Code, 
  Zap, 
  Package, 
  GitBranch,
  ExternalLink,
  CheckCircle,
  FileCode,
  Layers
} from 'lucide-react'

// Custom components matching the existing codebase style
function Button({ children, variant = 'primary', size = 'default', asChild = false, className = '', ...props }: {
  children: React.ReactNode
  variant?: 'primary' | 'outline'
  size?: 'default' | 'sm'
  asChild?: boolean
  className?: string
  [key: string]: any
}) {
  const baseClasses = "inline-flex items-center justify-center font-medium rounded-md transition-colors"
  const variants = {
    primary: "bg-indigo-600 text-white hover:bg-indigo-700 shadow-sm",
    outline: "border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-700"
  }
  const sizes = {
    default: "px-4 py-2",
    sm: "px-3 py-1.5 text-sm"
  }
  
  const classes = `${baseClasses} ${variants[variant]} ${sizes[size]} ${className}`
  
  if (asChild && React.isValidElement(children)) {
    return React.cloneElement(children as React.ReactElement, { className: classes, ...props })
  }
  
  return <button className={classes} {...props}>{children}</button>
}

function Card({ children, className = '', ...props }: { children: React.ReactNode, className?: string, [key: string]: any }) {
  return (
    <div className={`bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg shadow-sm ${className}`} {...props}>
      {children}
    </div>
  )
}

function CardHeader({ children, className = '', ...props }: { children: React.ReactNode, className?: string, [key: string]: any }) {
  return <div className={`p-6 pb-4 ${className}`} {...props}>{children}</div>
}

function CardContent({ children, className = '', ...props }: { children: React.ReactNode, className?: string, [key: string]: any }) {
  return <div className={`p-6 pt-0 ${className}`} {...props}>{children}</div>
}

function CardTitle({ children, className = '', ...props }: { children: React.ReactNode, className?: string, [key: string]: any }) {
  return <h3 className={`text-xl font-semibold text-gray-900 dark:text-gray-100 ${className}`} {...props}>{children}</h3>
}

function CardDescription({ children, className = '', ...props }: { children: React.ReactNode, className?: string, [key: string]: any }) {
  return <p className={`text-gray-600 dark:text-gray-400 mt-2 ${className}`} {...props}>{children}</p>
}

function Badge({ children, variant = 'secondary', className = '', ...props }: {
  children: React.ReactNode
  variant?: 'secondary'
  className?: string
  [key: string]: any
}) {
  return (
    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-300 ${className}`} {...props}>
      {children}
    </span>
  )
}

function Tabs({ defaultValue, className = '', ...props }: {
  defaultValue: string
  className?: string
  [key: string]: any
}) {
  const [activeTab, setActiveTab] = React.useState(defaultValue)
  
  return (
    <div className={className} {...props}>
      <div className="inline-flex h-10 items-center justify-center rounded-md bg-gray-100 dark:bg-gray-800 p-1 w-full grid grid-cols-3">
        <button
          className={`inline-flex items-center justify-center whitespace-nowrap rounded-sm px-3 py-1.5 text-sm font-medium transition-all ${
            activeTab === 'haskell' 
              ? 'bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 shadow-sm' 
              : 'text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-100'
          }`}
          onClick={() => setActiveTab('haskell')}
        >
          Haskell DSL
        </button>
        <button
          className={`inline-flex items-center justify-center whitespace-nowrap rounded-sm px-3 py-1.5 text-sm font-medium transition-all ${
            activeTab === 'rust' 
              ? 'bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 shadow-sm' 
              : 'text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-100'
          }`}
          onClick={() => setActiveTab('rust')}
        >
          Rust WASM
        </button>
        <button
          className={`inline-flex items-center justify-center whitespace-nowrap rounded-sm px-3 py-1.5 text-sm font-medium transition-all ${
            activeTab === 'integration' 
              ? 'bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 shadow-sm' 
              : 'text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-gray-100'
          }`}
          onClick={() => setActiveTab('integration')}
        >
          Integration
        </button>
      </div>
      
      {activeTab === 'haskell' && (
        <div className="mt-6 space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <FileCode className="w-5 h-5" />
                HaskQ DSL (Haskell)
              </CardTitle>
              <CardDescription>
                Symbolic quantum circuit composition and type-safe quantum logic
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-3">
                <h4 className="font-semibold">Prerequisites</h4>
                <ul className="list-disc list-inside space-y-1 text-sm text-gray-600 dark:text-gray-400">
                  <li>GHC 9.0+ (Glasgow Haskell Compiler)</li>
                  <li>Stack or Cabal build tools</li>
                  <li>4GB+ RAM for compilation</li>
                </ul>
              </div>

              <div className="space-y-3">
                <h4 className="font-semibold">Installation</h4>
                <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg font-mono text-sm">
                  <div className="space-y-2">
                    <div># Create new HaskQ DSL project</div>
                    <div>stack new haskq-dsl simple</div>
                    <div>cd haskq-dsl</div>
                    <div></div>
                    <div># Install dependencies</div>
                    <div>stack build</div>
                    <div>stack install</div>
                  </div>
                </div>
              </div>

              <div className="space-y-3">
                <h4 className="font-semibold">Example Usage</h4>
                <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg font-mono text-sm">
                  <div className="space-y-1">
                    <div className="text-blue-600">-- Bell state circuit</div>
                    <div>exampleBell :: Circuit</div>
                    <div>exampleBell = Circuit [H (Qubit 0), CNOT (Qubit 0) (Qubit 1)]</div>
                    <div></div>
                    <div className="text-blue-600">-- Export to JSON for WASM</div>
                    <div>saveCircuit "bell.json" exampleBell</div>
                  </div>
                </div>
              </div>

              <div className="flex gap-3">
                <Button className="flex items-center gap-2">
                  <Download className="w-4 h-4" />
                  Download HaskQ DSL
                </Button>
                <Button variant="outline" className="flex items-center gap-2">
                  <GitBranch className="w-4 h-4" />
                  View on GitHub
                </Button>
              </div>
            </CardContent>
          </Card>
        </div>
      )}

      {activeTab === 'rust' && (
        <div className="mt-6 space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Zap className="w-5 h-5" />
                HaskQ Simulator (Rust WASM)
              </CardTitle>
              <CardDescription>
                High-performance quantum simulation engine for the browser
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-3">
                <h4 className="font-semibold">Prerequisites</h4>
                <ul className="list-disc list-inside space-y-1 text-sm text-gray-600 dark:text-gray-400">
                  <li>Rust 1.70+ with cargo</li>
                  <li>wasm-pack for WebAssembly compilation</li>
                  <li>Node.js 18+ for integration testing</li>
                </ul>
              </div>

              <div className="space-y-3">
                <h4 className="font-semibold">Setup</h4>
                <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg font-mono text-sm">
                  <div className="space-y-2">
                    <div># Create Rust WASM project</div>
                    <div>cargo new haskq-sim --lib</div>
                    <div>cd haskq-sim</div>
                    <div></div>
                    <div># Install wasm-pack</div>
                    <div>curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh</div>
                    <div></div>
                    <div># Build WebAssembly module</div>
                    <div>wasm-pack build --target web</div>
                  </div>
                </div>
              </div>

              <div className="space-y-3">
                <h4 className="font-semibold">Example Usage</h4>
                <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg font-mono text-sm">
                  <div className="space-y-1">
                    <div className="text-green-600">// Load and simulate quantum circuit</div>
                    <div>import init, {`{ simulate }`} from './wasm/haskq_sim';</div>
                    <div></div>
                    <div>await init();</div>
                    <div>const result = simulate(circuitJson);</div>
                    <div>console.log(result);</div>
                  </div>
                </div>
              </div>

              <div className="flex gap-3">
                <Button className="flex items-center gap-2">
                  <Download className="w-4 h-4" />
                  Download WASM Module
                </Button>
                <Button variant="outline" className="flex items-center gap-2">
                  <Package className="w-4 h-4" />
                  Cargo Crate
                </Button>
              </div>
            </CardContent>
          </Card>
        </div>
      )}

      {activeTab === 'integration' && (
        <div className="mt-6 space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Code className="w-5 h-5" />
                Full Stack Integration
              </CardTitle>
              <CardDescription>
                Connecting Haskell DSL with Rust WASM in Next.js
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-3">
                <h4 className="font-semibold">Workflow</h4>
                <div className="grid gap-3">
                  <div className="flex items-center gap-3 p-3 border border-gray-200 dark:border-gray-700 rounded-lg">
                    <div className="w-8 h-8 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center text-blue-600 dark:text-blue-400 font-semibold text-sm">1</div>
                    <div>
                      <div className="font-medium">Circuit Definition</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">Define quantum circuits using Haskell DSL</div>
                    </div>
                  </div>
                  <div className="flex items-center gap-3 p-3 border border-gray-200 dark:border-gray-700 rounded-lg">
                    <div className="w-8 h-8 bg-orange-100 dark:bg-orange-900 rounded-full flex items-center justify-center text-orange-600 dark:text-orange-400 font-semibold text-sm">2</div>
                    <div>
                      <div className="font-medium">JSON Export</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">Export circuits to JSON for WASM consumption</div>
                    </div>
                  </div>
                  <div className="flex items-center gap-3 p-3 border border-gray-200 dark:border-gray-700 rounded-lg">
                    <div className="w-8 h-8 bg-green-100 dark:bg-green-900 rounded-full flex items-center justify-center text-green-600 dark:text-green-400 font-semibold text-sm">3</div>
                    <div>
                      <div className="font-medium">WASM Simulation</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">High-performance simulation in the browser</div>
                    </div>
                  </div>
                </div>
              </div>

              <div className="space-y-3">
                <h4 className="font-semibold">Complete Example</h4>
                <div className="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg font-mono text-sm">
                  <div className="space-y-1">
                    <div className="text-purple-600">// 1. Define circuit in Haskell</div>
                    <div>grover :: Int -&gt; Circuit</div>
                    <div>grover n = Circuit $ oracle ++ diffuser</div>
                    <div></div>
                    <div className="text-purple-600">// 2. Export and load in TypeScript</div>
                    <div>const circuit = await loadCircuit('grover.json');</div>
                    <div>const result = await simulate(circuit);</div>
                    <div></div>
                    <div className="text-purple-600">// 3. Visualize results</div>
                    <div>renderQuantumState(result.amplitudes);</div>
                  </div>
                </div>
              </div>

              <div className="flex gap-3">
                <Button className="flex items-center gap-2">
                  <Download className="w-4 h-4" />
                  Download Full Stack
                </Button>
                <Link href="/playground" className="inline-flex items-center justify-center px-4 py-2 border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-700 font-medium rounded-md transition-colors">
                  <ExternalLink className="w-4 h-4 mr-2" />
                  Try Playground
                </Link>
              </div>
            </CardContent>
          </Card>
        </div>
      )}
    </div>
  )
}

export default function DownloadsPage() {
  return (
    <div className="container mx-auto px-4 py-8 max-w-6xl">
      {/* Header */}
      <div className="text-center mb-12">
        <h1 className="text-4xl font-bold bg-gradient-to-r from-blue-600 to-purple-600 bg-clip-text text-transparent mb-4">
          HaskQ Library Downloads
        </h1>
        <p className="text-xl text-gray-600 dark:text-gray-400 mb-6">
          Hybrid Haskell + Rust quantum computing library for symbolic circuit composition and high-performance simulation
        </p>
        <div className="flex flex-wrap justify-center gap-2 mb-8">
          <Badge className="flex items-center gap-1">
            <FileCode className="w-3 h-3" />
            Haskell DSL
          </Badge>
          <Badge className="flex items-center gap-1">
            <Zap className="w-3 h-3" />
            Rust WASM
          </Badge>
          <Badge className="flex items-center gap-1">
            <Layers className="w-3 h-3" />
            Type Safety
          </Badge>
          <Badge className="flex items-center gap-1">
            <CheckCircle className="w-3 h-3" />
            Web Compatible
          </Badge>
        </div>
      </div>

      {/* Architecture Overview */}
      <Card className="mb-8">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Layers className="w-5 h-5" />
            Hybrid Architecture
          </CardTitle>
          <CardDescription>
            HaskQ combines the best of symbolic reasoning and high-performance simulation
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid md:grid-cols-3 gap-6">
            <div className="text-center p-4 border border-gray-200 dark:border-gray-700 rounded-lg">
              <FileCode className="w-12 h-12 mx-auto mb-3 text-blue-600" />
              <h3 className="font-semibold mb-2">Haskell DSL</h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                Type-safe quantum circuit composition, symbolic reasoning, and logic programming
              </p>
            </div>
            <div className="text-center p-4 border border-gray-200 dark:border-gray-700 rounded-lg">
              <Zap className="w-12 h-12 mx-auto mb-3 text-orange-600" />
              <h3 className="font-semibold mb-2">Rust WASM</h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                High-performance quantum simulation compiled to WebAssembly for browser execution
              </p>
            </div>
            <div className="text-center p-4 border border-gray-200 dark:border-gray-700 rounded-lg">
              <Code className="w-12 h-12 mx-auto mb-3 text-green-600" />
              <h3 className="font-semibold mb-2">Next.js Frontend</h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                Modern web interface with interactive playground and documentation
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Installation Tabs */}
      <Tabs defaultValue="haskell" className="mb-8">
        {/* Tabs content is now handled inline within the Tabs component */}
      </Tabs>

      {/* Additional Resources */}
      <div className="grid md:grid-cols-2 gap-6">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Terminal className="w-5 h-5" />
              CLI Tools
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-3">
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Command-line interface for circuit generation and analysis
            </p>
            <div className="bg-gray-100 dark:bg-gray-800 p-3 rounded font-mono text-sm">
              haskq generate --algorithm grover --qubits 4<br/>
              haskq simulate --circuit bell.json --shots 1000<br/>
              haskq analyze --file quantum_circuit.hs
            </div>
            <Button variant="outline" size="sm">Download CLI</Button>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Package className="w-5 h-5" />
              Package Registry
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-3">
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Install via your favorite package manager
            </p>
            <div className="space-y-2 text-sm">
              <div><span className="font-mono bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded">stack install haskq</span></div>
              <div><span className="font-mono bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded">cargo install haskq-sim</span></div>
              <div><span className="font-mono bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded">npm install @haskq/wasm</span></div>
            </div>
            <Button variant="outline" size="sm">View Packages</Button>
          </CardContent>
        </Card>
      </div>

      {/* Navigation */}
      <div className="text-center mt-12 pt-8 border-t border-gray-200 dark:border-gray-700">
        <p className="text-gray-600 dark:text-gray-400 mb-4">
          Ready to start building quantum applications?
        </p>
        <div className="flex flex-wrap justify-center gap-3">
          <Link href="/docs" className="inline-flex items-center justify-center px-4 py-2 bg-indigo-600 text-white hover:bg-indigo-700 font-medium rounded-md transition-colors shadow-sm">
            Read Documentation
          </Link>
          <Link href="/playground" className="inline-flex items-center justify-center px-4 py-2 border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-700 font-medium rounded-md transition-colors">
            Try Playground
          </Link>
          <Link href="/" className="inline-flex items-center justify-center px-4 py-2 border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-700 font-medium rounded-md transition-colors">
            Back to Home
          </Link>
        </div>
      </div>
    </div>
  )
} 