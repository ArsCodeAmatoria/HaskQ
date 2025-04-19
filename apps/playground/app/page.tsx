'use client'

import { useState } from 'react'
import Editor from '@monaco-editor/react'
import { Play, Download, Upload, Copy, RotateCcw, Save } from 'lucide-react'

const DEFAULT_CODE = `-- Create a Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩)
bellState :: Circ (Qubit, Qubit)
bellState = withQubits 2 $ \\[q1, q2] -> do
  q1' <- hadamard q1
  (q1'', q2') <- cnot q1' q2
  pure (q1'', q2')

-- Measure both qubits
main :: Circ [Measurement]
main = do
  (q1, q2) <- bellState
  (m1, q1') <- measure q1
  (m2, q2') <- measure q2
  pure [m1, m2]`

export default function Playground() {
  const [code, setCode] = useState(DEFAULT_CODE)
  const [result, setResult] = useState<string | null>(null)
  const [isSimulating, setIsSimulating] = useState(false)

  // Simulate function (mock implementation)
  const simulateCode = () => {
    setIsSimulating(true)
    
    // This would normally call a WebAssembly module with the compiled Haskell code
    setTimeout(() => {
      setResult(`
Circuit with 2 qubits:

0: --H-------●-------M--
             |
1: ----------X-------M--

Simulation result:
- Probability of |00⟩: 0.5
- Probability of |11⟩: 0.5
- Probability of |01⟩: 0.0
- Probability of |10⟩: 0.0

Measurement outcome: [Zero, Zero] or [One, One] with equal probability
      `)
      setIsSimulating(false)
    }, 1500)
  }

  return (
    <main className="min-h-screen bg-gradient-to-b from-quantum-dark-950 to-quantum-dark-900 text-white">
      <header className="border-b border-quantum-dark-800 py-4">
        <div className="container-custom flex justify-between items-center">
          <h1 className="text-2xl font-bold">HaskQ Playground</h1>
          <div className="flex items-center space-x-2">
            <button className="btn bg-quantum-blue-600 hover:bg-quantum-blue-700 text-white px-3 py-2 rounded-md flex items-center" onClick={simulateCode}>
              <Play className="mr-2 h-4 w-4" />
              {isSimulating ? 'Simulating...' : 'Run Simulation'}
            </button>
            <button className="btn bg-quantum-dark-800 hover:bg-quantum-dark-700 text-white px-3 py-2 rounded-md flex items-center">
              <Save className="mr-2 h-4 w-4" />
              Save
            </button>
          </div>
        </div>
      </header>

      <div className="container-custom py-6">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Code Editor */}
          <div className="bg-quantum-dark-800 rounded-md overflow-hidden shadow-xl h-[600px]">
            <div className="bg-quantum-dark-900 px-4 py-2 text-sm font-medium flex justify-between items-center">
              <span>Circuit Code</span>
              <div className="flex space-x-2">
                <button className="text-quantum-dark-400 hover:text-quantum-blue-400">
                  <Copy className="h-4 w-4" />
                </button>
                <button className="text-quantum-dark-400 hover:text-quantum-blue-400" onClick={() => setCode(DEFAULT_CODE)}>
                  <RotateCcw className="h-4 w-4" />
                </button>
              </div>
            </div>
            <Editor
              height="calc(100% - 36px)"
              defaultLanguage="haskell"
              value={code}
              onChange={(value) => setCode(value || '')}
              theme="vs-dark"
              options={{
                fontSize: 14,
                minimap: { enabled: false },
                scrollBeyondLastLine: false,
                automaticLayout: true,
              }}
            />
          </div>

          {/* Circuit Visualization and Results */}
          <div className="flex flex-col h-[600px]">
            {/* Circuit Visualization */}
            <div className="bg-quantum-dark-800 rounded-md overflow-hidden shadow-xl flex-1 mb-4">
              <div className="bg-quantum-dark-900 px-4 py-2 text-sm font-medium">
                Circuit Visualization
              </div>
              <div className="p-4 h-[calc(100%-36px)] overflow-auto font-mono text-sm whitespace-pre">
                {result || "Run the simulation to see the circuit visualization and results."}
              </div>
            </div>

            {/* Examples */}
            <div className="bg-quantum-dark-800 rounded-md overflow-hidden shadow-xl">
              <div className="bg-quantum-dark-900 px-4 py-2 text-sm font-medium">
                Examples
              </div>
              <div className="p-4 grid grid-cols-2 gap-2">
                <ExampleButton name="Bell State" onClick={() => setCode(DEFAULT_CODE)} />
                <ExampleButton name="Teleportation" onClick={() => {}} />
                <ExampleButton name="Deutsch Algorithm" onClick={() => {}} />
                <ExampleButton name="Grover Search" onClick={() => {}} />
              </div>
            </div>
          </div>
        </div>
      </div>
    </main>
  )
}

function ExampleButton({ name, onClick }: { name: string, onClick: () => void }) {
  return (
    <button 
      className="bg-quantum-dark-700 hover:bg-quantum-dark-600 rounded-md px-4 py-2 text-sm font-medium"
      onClick={onClick}
    >
      {name}
    </button>
  )
} 