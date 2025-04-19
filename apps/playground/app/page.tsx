'use client'

import { useState } from 'react'
import Header from '../components/Header'
import CodeEditor from '../components/CodeEditor'
import CircuitVisualizer from '../components/CircuitVisualizer'
import ExamplesPanel from '../components/ExamplesPanel'

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

  // Simulate function (mock implementation - would be connected to WebAssembly)
  const simulateCode = () => {
    setIsSimulating(true)
    
    // This would normally call a WebAssembly module with the compiled Haskell code
    setTimeout(() => {
      setResult(`
Circuit with 2 qubits:

0: --H--●--M--
        |
1: -----X--M--

Simulation results:
- Probability of |00⟩: 0.5
- Probability of |11⟩: 0.5
- Probability of |01⟩: 0.0
- Probability of |10⟩: 0.0

Measurement outcomes:
- 50% chance: [Zero, Zero]
- 50% chance: [One, One]
- 0% chance:  [Zero, One]
- 0% chance:  [One, Zero]

This is the Bell state |Φ⁺⟩ = 1/√2 (|00⟩ + |11⟩), which demonstrates quantum entanglement.
The measurement of one qubit determines the state of the other qubit, regardless of distance.
      `)
      setIsSimulating(false)
    }, 1500)
  }

  // Reset code to default
  const resetCode = () => {
    setCode(DEFAULT_CODE)
  }

  // Save circuit (mock implementation)
  const saveCircuit = () => {
    const savedData = {
      code: code,
      timestamp: new Date().toISOString()
    }
    localStorage.setItem('savedCircuit', JSON.stringify(savedData))
    alert('Circuit saved to local storage!')
  }

  // Export circuit (mock implementation)
  const exportCircuit = () => {
    const dataStr = "data:text/plain;charset=utf-8," + encodeURIComponent(code)
    const downloadAnchorNode = document.createElement('a')
    downloadAnchorNode.setAttribute("href", dataStr)
    downloadAnchorNode.setAttribute("download", "haskq-circuit.hs")
    document.body.appendChild(downloadAnchorNode)
    downloadAnchorNode.click()
    downloadAnchorNode.remove()
  }

  // Handle example selection
  const handleExampleSelect = (exampleCode: string) => {
    setCode(exampleCode)
  }

  return (
    <>
      <Header 
        onRunSimulation={simulateCode}
        onSaveCircuit={saveCircuit}
        onExportCircuit={exportCircuit}
        isSimulating={isSimulating}
        className=""
      />
      
      <main style={{ 
        minHeight: '100vh',
        backgroundColor: '#121212', 
        color: 'white',
        padding: '24px'
      }}>
        <div style={{
          maxWidth: '1200px',
          margin: '0 auto',
          display: 'grid',
          gridTemplateColumns: '1fr',
          gap: '24px'
        }}>
          {/* Left Column - Code Editor */}
          <div style={{
            backgroundColor: '#2a2a2a',
            borderRadius: '8px',
            overflow: 'hidden',
            boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)',
            minHeight: '500px'
          }}>
            <CodeEditor
              code={code}
              onChange={setCode}
              resetCode={resetCode}
            />
          </div>
          
          {/* Right Column - Circuit Visualization & Examples */}
          <div style={{
            backgroundColor: '#2a2a2a',
            borderRadius: '8px',
            overflow: 'hidden',
            boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)',
            minHeight: '400px'
          }}>
            <div style={{
              backgroundColor: '#1a1a1a',
              padding: '8px 16px',
              borderBottom: '1px solid #3a3a3a',
              fontSize: '14px',
              fontWeight: '500'
            }}>
              Circuit Visualization
            </div>
            <CircuitVisualizer 
              result={result}
              isSimulating={isSimulating}
            />
          </div>
          
          <div style={{
            backgroundColor: '#2a2a2a',
            borderRadius: '8px',
            overflow: 'hidden',
            boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)',
            minHeight: '200px'
          }}>
            <div style={{
              backgroundColor: '#1a1a1a',
              padding: '8px 16px',
              borderBottom: '1px solid #3a3a3a',
              fontSize: '14px',
              fontWeight: '500'
            }}>
              Example Circuits
            </div>
            <ExamplesPanel onSelectExample={handleExampleSelect} />
          </div>
        </div>
      </main>
    </>
  )
} 