'use client'

import { useEffect, useRef } from 'react'
import { Cpu, ZoomIn, ZoomOut, RotateCcw, Download } from 'lucide-react'

interface CircuitVisualizerProps {
  result: string | null
  isSimulating: boolean
}

export default function CircuitVisualizer({ result, isSimulating }: CircuitVisualizerProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  // This is where we would render the circuit visualization in an actual implementation
  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas || !result) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    // Clear canvas
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    // For now, we'll just show a placeholder visualization
    // In a real implementation, we would parse the circuit description and render it
    const drawCircuit = () => {
      const width = canvas.width
      const height = canvas.height

      ctx.fillStyle = '#1a1a1a'
      ctx.fillRect(0, 0, width, height)

      // Draw qubit lines
      ctx.strokeStyle = '#666666'
      ctx.lineWidth = 2

      const numQubits = 2
      const ySpacing = height / (numQubits + 1)

      for (let i = 0; i < numQubits; i++) {
        const y = (i + 1) * ySpacing
        ctx.beginPath()
        ctx.moveTo(50, y)
        ctx.lineTo(width - 50, y)
        ctx.stroke()

        // Label qubits
        ctx.fillStyle = '#999999'
        ctx.font = '14px Menlo, monospace'
        ctx.textAlign = 'right'
        ctx.fillText(`q${i}:`, 40, y + 5)
      }

      // Draw hadamard gate
      const drawH = (x: number, y: number) => {
        ctx.fillStyle = '#0087ff'
        ctx.fillRect(x - 15, y - 15, 30, 30)
        ctx.fillStyle = 'white'
        ctx.font = 'bold 16px Menlo, monospace'
        ctx.textAlign = 'center'
        ctx.textBaseline = 'middle'
        ctx.fillText('H', x, y)
      }

      // Draw CNOT control
      const drawControl = (x: number, y: number) => {
        ctx.fillStyle = '#0087ff'
        ctx.beginPath()
        ctx.arc(x, y, 6, 0, 2 * Math.PI)
        ctx.fill()
      }

      // Draw CNOT target
      const drawTarget = (x: number, y: number) => {
        ctx.strokeStyle = '#0087ff'
        ctx.lineWidth = 2
        ctx.beginPath()
        ctx.arc(x, y, 12, 0, 2 * Math.PI)
        ctx.stroke()

        ctx.beginPath()
        ctx.moveTo(x - 12, y)
        ctx.lineTo(x + 12, y)
        ctx.stroke()

        ctx.beginPath()
        ctx.moveTo(x, y - 12)
        ctx.lineTo(x, y + 12)
        ctx.stroke()
      }

      // Draw CNOT connection
      const drawCNOTLine = (x: number, y1: number, y2: number) => {
        ctx.strokeStyle = '#0087ff'
        ctx.lineWidth = 2
        ctx.beginPath()
        ctx.moveTo(x, y1)
        ctx.lineTo(x, y2)
        ctx.stroke()
      }

      // Draw measurement
      const drawMeasurement = (x: number, y: number) => {
        ctx.fillStyle = '#cc6600'
        ctx.fillRect(x - 15, y - 15, 30, 30)
        ctx.fillStyle = 'white'
        ctx.font = 'bold 16px Menlo, monospace'
        ctx.textAlign = 'center'
        ctx.textBaseline = 'middle'
        ctx.fillText('M', x, y)
      }

      // Position gates along the circuit
      const xH = 150
      const xCNOT = 250
      const xMeasure = 350

      // Draw Hadamard gate on qubit 0
      drawH(xH, ySpacing)

      // Draw CNOT with control on qubit 0 and target on qubit 1
      drawControl(xCNOT, ySpacing)
      drawTarget(xCNOT, 2 * ySpacing)
      drawCNOTLine(xCNOT, ySpacing, 2 * ySpacing)

      // Draw measurements
      drawMeasurement(xMeasure, ySpacing)
      drawMeasurement(xMeasure, 2 * ySpacing)
    }

    drawCircuit()

  }, [result])

  // Create a downloading image function
  const downloadCanvas = () => {
    if (!canvasRef.current) return
    
    const canvas = canvasRef.current
    const image = canvas.toDataURL('image/png')
    const link = document.createElement('a')
    link.download = 'haskq-circuit.png'
    link.href = image
    link.click()
  }

  const buttonStyle = {
    color: '#999',
    backgroundColor: 'transparent',
    border: 'none',
    cursor: 'pointer',
    padding: '4px',
    transition: 'color 0.2s'
  };

  return (
    <div style={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
      <div style={{ 
        backgroundColor: '#1a1a1a', 
        padding: '8px 16px',
        fontSize: '14px',
        fontWeight: '500',
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        borderBottom: '1px solid #333'
      }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <Cpu style={{ height: '16px', width: '16px', color: '#0087ff' }} />
          <span>Circuit Visualization</span>
        </div>
        <div style={{ display: 'flex', gap: '8px' }}>
          <button style={buttonStyle} title="Zoom in">
            <ZoomIn style={{ height: '16px', width: '16px' }} />
          </button>
          <button style={buttonStyle} title="Zoom out">
            <ZoomOut style={{ height: '16px', width: '16px' }} />
          </button>
          <button style={buttonStyle} title="Reset view">
            <RotateCcw style={{ height: '16px', width: '16px' }} />
          </button>
          <button 
            style={buttonStyle}
            title="Download as image"
            onClick={downloadCanvas}
          >
            <Download style={{ height: '16px', width: '16px' }} />
          </button>
        </div>
      </div>
      <div style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
        {isSimulating ? (
          <div style={{ 
            flex: 1, 
            display: 'flex', 
            alignItems: 'center', 
            justifyContent: 'center' 
          }}>
            <div style={{ 
              display: 'flex', 
              flexDirection: 'column', 
              alignItems: 'center' 
            }}>
              <div style={{ 
                width: '64px', 
                height: '64px', 
                borderRadius: '50%',
                border: '4px solid #0087ff',
                borderTopColor: 'transparent',
                animation: 'spin 1s linear infinite'
              }}></div>
              <p style={{ marginTop: '16px', color: '#999' }}>Simulating circuit...</p>
            </div>
          </div>
        ) : result ? (
          <div style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
            <div style={{ position: 'relative', flex: 1 }}>
              <canvas 
                ref={canvasRef}
                style={{ 
                  position: 'absolute', 
                  inset: 0, 
                  width: '100%', 
                  height: '100%' 
                }}
                width={800}
                height={400}
              />
            </div>
            <div style={{ 
              padding: '16px', 
              backgroundColor: '#1a1a1a', 
              borderTop: '1px solid #333' 
            }}>
              <h3 style={{ 
                marginBottom: '8px', 
                fontSize: '14px', 
                fontWeight: '500', 
                color: '#0087ff' 
              }}>Simulation Results</h3>
              <pre style={{ 
                fontFamily: 'monospace', 
                fontSize: '12px', 
                whiteSpace: 'pre-wrap', 
                backgroundColor: '#0d0d0d', 
                padding: '12px', 
                borderRadius: '6px', 
                overflow: 'auto', 
                maxHeight: '160px' 
              }}>
                {result}
              </pre>
            </div>
          </div>
        ) : (
          <div style={{ 
            flex: 1, 
            display: 'flex', 
            alignItems: 'center', 
            justifyContent: 'center' 
          }}>
            <div style={{ 
              maxWidth: '400px', 
              textAlign: 'center' 
            }}>
              <p style={{ 
                color: '#999', 
                marginBottom: '8px' 
              }}>Run the simulation to see the circuit visualization and results.</p>
              <div style={{ 
                marginTop: '16px', 
                border: '1px dashed #333', 
                padding: '32px', 
                borderRadius: '6px' 
              }}>
                <Cpu style={{ 
                  width: '48px', 
                  height: '48px', 
                  color: '#666', 
                  margin: '0 auto 16px' 
                }} />
                <p style={{ 
                  fontSize: '14px', 
                  color: '#666' 
                }}>Your circuit visualization will appear here after simulation.</p>
              </div>
            </div>
          </div>
        )}
      </div>
      <style jsx>{`
        @keyframes spin {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }
      `}</style>
    </div>
  )
} 