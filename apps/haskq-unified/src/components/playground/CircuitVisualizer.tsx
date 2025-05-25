'use client';

import React, { useRef, useEffect, useState } from 'react';
import { AlertCircle, Loader2, Cpu, Download, ZoomIn, ZoomOut, RotateCcw, Copy, BarChart3, Info } from 'lucide-react';

interface CircuitVisualizerProps {
  result: string | null;
  isSimulating: boolean;
}

export default function CircuitVisualizer({ result, isSimulating }: CircuitVisualizerProps) {
  const resultRef = useRef<HTMLDivElement>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [showCanvas, setShowCanvas] = useState(false);
  const [showMetrics, setShowMetrics] = useState(true);
  const [showDetails, setShowDetails] = useState(true);

  useEffect(() => {
    if (resultRef.current && result) {
      resultRef.current.scrollTop = 0;
    }

    // If we have a result, show the canvas visualization
    if (result) {
      setShowCanvas(true);
    }
  }, [result]);

  // Handle circuit visualization drawing
  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas || !result || !showCanvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    // Set canvas size
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;

    // Clear canvas
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // Draw circuit visualization
    const drawCircuit = () => {
      const width = canvas.width;
      const height = canvas.height;

      ctx.fillStyle = '#1a1a1a';
      ctx.fillRect(0, 0, width, height);

      // Draw qubit lines
      ctx.strokeStyle = '#666666';
      ctx.lineWidth = 2;

      const numQubits = 2;
      const ySpacing = height / (numQubits + 1);

      for (let i = 0; i < numQubits; i++) {
        const y = (i + 1) * ySpacing;
        ctx.beginPath();
        ctx.moveTo(50, y);
        ctx.lineTo(width - 50, y);
        ctx.stroke();

        // Label qubits
        ctx.fillStyle = '#999999';
        ctx.font = '14px monospace';
        ctx.textAlign = 'right';
        ctx.fillText(`q${i}:`, 40, y + 5);
      }

      // Draw hadamard gate
      const drawH = (x: number, y: number) => {
        ctx.fillStyle = '#6366f1'; // indigo-500
        ctx.fillRect(x - 15, y - 15, 30, 30);
        ctx.fillStyle = 'white';
        ctx.font = 'bold 16px monospace';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('H', x, y);
      };

      // Draw CNOT control
      const drawControl = (x: number, y: number) => {
        ctx.fillStyle = '#6366f1'; // indigo-500
        ctx.beginPath();
        ctx.arc(x, y, 6, 0, 2 * Math.PI);
        ctx.fill();
      };

      // Draw CNOT target
      const drawTarget = (x: number, y: number) => {
        ctx.strokeStyle = '#6366f1'; // indigo-500
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.arc(x, y, 12, 0, 2 * Math.PI);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(x - 12, y);
        ctx.lineTo(x + 12, y);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(x, y - 12);
        ctx.lineTo(x, y + 12);
        ctx.stroke();
      };

      // Draw CNOT connection
      const drawCNOTLine = (x: number, y1: number, y2: number) => {
        ctx.strokeStyle = '#6366f1'; // indigo-500
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(x, y1);
        ctx.lineTo(x, y2);
        ctx.stroke();
      };

      // Draw measurement
      const drawMeasurement = (x: number, y: number) => {
        ctx.fillStyle = '#a855f7'; // purple-500
        ctx.fillRect(x - 15, y - 15, 30, 30);
        ctx.fillStyle = 'white';
        ctx.font = 'bold 16px monospace';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('M', x, y);
      };

      // Position gates along the circuit
      const xH = 150;
      const xCNOT = 250;
      const xMeasure = 350;

      // Draw Hadamard gate on qubit 0
      drawH(xH, ySpacing);

      // Draw CNOT with control on qubit 0 and target on qubit 1
      drawControl(xCNOT, ySpacing);
      drawTarget(xCNOT, 2 * ySpacing);
      drawCNOTLine(xCNOT, ySpacing, 2 * ySpacing);

      // Draw measurements
      drawMeasurement(xMeasure, ySpacing);
      drawMeasurement(xMeasure, 2 * ySpacing);
    };

    drawCircuit();
  }, [showCanvas, result]);

  // Function to download the canvas as an image
  const downloadCanvas = () => {
    if (!canvasRef.current) return;
    
    const canvas = canvasRef.current;
    const image = canvas.toDataURL('image/png');
    const link = document.createElement('a');
    link.download = 'haskq-circuit.png';
    link.href = image;
    link.click();
  };

  // Extract metrics from results for quick view
  const extractMetrics = (resultText: string) => {
    if (!resultText) return null;
    
    const metrics = {
      circuitType: resultText.match(/Circuit Type: (.+)/)?.[1] || 'Unknown',
      qubits: resultText.match(/Qubits: (\d+)/)?.[1] || 'N/A',
      fidelity: resultText.match(/Fidelity: ([\d.]+)/)?.[1] || 'N/A',
      entanglement: resultText.includes('Entanglement: YES') ? 'YES' : 'NO',
      success: resultText.match(/Success (?:Rate|Probability): ([\d.]+)%/)?.[1] || null
    };
    
    return metrics;
  };

  const metrics = result ? extractMetrics(result) : null;

  // Copy results to clipboard
  const copyResults = () => {
    if (result) {
      navigator.clipboard.writeText(result);
    }
  };

  // Export results as file
  const exportResults = () => {
    if (result) {
      const blob = new Blob([result], { type: 'text/plain' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `haskq-simulation-${new Date().toISOString().slice(0, 10)}.txt`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }
  };

  return (
    <div className="flex flex-col h-full">
      <div className="bg-gray-100 dark:bg-gray-900 px-4 py-3 border-b border-gray-200 dark:border-gray-700 flex items-center justify-between">
        <div className="flex items-center space-x-2">
          <h2 className="font-semibold text-sm">Simulation Results</h2>
          {result && (
            <div className="flex items-center space-x-2">
              <button
                onClick={() => setShowMetrics(!showMetrics)}
                className="text-xs bg-indigo-100 dark:bg-indigo-900 text-indigo-700 dark:text-indigo-300 px-2 py-1 rounded flex items-center space-x-1 hover:bg-indigo-200 dark:hover:bg-indigo-800 transition-colors"
              >
                <BarChart3 className="w-3 h-3" />
                <span>Metrics</span>
              </button>
              <button
                onClick={() => setShowDetails(!showDetails)}
                className="text-xs bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300 px-2 py-1 rounded flex items-center space-x-1 hover:bg-gray-300 dark:hover:bg-gray-600 transition-colors"
              >
                <Info className="w-3 h-3" />
                <span>{showDetails ? 'Hide' : 'Show'} Details</span>
              </button>
            </div>
          )}
        </div>
        
        {result && (
          <div className="flex items-center space-x-2">
            <button
              onClick={copyResults}
              className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200 transition-colors"
              title="Copy to clipboard"
            >
              <Copy className="w-4 h-4" />
            </button>
            <button
              onClick={exportResults}
              className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200 transition-colors"
              title="Export results"
            >
              <Download className="w-4 h-4" />
            </button>
          </div>
        )}
      </div>

      {/* Quick Metrics Panel */}
      {result && metrics && showMetrics && (
        <div className="bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950 dark:to-purple-950 border-b border-gray-200 dark:border-gray-700 p-3">
          <div className="grid grid-cols-2 md:grid-cols-5 gap-4 text-sm">
            <div className="text-center">
              <div className="font-semibold text-indigo-600 dark:text-indigo-400">{metrics.circuitType}</div>
              <div className="text-xs text-gray-500">Circuit Type</div>
            </div>
            <div className="text-center">
              <div className="font-semibold text-purple-600 dark:text-purple-400">{metrics.qubits}</div>
              <div className="text-xs text-gray-500">Qubits</div>
            </div>
            <div className="text-center">
              <div className="font-semibold text-green-600 dark:text-green-400">
                {metrics.fidelity !== 'N/A' ? `${(parseFloat(metrics.fidelity) * 100).toFixed(1)}%` : 'N/A'}
              </div>
              <div className="text-xs text-gray-500">Fidelity</div>
            </div>
            <div className="text-center">
              <div className={`font-semibold ${metrics.entanglement === 'YES' ? 'text-orange-600 dark:text-orange-400' : 'text-gray-500'}`}>
                {metrics.entanglement}
              </div>
              <div className="text-xs text-gray-500">Entanglement</div>
            </div>
            <div className="text-center">
              <div className="font-semibold text-blue-600 dark:text-blue-400">
                {metrics.success ? `${metrics.success}%` : 'N/A'}
              </div>
              <div className="text-xs text-gray-500">Success Rate</div>
            </div>
          </div>
        </div>
      )}

      <div className="flex-1 p-4 overflow-auto font-mono text-sm" ref={resultRef}>
        {isSimulating ? (
          <div className="flex flex-col items-center justify-center h-full text-center">
            <Loader2 className="w-8 h-8 text-indigo-500 animate-spin mb-4" />
            <p className="text-gray-600 dark:text-gray-300 mb-2">Running quantum simulation...</p>
            <div className="flex items-center space-x-2 text-xs text-gray-500">
              <div className="w-2 h-2 bg-indigo-500 rounded-full animate-pulse"></div>
              <span>Initializing quantum state</span>
            </div>
            <div className="flex items-center space-x-2 text-xs text-gray-500 mt-1">
              <div className="w-2 h-2 bg-purple-500 rounded-full animate-pulse" style={{ animationDelay: '0.5s' }}></div>
              <span>Applying quantum gates</span>
            </div>
            <div className="flex items-center space-x-2 text-xs text-gray-500 mt-1">
              <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse" style={{ animationDelay: '1s' }}></div>
              <span>Computing measurement probabilities</span>
            </div>
          </div>
        ) : result ? (
          <div className="space-y-4">
            {showDetails ? (
              <div className="whitespace-pre-wrap text-xs leading-relaxed">{result}</div>
            ) : (
              <div className="text-center text-gray-500">
                <p>Details hidden. Click "Show Details" to see full simulation results.</p>
                <p className="text-xs mt-2">Use the metrics panel above for a quick overview.</p>
              </div>
            )}
          </div>
        ) : (
          <div className="flex flex-col items-center justify-center h-full text-center">
            <AlertCircle className="w-8 h-8 text-gray-400 mb-4" />
            <p className="text-gray-600 dark:text-gray-300 mb-2">No simulation results yet</p>
            <p className="text-gray-500 dark:text-gray-400 text-xs max-w-md">
              Write your quantum circuit code and click "Run Simulation" to see detailed results including 
              state vectors, probability distributions, and performance metrics.
            </p>
          </div>
        )}
      </div>

      {result && showDetails && (
        <div className="p-4 border-t border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-900">
          <div className="text-xs text-gray-500 dark:text-gray-400">
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="flex items-center">
                <div className="h-3 w-3 rounded-full bg-indigo-500 mr-2"></div>
                <span>State vectors & amplitudes</span>
              </div>
              <div className="flex items-center">
                <div className="h-3 w-3 rounded-full bg-purple-500 mr-2"></div>
                <span>Measurement statistics</span>
              </div>
              <div className="flex items-center">
                <div className="h-3 w-3 rounded-full bg-green-500 mr-2"></div>
                <span>Entanglement metrics</span>
              </div>
              <div className="flex items-center">
                <div className="h-3 w-3 rounded-full bg-orange-500 mr-2"></div>
                <span>Performance analysis</span>
              </div>
            </div>
            
            <div className="mt-3 pt-3 border-t border-gray-300 dark:border-gray-600 text-center">
              <p className="text-gray-400">
                Enhanced simulation results powered by HaskQ's state vector simulator
              </p>
            </div>
          </div>
        </div>
      )}
    </div>
  );
} 