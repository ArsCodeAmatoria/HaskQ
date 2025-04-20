'use client';

import { useEffect, useRef, useState } from 'react';
import { AlertCircle, Loader2, Cpu, Download, ZoomIn, ZoomOut, RotateCcw } from 'lucide-react';

interface CircuitVisualizerProps {
  result: string | null;
  isSimulating: boolean;
}

export default function CircuitVisualizer({ result, isSimulating }: CircuitVisualizerProps) {
  const resultRef = useRef<HTMLDivElement>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [showCanvas, setShowCanvas] = useState(false);

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

  return (
    <div className="h-full flex flex-col">
      {showCanvas && (
        <div className="bg-gray-100 dark:bg-gray-900 px-4 py-2 border-b border-gray-200 dark:border-gray-700 flex justify-between items-center">
          <div className="flex items-center space-x-2">
            <Cpu className="h-4 w-4 text-indigo-500" />
            <h2 className="font-semibold text-sm">Circuit Diagram</h2>
          </div>
          <div className="flex items-center space-x-2">
            <button 
              className="text-gray-500 hover:text-indigo-600 dark:text-gray-400 dark:hover:text-indigo-400"
              title="Zoom in"
            >
              <ZoomIn className="h-4 w-4" />
            </button>
            <button 
              className="text-gray-500 hover:text-indigo-600 dark:text-gray-400 dark:hover:text-indigo-400"
              title="Zoom out"
            >
              <ZoomOut className="h-4 w-4" />
            </button>
            <button 
              className="text-gray-500 hover:text-indigo-600 dark:text-gray-400 dark:hover:text-indigo-400"
              title="Reset view"
            >
              <RotateCcw className="h-4 w-4" />
            </button>
            <button 
              className="text-gray-500 hover:text-indigo-600 dark:text-gray-400 dark:hover:text-indigo-400"
              title="Download image"
              onClick={downloadCanvas}
            >
              <Download className="h-4 w-4" />
            </button>
          </div>
        </div>
      )}
      
      {showCanvas && (
        <div className="bg-gray-800 h-48 overflow-hidden">
          <canvas 
            ref={canvasRef} 
            className="w-full h-full" 
          />
        </div>
      )}
      
      <div className="bg-gray-100 dark:bg-gray-900 px-4 py-2 border-t border-b border-gray-200 dark:border-gray-700">
        <h2 className="font-semibold text-sm">Simulation Results</h2>
      </div>

      <div className="flex-1 p-4 overflow-auto font-mono text-sm" ref={resultRef}>
        {isSimulating ? (
          <div className="flex flex-col items-center justify-center h-full text-center">
            <Loader2 className="w-8 h-8 text-indigo-500 animate-spin mb-4" />
            <p className="text-gray-600 dark:text-gray-300">Running simulation...</p>
          </div>
        ) : result ? (
          <div className="whitespace-pre-wrap">{result}</div>
        ) : (
          <div className="flex flex-col items-center justify-center h-full text-center">
            <AlertCircle className="w-8 h-8 text-gray-400 mb-4" />
            <p className="text-gray-600 dark:text-gray-300 mb-2">No simulation results yet</p>
            <p className="text-gray-500 dark:text-gray-400 text-xs max-w-md">
              Write your quantum circuit code and click "Run Simulation" to see the results here
            </p>
          </div>
        )}
      </div>

      {result && (
        <div className="p-4 border-t border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-900">
          <div className="text-xs text-gray-500 dark:text-gray-400">
            <div className="flex items-center mb-1">
              <div className="h-3 w-3 rounded-full bg-indigo-500 mr-2"></div>
              <span>Qubit lines</span>
            </div>
            <div className="flex items-center mb-1">
              <div className="h-3 w-3 rounded-full bg-purple-500 mr-2"></div>
              <span>Measurement</span>
            </div>
            <div className="flex items-center">
              <div className="h-3 w-3 rounded-full bg-green-500 mr-2"></div>
              <span>Superposition</span>
            </div>
          </div>
        </div>
      )}
    </div>
  );
} 