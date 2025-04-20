'use client';

import { useEffect, useRef } from 'react';
import { AlertCircle, Loader2 } from 'lucide-react';

interface CircuitVisualizerProps {
  result: string | null;
  isSimulating: boolean;
}

export default function CircuitVisualizer({ result, isSimulating }: CircuitVisualizerProps) {
  const resultRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (resultRef.current && result) {
      resultRef.current.scrollTop = 0;
    }
  }, [result]);

  return (
    <div className="h-full flex flex-col">
      <div className="bg-gray-100 dark:bg-gray-900 px-4 py-3 border-b border-gray-200 dark:border-gray-700 flex justify-between items-center">
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