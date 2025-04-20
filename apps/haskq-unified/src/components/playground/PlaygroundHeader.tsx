'use client';

import { Play, Save, Download, RefreshCw } from 'lucide-react';

interface PlaygroundHeaderProps {
  onRunSimulation: () => void;
  onSaveCircuit: () => void;
  onExportCircuit: () => void;
  onResetCode: () => void;
  isSimulating: boolean;
}

export default function PlaygroundHeader({
  onRunSimulation,
  onSaveCircuit,
  onExportCircuit,
  onResetCode,
  isSimulating
}: PlaygroundHeaderProps) {
  return (
    <div className="flex items-center justify-between mb-6">
      <h1 className="text-2xl font-bold">Quantum Playground</h1>
      
      <div className="flex items-center space-x-2">
        <button
          onClick={onResetCode}
          className="bg-gray-200 hover:bg-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 text-gray-800 dark:text-gray-200 px-3 py-2 rounded-md flex items-center text-sm transition-colors"
          title="Reset Code"
        >
          <RefreshCw className="h-4 w-4 mr-2" />
          Reset
        </button>
        
        <button
          onClick={onSaveCircuit}
          className="bg-gray-200 hover:bg-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 text-gray-800 dark:text-gray-200 px-3 py-2 rounded-md flex items-center text-sm transition-colors"
          title="Save Circuit"
        >
          <Save className="h-4 w-4 mr-2" />
          Save
        </button>
        
        <button
          onClick={onExportCircuit}
          className="bg-gray-200 hover:bg-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 text-gray-800 dark:text-gray-200 px-3 py-2 rounded-md flex items-center text-sm transition-colors"
          title="Export as File"
        >
          <Download className="h-4 w-4 mr-2" />
          Export
        </button>
        
        <button
          onClick={onRunSimulation}
          disabled={isSimulating}
          className={`bg-indigo-600 hover:bg-indigo-700 text-white px-4 py-2 rounded-md flex items-center text-sm transition-colors ${
            isSimulating ? 'opacity-70 cursor-not-allowed' : ''
          }`}
          title="Run Simulation"
        >
          {isSimulating ? (
            <>
              <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
              Simulating...
            </>
          ) : (
            <>
              <Play className="h-4 w-4 mr-2" />
              Run Simulation
            </>
          )}
        </button>
      </div>
    </div>
  );
} 