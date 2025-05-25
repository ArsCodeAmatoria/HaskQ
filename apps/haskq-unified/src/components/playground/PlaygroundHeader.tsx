'use client';

import React, { useState } from 'react';
import { Play, Save, Download, RotateCcw, Settings, Zap } from 'lucide-react';

interface PlaygroundHeaderProps {
  onRunSimulation: (options?: SimulationOptions) => void;
  onSaveCircuit: () => void;
  onExportCircuit: () => void;
  onResetCode: () => void;
  isSimulating: boolean;
}

interface SimulationOptions {
  numRuns: number;
  noiseModel: string;
  noiseLevel: number;
  analysisDepth: string;
  includeFidelity: boolean;
  includeEntanglement: boolean;
}

export default function PlaygroundHeader({
  onRunSimulation,
  onSaveCircuit,
  onExportCircuit,
  onResetCode,
  isSimulating
}: PlaygroundHeaderProps) {
  const [showOptions, setShowOptions] = useState(false);
  const [options, setOptions] = useState<SimulationOptions>({
    numRuns: 1000,
    noiseModel: 'none',
    noiseLevel: 0.05,
    analysisDepth: 'detailed',
    includeFidelity: true,
    includeEntanglement: true
  });

  const handleRunSimulation = () => {
    onRunSimulation(options);
  };

  return (
    <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 mb-6">
      <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between space-y-4 lg:space-y-0">
        <div>
          <h1 className="text-2xl font-bold text-gray-900 dark:text-white">Quantum Circuit Playground</h1>
          <p className="text-gray-600 dark:text-gray-400 mt-1">
            Design, simulate, and analyze quantum circuits with enhanced visualization and metrics
          </p>
        </div>
        
        <div className="flex flex-wrap items-center gap-3">
          <button
            onClick={() => setShowOptions(!showOptions)}
            className="bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 px-4 py-2 rounded-md hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors flex items-center space-x-2"
          >
            <Settings className="w-4 h-4" />
            <span>Options</span>
          </button>
          
          <button
            onClick={handleRunSimulation}
            disabled={isSimulating}
            className="bg-indigo-600 hover:bg-indigo-700 disabled:bg-indigo-400 text-white px-6 py-2 rounded-md transition-colors flex items-center space-x-2 font-medium"
          >
            {isSimulating ? (
              <>
                <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
                <span>Simulating...</span>
              </>
            ) : (
              <>
                <Zap className="w-4 h-4" />
                <span>Run Simulation</span>
              </>
            )}
          </button>
          
          <div className="flex items-center space-x-2">
            <button
              onClick={onSaveCircuit}
              className="bg-green-100 dark:bg-green-900 text-green-700 dark:text-green-300 px-4 py-2 rounded-md hover:bg-green-200 dark:hover:bg-green-800 transition-colors flex items-center space-x-2"
            >
              <Save className="w-4 h-4" />
              <span>Save</span>
            </button>
            
            <button
              onClick={onExportCircuit}
              className="bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300 px-4 py-2 rounded-md hover:bg-blue-200 dark:hover:bg-blue-800 transition-colors flex items-center space-x-2"
            >
              <Download className="w-4 h-4" />
              <span>Export</span>
            </button>
            
            <button
              onClick={onResetCode}
              className="bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 px-4 py-2 rounded-md hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors flex items-center space-x-2"
            >
              <RotateCcw className="w-4 h-4" />
              <span>Reset</span>
            </button>
          </div>
        </div>
      </div>

      {/* Simulation Options Panel */}
      {showOptions && (
        <div className="mt-6 p-4 bg-gray-50 dark:bg-gray-900 rounded-lg border border-gray-200 dark:border-gray-700">
          <h3 className="text-lg font-semibold mb-4 flex items-center">
            <Settings className="w-5 h-5 mr-2" />
            Simulation Configuration
          </h3>
          
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {/* Number of Runs */}
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Monte Carlo Runs
              </label>
              <select
                value={options.numRuns}
                onChange={(e) => setOptions({...options, numRuns: parseInt(e.target.value)})}
                className="w-full border border-gray-300 dark:border-gray-600 rounded-md px-3 py-2 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
              >
                <option value={100}>100 (Fast)</option>
                <option value={1000}>1,000 (Standard)</option>
                <option value={10000}>10,000 (High Precision)</option>
                <option value={100000}>100,000 (Research Grade)</option>
              </select>
              <p className="text-xs text-gray-500 mt-1">More runs = better statistics</p>
            </div>

            {/* Noise Model */}
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Noise Model
              </label>
              <select
                value={options.noiseModel}
                onChange={(e) => setOptions({...options, noiseModel: e.target.value})}
                className="w-full border border-gray-300 dark:border-gray-600 rounded-md px-3 py-2 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
              >
                <option value="none">Ideal (No Noise)</option>
                <option value="depolarizing">Depolarizing</option>
                <option value="amplitude_damping">Amplitude Damping</option>
                <option value="phase_damping">Phase Damping</option>
                <option value="thermal">Thermal Noise</option>
                <option value="custom">Custom Realistic</option>
              </select>
              <p className="text-xs text-gray-500 mt-1">Simulate real quantum hardware</p>
            </div>

            {/* Noise Level */}
            <div className={options.noiseModel === 'none' ? 'opacity-50 pointer-events-none' : ''}>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Noise Level
              </label>
              <div className="flex items-center space-x-3">
                <input
                  type="range"
                  min="0.001"
                  max="0.5"
                  step="0.001"
                  value={options.noiseLevel}
                  onChange={(e) => setOptions({...options, noiseLevel: parseFloat(e.target.value)})}
                  className="flex-1"
                  disabled={options.noiseModel === 'none'}
                />
                <span className="text-sm font-mono w-12">{(options.noiseLevel * 100).toFixed(1)}%</span>
              </div>
              <p className="text-xs text-gray-500 mt-1">Error probability per gate</p>
            </div>

            {/* Analysis Depth */}
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Analysis Depth
              </label>
              <select
                value={options.analysisDepth}
                onChange={(e) => setOptions({...options, analysisDepth: e.target.value})}
                className="w-full border border-gray-300 dark:border-gray-600 rounded-md px-3 py-2 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
              >
                <option value="basic">Basic Results</option>
                <option value="detailed">Detailed Analysis</option>
                <option value="research">Research Grade</option>
                <option value="debug">Debug Mode</option>
              </select>
              <p className="text-xs text-gray-500 mt-1">Level of detail in results</p>
            </div>

            {/* Additional Metrics */}
            <div className="md:col-span-2">
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Additional Metrics
              </label>
              <div className="space-y-2">
                <label className="flex items-center">
                  <input
                    type="checkbox"
                    checked={options.includeFidelity}
                    onChange={(e) => setOptions({...options, includeFidelity: e.target.checked})}
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                  />
                  <span className="ml-2 text-sm">Fidelity Analysis</span>
                </label>
                <label className="flex items-center">
                  <input
                    type="checkbox"
                    checked={options.includeEntanglement}
                    onChange={(e) => setOptions({...options, includeEntanglement: e.target.checked})}
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                  />
                  <span className="ml-2 text-sm">Entanglement Metrics</span>
                </label>
              </div>
            </div>
          </div>
          
          <div className="mt-4 p-3 bg-blue-50 dark:bg-blue-950 rounded-md">
            <h4 className="text-sm font-medium text-blue-800 dark:text-blue-200 mb-2">Simulation Preview</h4>
            <div className="text-xs text-blue-700 dark:text-blue-300 space-y-1">
              <p>• {options.numRuns.toLocaleString()} Monte Carlo simulations</p>
              <p>• {options.noiseModel === 'none' ? 'Ideal quantum behavior' : `${options.noiseModel} noise (${(options.noiseLevel * 100).toFixed(1)}%)`}</p>
              <p>• {options.analysisDepth} analysis with {[options.includeFidelity && 'fidelity', options.includeEntanglement && 'entanglement'].filter(Boolean).join(' & ')} metrics</p>
              <p>• Estimated runtime: ~{Math.ceil(options.numRuns / 1000 * 2)}s</p>
            </div>
          </div>
        </div>
      )}
    </div>
  );
} 