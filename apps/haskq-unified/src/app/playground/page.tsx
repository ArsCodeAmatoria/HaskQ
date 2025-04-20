'use client';

import { useState, useEffect } from 'react';
import dynamic from 'next/dynamic';
import ExamplesPanel from '@/components/playground/ExamplesPanel';
import CircuitVisualizer from '@/components/playground/CircuitVisualizer';
import PlaygroundHeader from '@/components/playground/PlaygroundHeader';
import { loader } from '@monaco-editor/react';

// Dynamically import the Monaco Editor with SSR disabled
const MonacoEditor = dynamic(
  () => import('@monaco-editor/react'),
  { ssr: false }
);

// Define Haskell keywords and syntax highlighting rules
const haskellSyntax = {
  keywords: [
    'module', 'where', 'import', 'qualified', 'as', 'hiding',
    'type', 'data', 'newtype', 'class', 'instance', 'deriving',
    'do', 'case', 'of', 'let', 'in', 'if', 'then', 'else',
    'forall', 'family', 'default', 'foreign', 'export', 'dynamic'
  ],
  operators: [
    '=', '->', '=>', '::', ':', '|', '\\', '<-', '@', '~', '=>', '$', '<$>', '<*>', '>>=', '>>'
  ],
  typeKeywords: [
    'Int', 'Integer', 'Float', 'Double', 'Bool', 'Char', 'String',
    'Maybe', 'Either', 'IO', 'Qubit', 'Circ', 'Measurement'
  ],
  builtins: [
    'map', 'filter', 'foldr', 'foldl', 'zip', 'unzip', 'head', 'tail',
    'fst', 'snd', 'null', 'length', 'reverse', 'concat', 'sum', 'product',
    'hadamard', 'measure', 'cnot', 'qinit', 'gateX', 'gateY', 'gateZ'
  ]
};

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
  pure [m1, m2]`;

export default function PlaygroundPage() {
  const [code, setCode] = useState(DEFAULT_CODE);
  const [result, setResult] = useState<string | null>(null);
  const [isSimulating, setIsSimulating] = useState(false);
  const [editorMounted, setEditorMounted] = useState(false);
  
  // Configure Monaco editor for Haskell syntax highlighting
  useEffect(() => {
    if (typeof window !== 'undefined') {
      // Make sure this runs only in browser
      loader.init().then((monaco) => {
        if (!monaco) return;
        
        // Register Haskell language if it doesn't exist
        if (!monaco.languages.getLanguages().some(lang => lang.id === 'haskell')) {
          monaco.languages.register({ id: 'haskell' });
          
          // Define Haskell syntax highlighting
          monaco.languages.setMonarchTokensProvider('haskell', {
            tokenizer: {
              root: [
                // Comments
                [/--.*$/, 'comment'],
                [/{-/, 'comment', '@comment'],
                
                // Strings
                [/"([^"\\]|\\.)*$/, 'string.invalid'],
                [/"/, 'string', '@string'],
                
                // Numbers
                [/\d+(\.\d+)?([eE][+-]?\d+)?/, 'number'],
                
                // Keywords
                [/\b(?:class|data|deriving|do|else|if|import|in|infix|infixl|infixr|instance|let|module|newtype|of|then|type|where|qualified|as|hiding)\b/, 'keyword'],
                
                // Type-related keywords
                [/\b(?:Int|Integer|Bool|Char|String|IO|Maybe|Either|Qubit|Circ|Measurement)\b/, 'type'],
                
                // Functions and operators
                [/\b(?:map|filter|foldr|foldl|zip|pure|return|hadamard|measure|cnot|qinit|gateX|gateY|gateZ)\b/, 'function'],
                [/[=\->:<\|\.\\@~\$\+\*]+/, 'operator'],
                
                // Variable names
                [/[a-z][a-zA-Z0-9_']*/, 'variable'],
                
                // Type names (start with uppercase)
                [/[A-Z][a-zA-Z0-9_']*/, 'type.identifier']
              ],
              comment: [
                [/[^{-]+/, 'comment'],
                [/-}/, 'comment', '@pop'],
                [/{-/, 'comment', '@push'],
                [/[{-]/, 'comment']
              ],
              string: [
                [/[^\\"]+/, 'string'],
                [/\\./, 'string.escape'],
                [/"/, 'string', '@pop']
              ]
            }
          });
          
          // Define a custom theme for Haskell
          monaco.editor.defineTheme('haskQDark', {
            base: 'vs-dark',
            inherit: true,
            rules: [
              { token: 'comment', foreground: '6A9955' },
              { token: 'string', foreground: 'CE9178' },
              { token: 'keyword', foreground: '569CD6', fontStyle: 'bold' },
              { token: 'type', foreground: '4EC9B0' },
              { token: 'type.identifier', foreground: '4EC9B0' },
              { token: 'function', foreground: 'DCDCAA' },
              { token: 'variable', foreground: '9CDCFE' },
              { token: 'number', foreground: 'B5CEA8' },
              { token: 'operator', foreground: 'D4D4D4' },
              { token: 'string.escape', foreground: 'D7BA7D' }
            ],
            colors: {
              'editor.background': '#1E1E1E',
              'editor.foreground': '#D4D4D4',
              'editorLineNumber.foreground': '#858585',
              'editor.lineHighlightBackground': '#2D2D30',
              'editorCursor.foreground': '#A6A6A6',
              'editor.selectionBackground': '#264F78',
              'editor.inactiveSelectionBackground': '#3A3D41'
            }
          });
        }
      });
    }
  }, []);
  
  // Function to handle editor mounting
  const handleEditorDidMount = () => {
    setEditorMounted(true);
  };
  
  // Simulate function (mock implementation - would be connected to WebAssembly)
  const simulateCode = () => {
    setIsSimulating(true);
    
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
      `);
      setIsSimulating(false);
    }, 1500);
  };

  // Reset code to default
  const resetCode = () => {
    setCode(DEFAULT_CODE);
  };

  // Save circuit (mock implementation)
  const saveCircuit = () => {
    const savedData = {
      code: code,
      timestamp: new Date().toISOString()
    };
    localStorage.setItem('savedCircuit', JSON.stringify(savedData));
    alert('Circuit saved to local storage!');
  };

  // Export circuit (mock implementation)
  const exportCircuit = () => {
    const dataStr = "data:text/plain;charset=utf-8," + encodeURIComponent(code);
    const downloadAnchorNode = document.createElement('a');
    downloadAnchorNode.setAttribute("href", dataStr);
    downloadAnchorNode.setAttribute("download", "haskq-circuit.hs");
    document.body.appendChild(downloadAnchorNode);
    downloadAnchorNode.click();
    downloadAnchorNode.remove();
  };

  // Handle example selection
  const handleExampleSelect = (exampleCode: string) => {
    setCode(exampleCode);
  };
  
  return (
    <div className="container mx-auto px-4 py-8">
      <PlaygroundHeader 
        onRunSimulation={simulateCode}
        onSaveCircuit={saveCircuit}
        onExportCircuit={exportCircuit}
        onResetCode={resetCode}
        isSimulating={isSimulating}
      />
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Code Editor Section */}
        <div className="flex flex-col space-y-6">
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden flex-grow">
            <div className="bg-gray-100 dark:bg-gray-900 px-4 py-3 border-b border-gray-200 dark:border-gray-700">
              <h2 className="font-semibold text-sm">Code Editor</h2>
            </div>
            <div className="h-[500px]">
              <MonacoEditor
                height="100%"
                language="haskell"
                theme="haskQDark"
                value={code}
                onChange={(value) => setCode(value || '')}
                onMount={handleEditorDidMount}
                options={{
                  minimap: { enabled: false },
                  scrollBeyondLastLine: false,
                  fontSize: 14,
                  fontFamily: 'Menlo, Monaco, Consolas, "Courier New", monospace',
                  fontLigatures: true,
                  cursorBlinking: 'smooth',
                  smoothScrolling: true,
                  contextmenu: true,
                  formatOnPaste: true,
                  formatOnType: true,
                  renderLineHighlight: 'all',
                  wordWrap: 'on',
                  automaticLayout: true,
                  tabSize: 2,
                  lineNumbersMinChars: 3,
                  lineDecorationsWidth: 10,
                  folding: true,
                  glyphMargin: false,
                  bracketPairColorization: {
                    enabled: true,
                  },
                }}
              />
            </div>
          </div>
          
          {/* Examples Panel */}
          <div className="h-[300px]">
            <ExamplesPanel onSelectExample={handleExampleSelect} />
          </div>
        </div>
        
        {/* Circuit Visualization Section */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden h-full">
          <CircuitVisualizer 
            result={result}
            isSimulating={isSimulating}
          />
        </div>
      </div>
      
      <div className="mt-8 bg-gray-50 dark:bg-gray-900 rounded-lg p-6 border border-gray-200 dark:border-gray-700">
        <h2 className="text-lg font-semibold mb-2">About the Playground</h2>
        <p className="text-gray-600 dark:text-gray-300 mb-4">
          The HaskQ Playground allows you to write and simulate quantum circuits directly in your browser. 
          Use the editor to write your quantum code, select from example circuits, or build your own.
        </p>
        <p className="text-gray-600 dark:text-gray-300">
          This is a simplified simulator for educational purposes. For more advanced simulations, 
          consider installing the full HaskQ library.
        </p>
      </div>
    </div>
  );
} 