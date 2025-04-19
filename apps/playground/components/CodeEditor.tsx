'use client'

import { useState } from 'react'
import Editor from '@monaco-editor/react'
import { Copy, RotateCcw, Check, Code } from 'lucide-react'

interface CodeEditorProps {
  code: string
  onChange: (value: string) => void
  resetCode: () => void
}

export default function CodeEditor({ code, onChange, resetCode }: CodeEditorProps) {
  const [copied, setCopied] = useState(false)

  const handleCopy = () => {
    navigator.clipboard.writeText(code)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
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
          <Code style={{ height: '16px', width: '16px', color: '#0087ff' }} />
          <span>Circuit Code</span>
        </div>
        <div style={{ display: 'flex', gap: '8px' }}>
          <button 
            style={buttonStyle}
            onClick={handleCopy}
            title="Copy code"
          >
            {copied ? (
              <Check style={{ height: '16px', width: '16px', color: '#22c55e' }} />
            ) : (
              <Copy style={{ height: '16px', width: '16px' }} />
            )}
          </button>
          <button 
            style={buttonStyle}
            onClick={resetCode}
            title="Reset to default"
          >
            <RotateCcw style={{ height: '16px', width: '16px' }} />
          </button>
        </div>
      </div>
      <div style={{ flex: 1 }}>
        <Editor
          height="100%"
          defaultLanguage="haskell"
          value={code}
          onChange={(value) => onChange(value || '')}
          theme="vs-dark"
          options={{
            fontSize: 14,
            minimap: { enabled: false },
            scrollBeyondLastLine: false,
            automaticLayout: true,
            fontFamily: 'Fira Code, Menlo, Monaco, Consolas, monospace',
            fontLigatures: true,
            lineNumbers: 'on',
            renderLineHighlight: 'all',
            cursorBlinking: 'smooth',
            cursorSmoothCaretAnimation: 'on',
            smoothScrolling: true,
            padding: { top: 10 }
          }}
        />
      </div>
    </div>
  )
} 