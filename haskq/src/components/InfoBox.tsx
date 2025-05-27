'use client';

import { ReactNode } from 'react';

type InfoBoxType = 'note' | 'warning' | 'tip';

interface InfoBoxProps {
  type: InfoBoxType;
  title?: string;
  children: ReactNode;
  className?: string;
}

export function InfoBox({ type, title, children, className = '' }: InfoBoxProps) {
  const defaultTitles = {
    note: 'Note',
    warning: 'Warning',
    tip: 'Tip',
  };

  const icons = {
    note: (
      <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5 text-blue-500 dark:text-blue-400" viewBox="0 0 20 20" fill="currentColor">
        <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
      </svg>
    ),
    warning: (
      <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5 text-amber-500 dark:text-amber-400" viewBox="0 0 20 20" fill="currentColor">
        <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
      </svg>
    ),
    tip: (
      <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5 text-green-500 dark:text-green-400" viewBox="0 0 20 20" fill="currentColor">
        <path d="M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM15.657 5.757a1 1 0 00-1.414-1.414l-.707.707a1 1 0 001.414 1.414l.707-.707zM18 10a1 1 0 01-1 1h-1a1 1 0 110-2h1a1 1 0 011 1zM5.05 6.464A1 1 0 106.464 5.05l-.707-.707a1 1 0 00-1.414 1.414l.707.707zM5 10a1 1 0 01-1 1H3a1 1 0 110-2h1a1 1 0 011 1zM8 16v-1h4v1a2 2 0 11-4 0zM12 14c.015-.34.208-.646.477-.859a4 4 0 10-4.954 0c.27.213.462.519.476.859h4.002z" />
      </svg>
    ),
  };

  return (
    <div className={`info-box ${type} ${className}`}>
      <div className="flex items-center gap-2 mb-2">
        {icons[type]}
        <h4 className="font-semibold text-lg">{title || defaultTitles[type]}</h4>
      </div>
      <div className="pl-7">{children}</div>
    </div>
  );
} 