'use client';

import React from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';

interface DocLayoutProps {
  children: React.ReactNode;
  title: string;
  description?: string;
}

export function DocLayout({ children, title, description }: DocLayoutProps) {
  const pathname = usePathname();
  
  return (
    <div className="flex flex-col min-h-screen">
      {/* Main content */}
      <main className="flex-grow pb-16">
        <div className="container mx-auto px-4 md:px-6 lg:px-8 max-w-5xl">
          <article className="prose dark:prose-invert max-w-full">
            <h1 className="text-3xl font-bold mb-4">{title}</h1>
            {description && (
              <p className="text-lg text-gray-600 dark:text-gray-300 mb-8 font-normal">
                {description}
              </p>
            )}
            <div className="markdown-content">
              {children}
            </div>
          </article>
          
          {/* Navigation links */}
          <div className="mt-16 pt-8 border-t border-gray-200 dark:border-gray-800 flex justify-between">
            <Link 
              href="/docs" 
              className="text-indigo-600 dark:text-indigo-400 hover:underline flex items-center"
            >
              ← Back to Documentation
            </Link>
          </div>
        </div>
      </main>
    </div>
  );
} 