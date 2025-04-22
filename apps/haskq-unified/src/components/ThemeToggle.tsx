'use client';

import { useEffect, useState } from 'react';
import { Sun, Moon } from 'lucide-react';
import { usePathname } from 'next/navigation';

export default function ThemeToggle() {
  const [isDarkMode, setIsDarkMode] = useState(false);
  const pathname = usePathname();

  // Determine if we're on landing page, docs, or playground
  const isAlwaysDarkPage = pathname === '/' || 
                            pathname.startsWith('/docs') ||
                            pathname.startsWith('/playground');

  useEffect(() => {
    // For always dark pages, enforce dark mode
    if (isAlwaysDarkPage) {
      setIsDarkMode(true);
      applyTheme(true);
      return;
    }

    // For other pages, check system preference or stored value on component mount
    const savedMode = localStorage.getItem('darkMode');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    
    const initialMode = savedMode ? savedMode === 'true' : prefersDark;
    setIsDarkMode(initialMode);
    
    // Apply initial theme
    applyTheme(initialMode);
  }, [isAlwaysDarkPage, pathname]);

  const toggleTheme = () => {
    // Prevent toggling for pages that should always be dark
    if (isAlwaysDarkPage) return;

    const newMode = !isDarkMode;
    setIsDarkMode(newMode);
    applyTheme(newMode);
    localStorage.setItem('darkMode', newMode.toString());
  };

  const applyTheme = (isDark: boolean) => {
    if (isDark) {
      document.documentElement.classList.add('dark');
    } else {
      document.documentElement.classList.remove('dark');
    }
  };

  return (
    <button
      onClick={toggleTheme}
      className={`p-2 rounded-md text-gray-500 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800 ${isAlwaysDarkPage ? 'opacity-50 cursor-not-allowed' : ''}`}
      aria-label={isDarkMode ? 'Switch to light theme' : 'Switch to dark theme'}
      disabled={isAlwaysDarkPage}
    >
      {isDarkMode ? (
        <Sun className="h-5 w-5" />
      ) : (
        <Moon className="h-5 w-5" />
      )}
    </button>
  );
} 