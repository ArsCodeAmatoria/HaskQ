'use client';

import { useEffect, useState } from 'react';
import { Sun } from 'lucide-react';
import { usePathname } from 'next/navigation';

export default function ThemeToggle() {
  const [isDarkMode, setIsDarkMode] = useState(true);
  const pathname = usePathname();

  useEffect(() => {
    // Always enforce dark mode
    setIsDarkMode(true);
    
    // Apply dark theme
    document.documentElement.classList.add('dark');
  }, [pathname]);

  return (
    <button
      className="p-2 rounded-md text-gray-500 dark:text-gray-400 opacity-50 cursor-not-allowed"
      aria-label="Dark theme enabled"
      disabled={true}
    >
      <Sun className="h-5 w-5" />
    </button>
  );
} 