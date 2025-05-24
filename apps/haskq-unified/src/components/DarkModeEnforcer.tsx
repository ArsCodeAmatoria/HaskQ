'use client';

import { useEffect } from 'react';

export default function DarkModeEnforcer() {
  useEffect(() => {
    // Enforce dark mode immediately
    const enforceDarkMode = () => {
      document.documentElement.classList.add('dark');
      document.documentElement.style.colorScheme = 'dark';
      document.body.style.backgroundColor = '#111827';
      document.body.style.color = '#f3f4f6';
    };

    // Run enforcement immediately
    enforceDarkMode();

    // Set up a MutationObserver to watch for changes to the html element
    const observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        if (mutation.type === 'attributes' && mutation.attributeName === 'class') {
          const target = mutation.target as HTMLElement;
          if (target.tagName === 'HTML' && !target.classList.contains('dark')) {
            target.classList.add('dark');
          }
        }
      });
    });

    // Observe changes to the html element
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ['class', 'style']
    });

    // Also observe the body
    observer.observe(document.body, {
      attributes: true,
      attributeFilter: ['class', 'style']
    });

    // Override window.matchMedia to always return dark preference
    const originalMatchMedia = window.matchMedia;
    window.matchMedia = function(query) {
      const result = originalMatchMedia.call(this, query);
      if (query.includes('prefers-color-scheme')) {
        return {
          ...result,
          matches: query.includes('dark'),
          addEventListener: () => {},
          removeEventListener: () => {},
        };
      }
      return result;
    };

    // Cleanup
    return () => {
      observer.disconnect();
    };
  }, []);

  return null;
} 