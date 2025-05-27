'use client';

import { useEffect } from 'react';

export default function DarkModeEnforcer() {
  useEffect(() => {
    // Enforce dark mode immediately and aggressively
    const enforceDarkMode = () => {
      const html = document.documentElement;
      const body = document.body;
      
      // Set HTML attributes
      html.classList.add('dark');
      html.setAttribute('data-theme', 'dark');
      html.style.colorScheme = 'dark';
      html.style.backgroundColor = '#111827';
      html.style.color = '#f3f4f6';
      
      // Set body styles
      body.style.backgroundColor = '#111827';
      body.style.color = '#f3f4f6';
      
      // Override any white backgrounds that might slip through
      const whiteBgElements = document.querySelectorAll('.bg-white, [style*="background-color: white"], [style*="background: white"]');
      whiteBgElements.forEach(el => {
        (el as HTMLElement).style.backgroundColor = '#1f2937';
      });
      
      // Override any black text that might be hard to read
      const blackTextElements = document.querySelectorAll('.text-black, [style*="color: black"]');
      blackTextElements.forEach(el => {
        (el as HTMLElement).style.color = '#f3f4f6';
      });
    };

    // Run enforcement immediately
    enforceDarkMode();

    // Set up a MutationObserver to watch for changes to the DOM
    const observer = new MutationObserver((mutations) => {
      let needsEnforcement = false;
      
      mutations.forEach((mutation) => {
        if (mutation.type === 'attributes') {
          const target = mutation.target as HTMLElement;
          
          // Watch for changes to class or data-theme on html
          if (target.tagName === 'HTML') {
            if (mutation.attributeName === 'class' && !target.classList.contains('dark')) {
              needsEnforcement = true;
            }
            if (mutation.attributeName === 'data-theme' && target.getAttribute('data-theme') !== 'dark') {
              needsEnforcement = true;
            }
          }
          
          // Watch for style changes that might introduce light mode
          if (mutation.attributeName === 'style') {
            const style = target.style;
            if (style.backgroundColor === 'white' || style.backgroundColor === '#ffffff' || 
                style.backgroundColor === 'rgb(255, 255, 255)' || style.color === 'black' ||
                style.color === '#000000' || style.color === 'rgb(0, 0, 0)') {
              needsEnforcement = true;
            }
          }
        }
        
        // Watch for added nodes that might have light mode styling
        if (mutation.type === 'childList') {
          mutation.addedNodes.forEach(node => {
            if (node.nodeType === Node.ELEMENT_NODE) {
              const element = node as HTMLElement;
              if (element.classList?.contains('bg-white') || 
                  element.classList?.contains('text-black') ||
                  element.style?.backgroundColor === 'white' ||
                  element.style?.color === 'black') {
                needsEnforcement = true;
              }
            }
          });
        }
      });
      
      if (needsEnforcement) {
        enforceDarkMode();
      }
    });

    // Observe changes to the entire document
    observer.observe(document, {
      attributes: true,
      attributeFilter: ['class', 'style', 'data-theme'],
      childList: true,
      subtree: true
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

    // Override getComputedStyle to prevent light mode detection
    const originalGetComputedStyle = window.getComputedStyle;
    window.getComputedStyle = function(element, pseudoElement) {
      const styles = originalGetComputedStyle.call(this, element, pseudoElement);
      
      // Create a proxy to override specific properties
      return new Proxy(styles, {
        get(target, prop) {
          const value = (target as any)[prop];
          if (prop === 'backgroundColor' && value === 'rgb(255, 255, 255)') {
            return 'rgb(31, 41, 55)'; // gray-800
          }
          if (prop === 'color' && value === 'rgb(0, 0, 0)') {
            return 'rgb(243, 244, 246)'; // gray-100
          }
          return value;
        }
      });
    };

    // Add a global style that forces dark mode on all elements
    const forceStyleElement = document.createElement('style');
    forceStyleElement.textContent = `
      /* Force dark mode on everything */
      * {
        color-scheme: dark !important;
      }
      
      .bg-white {
        background-color: #1f2937 !important;
      }
      
      .text-black {
        color: #f3f4f6 !important;
      }
      
      .text-gray-900 {
        color: #f3f4f6 !important;
      }
      
      .border-gray-200 {
        border-color: #374151 !important;
      }
      
      .border-gray-300 {
        border-color: #4b5563 !important;
      }
      
      /* Override any inline styles that might force light mode */
      [style*="background-color: white"],
      [style*="background: white"],
      [style*="background-color: #ffffff"],
      [style*="background-color: rgb(255, 255, 255)"] {
        background-color: #1f2937 !important;
      }
      
      [style*="color: black"],
      [style*="color: #000000"],
      [style*="color: rgb(0, 0, 0)"] {
        color: #f3f4f6 !important;
      }
    `;
    document.head.appendChild(forceStyleElement);

    // Cleanup
    return () => {
      observer.disconnect();
      if (forceStyleElement.parentNode) {
        forceStyleElement.parentNode.removeChild(forceStyleElement);
      }
    };
  }, []);

  return null;
} 