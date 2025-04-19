/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx,mdx}',
    './components/**/*.{js,ts,jsx,tsx,mdx}',
    './app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    extend: {
      colors: {
        quantum: {
          blue: {
            50: '#e6f3ff',
            100: '#cce7ff',
            200: '#99cfff',
            300: '#66b7ff',
            400: '#339fff',
            500: '#0087ff',
            600: '#006ccc',
            700: '#005199',
            800: '#003666',
            900: '#001a33',
          },
          dark: {
            50: '#f2f2f2',
            100: '#e6e6e6',
            200: '#cccccc',
            300: '#b3b3b3',
            400: '#999999',
            500: '#808080',
            600: '#666666',
            700: '#4d4d4d',
            800: '#333333',
            900: '#1a1a1a',
            950: '#0d0d0d',
          }
        }
      },
      fontFamily: {
        sans: ['Inter', 'ui-sans-serif', 'system-ui'],
        mono: ['Fira Code', 'ui-monospace', 'SFMono-Regular'],
      },
      backgroundImage: {
        'quantum-gradient': 'linear-gradient(135deg, #0d0d0d 0%, #001a33 50%, #003666 100%)',
      },
    },
  },
  plugins: [],
} 