/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './src/pages/**/*.{js,ts,jsx,tsx,mdx}',
    './src/components/**/*.{js,ts,jsx,tsx,mdx}',
    './src/app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  darkMode: ['class', '[data-theme="dark"]'],
  theme: {
    extend: {
      colors: {
        background: '#111827',
        foreground: '#f3f4f6',
        card: '#1f2937',
        border: '#374151',
        primary: {
          50: '#e7e9ff',
          100: '#c5caff',
          200: '#a3abff',
          300: '#818cff',
          400: '#5f6dff',
          500: '#4f5cd7',
          600: '#3e4baf',
          700: '#2e3a87',
          800: '#1e295f',
          900: '#0f1837',
        },
      },
      animation: {
        'fade-in': 'fadeIn 0.8s ease-out forwards',
        'fade-in-up': 'fadeInUp 0.5s ease-out forwards',
        'spin-slow': 'spin 15s linear infinite',
        'spin-slower': 'spin 20s linear infinite',
        'spin-reverse': 'spin 10s linear infinite reverse',
      },
      keyframes: {
        fadeIn: {
          '0%': { opacity: '0.01' },
          '100%': { opacity: '1' },
        },
        fadeInUp: {
          '0%': { 
            opacity: '0.01',
            transform: 'translateY(20px)'
          },
          '100%': { 
            opacity: '1',
            transform: 'translateY(0)'
          },
        },
      },
      typography: (theme) => ({
        DEFAULT: {
          css: {
            maxWidth: 'none',
            color: theme('colors.gray.300'),
            a: {
              color: theme('colors.indigo.400'),
              '&:hover': {
                color: theme('colors.indigo.300'),
              },
            },
            'h1, h2, h3, h4, h5, h6': {
              color: theme('colors.gray.100'),
              marginTop: theme('spacing.10'),
              marginBottom: theme('spacing.4'),
            },
            code: {
              color: theme('colors.indigo.300'),
              backgroundColor: theme('colors.gray.800'),
              borderRadius: theme('borderRadius.md'),
              padding: `${theme('spacing.1')} ${theme('spacing.1.5')}`,
            },
          },
        },
      }),
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}; 