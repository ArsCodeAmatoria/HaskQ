import nextPlugin from '@next/eslint-plugin-next';
import tseslint from 'typescript-eslint';

export default [
  {
    plugins: {
      '@next': nextPlugin,
      '@typescript-eslint': tseslint.plugin,
    },
    rules: {
      'react/no-unescaped-entities': 'off',
      '@next/next/no-html-link-for-pages': 'off',
      '@typescript-eslint/no-unused-vars': 'warn'
    },
  },
]; 