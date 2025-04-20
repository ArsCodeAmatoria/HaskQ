// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const math = require('remark-math');
const katex = require('rehype-katex');

// Determine if we are in development or production
const isDev = process.env.NODE_ENV === 'development';

// Base URL configuration
const baseUrl = isDev ? 'http://localhost:' : 'https://haskq.vercel.app';
const landingPort = '3000';
const docsPort = '3000';
const playgroundPort = '3003';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'HaskQ',
  tagline: 'A linear quantum programming language',
  favicon: 'img/favicon.svg',

  // Set the production url of your site here
  url: 'https://haskq.vercel.app',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',
  trailingSlash: false,

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'ArsCodeAmatoria', // Usually your GitHub org/user name.
  projectName: 'HaskQ', // Usually your repo name.

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  scripts: [
    {
      src: 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-MML-AM_CHTML',
      async: true,
    },
  ],
  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css',
      type: 'text/css',
      integrity: 'sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X',
      crossorigin: 'anonymous',
    },
  ],

  plugins: [
    async function tailwindPlugin(context, options) {
      return {
        name: 'docusaurus-tailwindcss',
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require('tailwindcss'));
          postcssOptions.plugins.push(require('autoprefixer'));
          return postcssOptions;
        },
      };
    },
  ],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          // ... rest of the docs configuration ...
          remarkPlugins: [math],
          rehypePlugins: [katex],
          routeBasePath: '/',
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // Replace with your project's social card
      image: 'img/social-card.png',
      navbar: {
        title: 'HaskQ',
        hideOnScroll: false,
        items: [
          {
            href: isDev ? `http://localhost:${landingPort}` : 'https://haskq.vercel.app',
            label: 'Home',
            position: 'left',
            target: '_self',
          },
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Documentation',
          },
          {
            href: isDev ? `http://localhost:${playgroundPort}/playground` : 'https://haskq.vercel.app/playground',
            label: 'Playground',
            position: 'left',
            target: '_self',
          },
          {
            href: 'https://github.com/ArsCodeAmatoria/HaskQ',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Documentation',
            items: [
              {
                label: 'Getting Started',
                to: '/getting-started',
              },
              {
                label: 'Core Concepts',
                to: '/category/core-concepts',
              },
              {
                label: 'Tutorials',
                to: '/category/tutorials',
              },
            ],
          },
          {
            title: 'Resources',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/ArsCodeAmatoria/HaskQ',
              },
            ],
          },
          {
            title: 'Quantum Ecosystem',
            items: [
              {
                label: 'Quantum Circuits',
                href: 'https://quantumcircuits.com/',
              },
              {
                label: 'IBM Quantum',
                href: 'https://quantum-computing.ibm.com/',
              },
              {
                label: 'Qiskit',
                href: 'https://qiskit.org/',
              },
              {
                label: 'Quipper',
                href: 'https://www.mathstat.dal.ca/~selinger/quipper/',
              }
            ],
          },
          {
            title: 'Connect',
            items: [
              {
                html: `
                  <a href="https://github.com/ArsCodeAmatoria/HaskQ" target="_blank" rel="noopener noreferrer" class="footer__link-item" style="display: flex; align-items: center;">
                    <svg style="margin-right: 8px;" width="24" height="24" viewBox="0 0 24 24" fill="currentColor">
                      <path d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12" />
                    </svg>
                    GitHub
                  </a>
                `,
              },
              {
                html: `
                  <a href="https://x.com/ArsCodeAmatoria" target="_blank" rel="noopener noreferrer" class="footer__link-item" style="display: flex; align-items: center;">
                    <svg style="margin-right: 8px;" width="24" height="24" viewBox="0 0 24 24" fill="currentColor">
                      <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z" />
                    </svg>
                    Twitter
                  </a>
                `,
              },
              {
                html: `
                  <a href="https://discord.gg/quantum-computing" target="_blank" rel="noopener noreferrer" class="footer__link-item" style="display: flex; align-items: center;">
                    <svg style="margin-right: 8px;" width="24" height="24" viewBox="0 0 24 24" fill="currentColor">
                      <path d="M20.317 4.492c-1.53-.69-3.17-1.2-4.885-1.49a.075.075 0 0 0-.079.036c-.21.39-.444.885-.608 1.283a18.566 18.566 0 0 0-5.487 0 12.36 12.36 0 0 0-.617-1.283.077.077 0 0 0-.079-.036c-1.714.29-3.354.8-4.885 1.491a.07.07 0 0 0-.032.027C.533 9.093-.32 13.555.099 17.961a.08.08 0 0 0 .031.055 20.03 20.03 0 0 0 5.993 2.98.078.078 0 0 0 .084-.026c.462-.62.874-1.275 1.226-1.963.021-.04.001-.088-.041-.104a13.202 13.202 0 0 1-1.872-.878.075.075 0 0 1-.008-.125c.126-.093.252-.19.372-.287a.075.075 0 0 1 .078-.01c3.927 1.764 8.18 1.764 12.061 0a.075.075 0 0 1 .079.009c.12.098.245.195.372.288a.075.075 0 0 1-.006.125c-.598.344-1.22.635-1.873.877a.075.075 0 0 0-.041.105c.36.687.772 1.341 1.225 1.962a.077.077 0 0 0 .084.028 19.963 19.963 0 0 0 6.002-2.981.076.076 0 0 0 .032-.054c.5-5.094-.838-9.52-3.549-13.442a.06.06 0 0 0-.031-.028zM8.02 15.278c-1.182 0-2.157-1.069-2.157-2.38 0-1.312.956-2.38 2.157-2.38 1.21 0 2.176 1.077 2.157 2.38 0 1.312-.956 2.38-2.157 2.38zm7.975 0c-1.183 0-2.157-1.069-2.157-2.38 0-1.312.955-2.38 2.157-2.38 1.21 0 2.176 1.077 2.157 2.38 0 1.312-.946 2.38-2.157 2.38z" />
                    </svg>
                    Discord
                  </a>
                `,
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} HaskQ. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['haskell', 'bash'],
      },
    }),
};

module.exports = config; 