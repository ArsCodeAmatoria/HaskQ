// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const math = require('remark-math');
const katex = require('rehype-katex');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'HaskQ Documentation',
  tagline: 'Quantum Circuits, Purely Functional',
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: 'https://hask-q.vercel.app',
  // Set the /<baseUrl>/ pathname under which your site is served
  baseUrl: '/',

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

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl: 'https://github.com/ArsCodeAmatoria/HaskQ/tree/main/apps/docs/',
          remarkPlugins: [math],
          rehypePlugins: [katex],
          routeBasePath: '/',
          // Set the home page document
          homePageId: 'intro',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl: 'https://github.com/ArsCodeAmatoria/HaskQ/tree/main/apps/docs/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.13.24/dist/katex.min.css',
      type: 'text/css',
      integrity: 'sha384-odtC+0UGzzFL/6PNoE8rX/SPcQDXBJ+uRepguP4QkPCm2LBxH3FA3y+fKSiJ+AmM',
      crossorigin: 'anonymous',
    },
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // Replace with your project's social card
      image: 'img/haskq-social-card.png',
      navbar: {
        title: 'HaskQ',
        logo: {
          alt: 'HaskQ Logo',
          src: 'img/logo.png',
        },
        items: [
          {
            to: '/',
            label: 'Documentation',
            position: 'left',
            activeBaseRegex: '/$',
          },
          {
            href: 'https://github.com/ArsCodeAmatoria/HaskQ',
            label: 'GitHub',
            position: 'right',
          },
          {
            href: 'https://hask-q.vercel.app/playground',
            label: 'Playground',
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
              {
                label: 'Playground',
                href: 'https://hask-q.vercel.app/playground',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} HaskQ. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['haskell'],
      },
    }),
};

module.exports = config; 