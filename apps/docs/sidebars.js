/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  // By default, Docusaurus generates a sidebar from the docs folder structure
  tutorialSidebar: [
    {
      type: 'category',
      label: 'Introduction',
      items: [
        'intro',
        'getting-started',
        'installation',
        'project-structure',
      ],
    },
    {
      type: 'category',
      label: 'Core Concepts',
      items: [
        'core-concepts/quantum-computing-basics',
        'core-concepts/linear-types',
        'core-concepts/circuit-composition',
        'core-concepts/quantum-gates',
        'core-concepts/measurement',
        'core-concepts/simulation',
        'core-concepts/error-correction',
      ],
    },
    {
      type: 'category',
      label: 'Tutorials',
      items: [
        'tutorials/bell-states',
        'tutorials/algorithms',
        'tutorials/grover',
        'tutorials/qft',
        'tutorials/advanced-algorithms',
        'tutorials/error-correction',
        'tutorials/fault-tolerance',
        'tutorials/surface-codes',
        'tutorials/noise-models',
        'tutorials/hybrid-algorithms',
        'tutorials/optimization',
      ],
    },
  ],
};

module.exports = sidebars;