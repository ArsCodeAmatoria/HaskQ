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
        'core-concepts/circ-monad',
        'core-concepts/gates',
        'core-concepts/measurements',
      ],
    },
    {
      type: 'category',
      label: 'Tutorials',
      items: [
        'tutorials/bell-states',
        'tutorials/quantum-teleportation',
        'tutorials/deutsch-algorithm',
        'tutorials/grover-search',
      ],
    },
    {
      type: 'category',
      label: 'API',
      items: [
        'api/core-types',
        'api/gates',
        'api/circuit',
        'api/measurement',
        'api/simulator',
      ],
    },
    {
      type: 'category',
      label: 'Advanced Topics',
      items: [
        'advanced/circuit-optimization',
        'advanced/qasm-export',
        'advanced/custom-gates',
      ],
    },
  ],
};

module.exports = sidebars;