import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/__docusaurus/debug',
    component: ComponentCreator('/__docusaurus/debug', 'd2c'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/config',
    component: ComponentCreator('/__docusaurus/debug/config', 'c76'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/content',
    component: ComponentCreator('/__docusaurus/debug/content', '3c5'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/globalData',
    component: ComponentCreator('/__docusaurus/debug/globalData', '778'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/metadata',
    component: ComponentCreator('/__docusaurus/debug/metadata', '375'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/registry',
    component: ComponentCreator('/__docusaurus/debug/registry', 'b85'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/routes',
    component: ComponentCreator('/__docusaurus/debug/routes', 'ac3'),
    exact: true
  },
  {
    path: '/',
    component: ComponentCreator('/', 'b35'),
    routes: [
      {
        path: '/',
        component: ComponentCreator('/', '96c'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/circuit-composition',
        component: ComponentCreator('/core-concepts/circuit-composition', '8f5'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/error-correction',
        component: ComponentCreator('/core-concepts/error-correction', '24a'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/linear-types',
        component: ComponentCreator('/core-concepts/linear-types', '20f'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/measurement',
        component: ComponentCreator('/core-concepts/measurement', '33f'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/quantum-computing-basics',
        component: ComponentCreator('/core-concepts/quantum-computing-basics', '789'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/quantum-gates',
        component: ComponentCreator('/core-concepts/quantum-gates', 'fe0'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/core-concepts/simulation',
        component: ComponentCreator('/core-concepts/simulation', '569'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/getting-started',
        component: ComponentCreator('/getting-started', 'c9b'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/installation',
        component: ComponentCreator('/installation', '2e7'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/playground',
        component: ComponentCreator('/playground', '309'),
        exact: true
      },
      {
        path: '/project-structure',
        component: ComponentCreator('/project-structure', '8d3'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/advanced-algorithms',
        component: ComponentCreator('/tutorials/advanced-algorithms', '9f9'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/algorithms',
        component: ComponentCreator('/tutorials/algorithms', '0fa'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/bell-states',
        component: ComponentCreator('/tutorials/bell-states', '954'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/error-correction',
        component: ComponentCreator('/tutorials/error-correction', '5de'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/fault-tolerance',
        component: ComponentCreator('/tutorials/fault-tolerance', '4f4'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/grover',
        component: ComponentCreator('/tutorials/grover', 'c6d'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/hybrid-algorithms',
        component: ComponentCreator('/tutorials/hybrid-algorithms', '433'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/noise-models',
        component: ComponentCreator('/tutorials/noise-models', '604'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/optimization',
        component: ComponentCreator('/tutorials/optimization', 'adc'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/qft',
        component: ComponentCreator('/tutorials/qft', '2fd'),
        exact: true,
        sidebar: "tutorialSidebar"
      },
      {
        path: '/tutorials/surface-codes',
        component: ComponentCreator('/tutorials/surface-codes', '7f8'),
        exact: true,
        sidebar: "tutorialSidebar"
      }
    ]
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];
