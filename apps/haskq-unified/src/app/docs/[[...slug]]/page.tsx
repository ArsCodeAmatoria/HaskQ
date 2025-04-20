import { notFound } from 'next/navigation';
import { getMDXFileBySlug, processMDX, getAllDocPaths } from '@/lib/mdx';
import Link from 'next/link';
import { Book, Code, ArrowRight } from 'lucide-react';

export async function generateStaticParams() {
  try {
    // Get all paths from the old docs directory
    // In a production setup, we'd scan our content directory
    return [
      { slug: ['intro'] },
      { slug: ['getting-started'] },
      { slug: ['installation'] },
      { slug: ['project-structure'] },
      { slug: ['core-concepts', 'quantum-computing-basics'] },
      { slug: ['core-concepts', 'linear-types'] },
      { slug: ['core-concepts', 'circuit-composition'] },
      { slug: ['core-concepts', 'quantum-gates'] },
      { slug: ['core-concepts', 'measurement'] },
      { slug: ['core-concepts', 'simulation'] },
      { slug: ['core-concepts', 'error-correction'] },
      { slug: ['tutorials', 'bell-states'] },
      { slug: ['tutorials', 'algorithms'] },
      { slug: ['tutorials', 'grover'] },
      { slug: ['tutorials', 'qft'] },
      { slug: ['tutorials', 'advanced-algorithms'] },
      { slug: ['tutorials', 'error-correction'] },
      { slug: ['tutorials', 'fault-tolerance'] },
      { slug: ['tutorials', 'surface-codes'] },
      { slug: ['tutorials', 'noise-models'] },
      { slug: ['tutorials', 'hybrid-algorithms'] },
      { slug: ['tutorials', 'optimization'] },
    ];
  } catch (error) {
    console.error('Error generating static paths:', error);
    return [];
  }
}

function DocsIndexPage() {
  const sections = [
    {
      title: 'Getting Started',
      description: 'Learn how to install HaskQ and set up your first quantum circuit',
      href: '/docs/getting-started',
      icon: <Book className="h-8 w-8 text-indigo-500" />,
    },
    {
      title: 'Core Concepts',
      description: 'Understand the fundamental concepts of quantum computing with HaskQ',
      href: '/docs/core-concepts/quantum-computing-basics',
      icon: <Code className="h-8 w-8 text-indigo-500" />,
    },
    {
      title: 'Tutorials',
      description: 'Step-by-step guides to building quantum circuits and algorithms',
      href: '/docs/tutorials/bell-states',
      icon: <ArrowRight className="h-8 w-8 text-indigo-500" />,
    },
  ];
  
  return (
    <div>
      <h1 className="text-3xl font-bold mb-6">HaskQ Documentation</h1>
      <p className="mb-8 text-gray-600 dark:text-gray-300 max-w-3xl">
        Welcome to the HaskQ documentation. Here you'll find comprehensive guides and documentation to help you start working with HaskQ as quickly as possible.
      </p>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-12">
        {sections.map((section) => (
          <Link 
            key={section.href} 
            href={section.href}
            className="flex flex-col h-full bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-shadow border border-gray-100 dark:border-gray-700"
          >
            <div className="mb-4">{section.icon}</div>
            <h2 className="text-xl font-semibold text-indigo-600 dark:text-indigo-400 mb-2">{section.title}</h2>
            <p className="text-gray-600 dark:text-gray-300 flex-grow">{section.description}</p>
          </Link>
        ))}
      </div>
      
      <div className="bg-indigo-50 dark:bg-indigo-900/20 rounded-lg p-6 border border-indigo-100 dark:border-indigo-800">
        <h2 className="text-xl font-semibold mb-4">Getting Help</h2>
        <p className="mb-4">
          If you have questions or need help with HaskQ, there are several ways to get assistance:
        </p>
        <ul className="list-disc list-inside space-y-2 text-gray-700 dark:text-gray-300">
          <li>Check out the <Link href="/docs/getting-started" className="text-indigo-600 dark:text-indigo-400 hover:underline">Getting Started</Link> guide</li>
          <li>Browse the <Link href="/docs/core-concepts/quantum-computing-basics" className="text-indigo-600 dark:text-indigo-400 hover:underline">Core Concepts</Link> documentation</li>
          <li>Visit our <a href="https://github.com/ArsCodeAmatoria/HaskQ" className="text-indigo-600 dark:text-indigo-400 hover:underline" target="_blank" rel="noopener noreferrer">GitHub repository</a> to file issues or contribute</li>
        </ul>
      </div>
    </div>
  );
}

export default async function DocsPage({ params }: { params: { slug?: string[] } }) {
  // If no slug is provided, show the docs index page
  if (!params.slug || params.slug.length === 0) {
    return <DocsIndexPage />;
  }

  const slug = params.slug.join('/');
  
  try {
    // Get file content and frontmatter
    const { content, frontmatter } = await getMDXFileBySlug(slug);
    
    // Process the MDX content
    const mdxOutput = await processMDX(content);
    
    return (
      <article className="prose dark:prose-invert max-w-4xl">
        <h1 className="text-3xl font-bold mb-6">{frontmatter.title}</h1>
        {frontmatter.description && (
          <p className="text-lg text-gray-600 dark:text-gray-300 mb-8">
            {frontmatter.description}
          </p>
        )}
        
        <div className="markdown-content">
          {mdxOutput?.content}
        </div>
      </article>
    );
  } catch (error) {
    console.error('Error rendering doc page:', error);
    notFound();
  }
} 