import { Metadata } from 'next';
import { MDXRemote } from 'next-mdx-remote/rsc';
import fs from 'fs';
import path from 'path';

export const metadata: Metadata = {
  title: 'Consciousness Algorithms | HaskQ Tutorials',
  description: 'Quantum algorithms for modeling consciousness phenomena, from Penrose-Hameroff microtubules to AGDEF consciousness dynamics.',
  openGraph: {
    title: 'Consciousness Algorithms - HaskQ',
    description: 'Explore quantum theories of consciousness with computational implementations.',
    type: 'article',
  },
};

async function getConsciousnessContent() {
  const filePath = path.join(process.cwd(), 'content', 'tutorials', 'consciousness-algorithms.mdx');
  const source = fs.readFileSync(filePath, 'utf8');
  return source;
}

export default async function ConsciousnessAlgorithmsPage() {
  const source = await getConsciousnessContent();
  
  return (
    <div className="max-w-4xl mx-auto py-8">
      <div className="prose prose-lg dark:prose-invert max-w-none">
        <MDXRemote source={source} />
      </div>
    </div>
  );
} 