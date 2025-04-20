// This file will contain functions to process MDX files

import fs from 'fs';
import path from 'path';
import matter from 'gray-matter';
// Import client-side MDX compiler instead of RSC version
import { serialize } from 'next-mdx-remote/serialize';
import rehypePrismPlus from 'rehype-prism-plus';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';

// Base directory for docs content
const contentDirectory = path.join(process.cwd(), 'content');

// Define types for doc paths
type DocPath = {
  params: {
    slug: string[];
  };
};

// Get all doc paths from given directory with .md and .mdx extensions
export async function getAllDocPaths(directory: string = ''): Promise<DocPath[]> {
  const fullPath = path.join(contentDirectory, directory);
  
  // Check if directory exists
  if (!fs.existsSync(fullPath)) {
    return [];
  }
  
  const files = fs.readdirSync(fullPath);
  const paths: DocPath[] = [];
  
  for (const file of files) {
    const filePath = path.join(fullPath, file);
    const stat = fs.statSync(filePath);
    
    if (stat.isDirectory()) {
      const subPaths: DocPath[] = await getAllDocPaths(path.join(directory, file));
      paths.push(...subPaths);
    } else if (file.endsWith('.md') || file.endsWith('.mdx')) {
      const slug = file.replace(/\.mdx?$/, '');
      paths.push({
        params: {
          slug: directory ? [directory, slug].join('/').split('/') : [slug],
        },
      });
    }
  }
  
  return paths;
}

// Define type for the MDX file result
type MDXFile = {
  frontmatter: {
    title?: string;
    description?: string;
    [key: string]: any;
  };
  content: string;
  mdxSource?: any;
};

// Get MDX file by slug
export async function getMDXFileBySlug(slug: string): Promise<MDXFile> {
  // Determine if this is local file (from old docs) or not yet migrated 
  const oldDocsPath = path.join(process.cwd(), '../docs/docs', `${slug}.md`);
  const contentPath = path.join(contentDirectory, `${slug}.mdx`);
  
  let filePath = '';
  
  // Check which file exists - prefer the content directory (for migrated files)
  if (fs.existsSync(contentPath)) {
    filePath = contentPath;
  } else if (fs.existsSync(oldDocsPath)) {
    filePath = oldDocsPath;
  } else {
    // For now, return placeholder content if file doesn't exist
    const placeholderContent = `# ${slug.split('/').pop()?.replace(/-/g, ' ') || 'Documentation'}\n\nThis content is not yet available.`;
    
    const mdxSource = await serialize(placeholderContent, {
      parseFrontmatter: true,
      mdxOptions: {
        remarkPlugins: [remarkMath],
        rehypePlugins: [rehypePrismPlus, rehypeKatex],
      },
    });
    
    return {
      frontmatter: {
        title: slug.split('/').pop()?.replace(/-/g, ' ') || 'Documentation',
        description: `Documentation for ${slug}`,
      },
      content: placeholderContent,
      mdxSource,
    };
  }
  
  // Read file content
  const fileContents = fs.readFileSync(filePath, 'utf8');
  
  // Parse frontmatter and content
  const { data, content } = matter(fileContents);
  
  // Serialize MDX content
  const mdxSource = await serialize(content, {
    parseFrontmatter: true,
    mdxOptions: {
      remarkPlugins: [remarkMath],
      rehypePlugins: [rehypePrismPlus, rehypeKatex],
    },
  });
  
  return {
    frontmatter: {
      title: data.title || slug.split('/').pop()?.replace(/-/g, ' '),
      description: data.description || `Documentation for ${slug}`,
      ...data,
    },
    content,
    mdxSource,
  };
}

// Process MDX content is now handled during getMDXFileBySlug
export async function processMDX(content: string) {
  try {
    const mdxSource = await serialize(content, {
      parseFrontmatter: true,
      mdxOptions: {
        remarkPlugins: [remarkMath],
        rehypePlugins: [rehypePrismPlus, rehypeKatex],
      },
    });
    
    return {
      content: null, // No direct React component, will be used with MDXRemote
      mdxSource,
    };
  } catch (error) {
    console.error('Error processing MDX:', error);
    throw error;
  }
} 