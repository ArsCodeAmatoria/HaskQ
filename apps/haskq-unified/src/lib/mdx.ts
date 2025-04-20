// This file provides utilities for MDX processing if needed

import { serialize } from 'next-mdx-remote/serialize';
import rehypePrismPlus from 'rehype-prism-plus';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';

// Process MDX content
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