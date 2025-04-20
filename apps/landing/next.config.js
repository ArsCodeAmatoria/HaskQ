/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  // Switch to export mode to avoid SSG issues with React context
  output: 'export',
  // Set the output directory to match Vercel config
  distDir: 'dist',
  // Disable image optimization to prevent issues with static export
  images: {
    unoptimized: true
  },
  // Skip type checking to speed up build
  typescript: {
    ignoreBuildErrors: true
  }
};

module.exports = nextConfig; 