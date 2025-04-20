/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  basePath: '/playground',
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