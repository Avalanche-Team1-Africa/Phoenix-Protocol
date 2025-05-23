import type { NextConfig } from "next";
import withPWA from 'next-pwa';

// Configure PWA
const pwaConfig = {
  dest: 'public',
  register: true,
  skipWaiting: true,
  disable: process.env.NODE_ENV === 'development',
  sw: 'sw.js',
};

const nextConfig: NextConfig = {
  images: {
    formats: ['image/avif', 'image/webp'],
    remotePatterns: [
      {
        protocol: 'https',
        hostname: '**',
      },
    ],
  },
  experimental: {
    // Temporarily disable optimizeCss to avoid permission issues
    // optimizeCss: true,
    optimizePackageImports: ['lucide-react', '@radix-ui/react-icons'],
  },
  webpack: (config) => {
    // Enable tree-shaking for packages
    config.optimization.usedExports = true;
    
    // Add support for SVG files
    config.module.rules.push({
      test: /\.svg$/,
      use: ['@svgr/webpack'],
    });
    
    return config;
  },
};

// Apply PWA configuration
const buildConfig = withPWA(pwaConfig)(nextConfig);

export default buildConfig;
