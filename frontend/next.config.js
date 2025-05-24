/** @type {import('next').NextConfig} */
const nextConfig = {
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
    optimizePackageImports: ['lucide-react', '@radix-ui/react-icons'],
  },
  // Disable tracing to avoid permission issues
  generateBuildId: async () => {
    return 'phoenix-protocol-build';
  },
  output: 'standalone',
  poweredByHeader: false,
  // Disable telemetry to avoid trace file creation
  env: {
    NEXT_TELEMETRY_DISABLED: '1'
  },
  webpack: (config) => {
    // Add support for SVG files
    config.module.rules.push({
      test: /\.svg$/,
      use: ['@svgr/webpack'],
    });
    
    return config;
  },
};

module.exports = nextConfig;