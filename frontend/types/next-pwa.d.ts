declare module 'next-pwa' {
  import { NextConfig } from 'next';
  
  export interface PWAConfig {
    dest?: string;
    register?: boolean;
    skipWaiting?: boolean;
    disable?: boolean;
    sw?: string;
    scope?: string;
    [key: string]: any;
  }
  
  export default function withPWA(config?: PWAConfig): (nextConfig: NextConfig) => NextConfig;
}