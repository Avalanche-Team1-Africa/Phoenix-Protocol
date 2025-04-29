import { ethers } from "ethers";

// Chain IDs
export const CHAIN_IDS = {
  ETHEREUM_MAINNET: 1,
  ETHEREUM_SEPOLIA: 11155111,
  AVALANCHE_MAINNET: 43114,
  AVALANCHE_FUJI: 43113,
  CARDANO_MAINNET: 2, // Using 2 for Cardano to avoid collision with Ethereum
  CARDANO_TESTNET: 3,
};

// RPC URLs
export const RPC_URLS: Record<number, string> = {
  [CHAIN_IDS.ETHEREUM_MAINNET]: "https://eth-mainnet.g.alchemy.com/v2/demo",
  [CHAIN_IDS.ETHEREUM_SEPOLIA]: "https://eth-sepolia.g.alchemy.com/v2/demo",
  [CHAIN_IDS.AVALANCHE_MAINNET]: "https://api.avax.network/ext/bc/C/rpc",
  [CHAIN_IDS.AVALANCHE_FUJI]: "https://api.avax-test.network/ext/bc/C/rpc",
  // Cardano doesn't use RPC in the same way, but we'll include placeholders
  [CHAIN_IDS.CARDANO_MAINNET]: "https://cardano-mainnet.blockfrost.io/api/v0",
  [CHAIN_IDS.CARDANO_TESTNET]: "https://cardano-testnet.blockfrost.io/api/v0",
};

// Chain metadata
export const CHAIN_METADATA: Record<number, { name: string; nativeCurrency: { name: string; symbol: string; decimals: number } }> = {
  [CHAIN_IDS.ETHEREUM_MAINNET]: {
    name: "Ethereum Mainnet",
    nativeCurrency: { name: "Ether", symbol: "ETH", decimals: 18 },
  },
  [CHAIN_IDS.ETHEREUM_SEPOLIA]: {
    name: "Sepolia Testnet",
    nativeCurrency: { name: "Sepolia Ether", symbol: "ETH", decimals: 18 },
  },
  [CHAIN_IDS.AVALANCHE_MAINNET]: {
    name: "Avalanche C-Chain",
    nativeCurrency: { name: "Avalanche", symbol: "AVAX", decimals: 18 },
  },
  [CHAIN_IDS.AVALANCHE_FUJI]: {
    name: "Avalanche Fuji Testnet",
    nativeCurrency: { name: "Avalanche", symbol: "AVAX", decimals: 18 },
  },
  [CHAIN_IDS.CARDANO_MAINNET]: {
    name: "Cardano Mainnet",
    nativeCurrency: { name: "Cardano", symbol: "ADA", decimals: 6 },
  },
  [CHAIN_IDS.CARDANO_TESTNET]: {
    name: "Cardano Testnet",
    nativeCurrency: { name: "Cardano", symbol: "ADA", decimals: 6 },
  },
};

// Get provider for a specific chain
export function getProvider(chainId: number): ethers.JsonRpcProvider | null {
  // Cardano uses a different API structure, so we'll handle it separately
  if (chainId === CHAIN_IDS.CARDANO_MAINNET || chainId === CHAIN_IDS.CARDANO_TESTNET) {
    return null; // In a real app, we'd return a Cardano-specific provider
  }
  
  const rpcUrl = RPC_URLS[chainId];
  if (!rpcUrl) return null;
  
  return new ethers.JsonRpcProvider(rpcUrl);
}

// Get all supported providers
export function getAllProviders(): Record<number, ethers.JsonRpcProvider | null> {
  const providers: Record<number, ethers.JsonRpcProvider | null> = {};
  
  Object.keys(RPC_URLS).forEach((chainIdStr) => {
    const chainId = parseInt(chainIdStr);
    providers[chainId] = getProvider(chainId);
  });
  
  return providers;
}

// Check if a chain is supported
export function isChainSupported(chainId: number): boolean {
  return !!RPC_URLS[chainId];
}

// Get chain metadata
export function getChainMetadata(chainId: number) {
  return CHAIN_METADATA[chainId] || null;
}