import { ethers } from "ethers";

// Chain IDs
export const CHAIN_IDS = {
  ETHEREUM_MAINNET: 1,
  ETHEREUM_SEPOLIA: 11155111,
  AVALANCHE_MAINNET: 43114,
  AVALANCHE_FUJI: 43113,
};

// RPC URLs - Replace with your actual API keys for production
export const RPC_URLS: Record<number, string> = {
  [CHAIN_IDS.ETHEREUM_MAINNET]: process.env.NEXT_PUBLIC_ETHEREUM_MAINNET_RPC || "https://eth-mainnet.g.alchemy.com/v2/your-api-key",
  [CHAIN_IDS.ETHEREUM_SEPOLIA]: process.env.NEXT_PUBLIC_ETHEREUM_SEPOLIA_RPC || "https://eth-sepolia.g.alchemy.com/v2/your-api-key",
  [CHAIN_IDS.AVALANCHE_MAINNET]: process.env.NEXT_PUBLIC_AVALANCHE_MAINNET_RPC || "https://api.avax.network/ext/bc/C/rpc",
  [CHAIN_IDS.AVALANCHE_FUJI]: process.env.NEXT_PUBLIC_AVALANCHE_FUJI_RPC || "https://api.avax-test.network/ext/bc/C/rpc",
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
};

// Contract addresses for each network
export const CONTRACT_ADDRESSES: Record<number, { 
  recoveryModule: string;
  intentRegistry: string;
  disputeResolutionDAO: string;
}> = {
  [CHAIN_IDS.ETHEREUM_MAINNET]: {
    recoveryModule: process.env.NEXT_PUBLIC_ETH_MAINNET_RECOVERY_MODULE || "0x0000000000000000000000000000000000000000",
    intentRegistry: process.env.NEXT_PUBLIC_ETH_MAINNET_INTENT_REGISTRY || "0x0000000000000000000000000000000000000000",
    disputeResolutionDAO: process.env.NEXT_PUBLIC_ETH_MAINNET_DISPUTE_DAO || "0x0000000000000000000000000000000000000000",
  },
  [CHAIN_IDS.ETHEREUM_SEPOLIA]: {
    recoveryModule: process.env.NEXT_PUBLIC_ETH_SEPOLIA_RECOVERY_MODULE || "0x0000000000000000000000000000000000000000",
    intentRegistry: process.env.NEXT_PUBLIC_ETH_SEPOLIA_INTENT_REGISTRY || "0x0000000000000000000000000000000000000000",
    disputeResolutionDAO: process.env.NEXT_PUBLIC_ETH_SEPOLIA_DISPUTE_DAO || "0x0000000000000000000000000000000000000000",
  },
  [CHAIN_IDS.AVALANCHE_MAINNET]: {
    recoveryModule: process.env.NEXT_PUBLIC_AVAX_MAINNET_RECOVERY_MODULE || "0x0000000000000000000000000000000000000000",
    intentRegistry: process.env.NEXT_PUBLIC_AVAX_MAINNET_INTENT_REGISTRY || "0x0000000000000000000000000000000000000000",
    disputeResolutionDAO: process.env.NEXT_PUBLIC_AVAX_MAINNET_DISPUTE_DAO || "0x0000000000000000000000000000000000000000",
  },
  [CHAIN_IDS.AVALANCHE_FUJI]: {
    recoveryModule: process.env.NEXT_PUBLIC_AVAX_FUJI_RECOVERY_MODULE || "0x0000000000000000000000000000000000000000",
    intentRegistry: process.env.NEXT_PUBLIC_AVAX_FUJI_INTENT_REGISTRY || "0x0000000000000000000000000000000000000000",
    disputeResolutionDAO: process.env.NEXT_PUBLIC_AVAX_FUJI_DISPUTE_DAO || "0x0000000000000000000000000000000000000000",
  },
};

// Get provider for a specific chain
export function getProvider(chainId: number): ethers.JsonRpcProvider | null {
  const rpcUrl = RPC_URLS[chainId];
  if (!rpcUrl) return null;
  
  return new ethers.JsonRpcProvider(rpcUrl);
}

// Get all supported providers
export function getAllProviders(): Record<number, ethers.JsonRpcProvider> {
  const providers: Record<number, ethers.JsonRpcProvider> = {};
  
  Object.keys(RPC_URLS).forEach((chainIdStr) => {
    const chainId = parseInt(chainIdStr);
    const provider = getProvider(chainId);
    if (provider) {
      providers[chainId] = provider;
    }
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

// Get contract addresses for a specific chain
export function getContractAddresses(chainId: number) {
  return CONTRACT_ADDRESSES[chainId] || null;
}