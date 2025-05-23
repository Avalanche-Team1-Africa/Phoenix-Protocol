"use client";

import React, { createContext, useContext, useState, useEffect, ReactNode } from "react";
import { ethers } from "ethers";
import { connectWallet, disconnectWallet, WalletType as BlockchainWalletType } from "@/lib/blockchain/wallet-connect";
import { CHAIN_IDS, getChainMetadata, RPC_URLS } from "@/lib/blockchain/providers";
import { getTokenBalance } from "@/lib/blockchain/transactions";

// Define wallet types - focusing only on EVM compatible wallets
export type WalletType = "metamask" | "walletconnect" | "coinbase" | "brave" | "trust";

interface WalletInfo {
  address: string;
  chainId: number;
  balance: string;
  provider: any;
  connected: boolean;
  walletType: WalletType | null;
  signer: ethers.Signer | null;
}

interface WalletContextType {
  wallet: WalletInfo;
  connect: (walletType: WalletType) => Promise<boolean>;
  disconnect: () => void;
  isConnecting: boolean;
  error: string | null;
  switchChain: (chainId: number) => Promise<boolean>;
  getSigner: () => ethers.Signer | null;
}

const initialWalletState: WalletInfo = {
  address: "",
  chainId: 0,
  balance: "0",
  provider: null,
  connected: false,
  walletType: null,
  signer: null
};

const WalletContext = createContext<WalletContextType>({
  wallet: initialWalletState,
  connect: async () => false,
  disconnect: () => {},
  isConnecting: false,
  error: null,
  switchChain: async () => false,
  getSigner: () => null
});

export const useWallet = () => useContext(WalletContext);

interface WalletProviderProps {
  children: ReactNode;
}

export const WalletProvider = ({ children }: WalletProviderProps) => {
  const [wallet, setWallet] = useState<WalletInfo>(initialWalletState);
  const [isConnecting, setIsConnecting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Check if wallet is already connected on mount
  useEffect(() => {
    const checkConnection = async () => {
      // Check localStorage for previous connection
      const savedWalletType = localStorage.getItem("phoenixWalletType") as WalletType | null;
      
      if (savedWalletType) {
        try {
          await connect(savedWalletType);
        } catch (error) {
          console.error("Failed to reconnect wallet:", error);
          localStorage.removeItem("phoenixWalletType");
        }
      }
    };
    
    checkConnection();
  }, []);

  // Handle account changes
  useEffect(() => {
    if (!wallet.provider) return;

    const handleAccountsChanged = async (accounts: string[]) => {
      if (accounts.length === 0) {
        // User disconnected their wallet
        disconnect();
      } else if (accounts[0] !== wallet.address) {
        // Account changed, update state
        updateWalletInfo(accounts[0], wallet.provider, wallet.walletType);
      }
    };

    const handleChainChanged = (chainIdHex: string) => {
      // Chain changed, reload the page as recommended by MetaMask
      window.location.reload();
    };

    // Add listeners
    if (window.ethereum) {
      window.ethereum.on("accountsChanged", handleAccountsChanged);
      window.ethereum.on("chainChanged", handleChainChanged);
    }

    // Cleanup listeners
    return () => {
      if (window.ethereum) {
        window.ethereum.removeListener("accountsChanged", handleAccountsChanged);
        window.ethereum.removeListener("chainChanged", handleChainChanged);
      }
    };
  }, [wallet.provider, wallet.address]);

  const updateWalletInfo = async (address: string, provider: any, walletType: WalletType | null) => {
    try {
      let chainId = wallet.chainId;
      let balance = "0";
      let signer = null;
      
      // Get network and balance
      if (provider) {
        const ethersProvider = new ethers.BrowserProvider(provider);
        const network = await ethersProvider.getNetwork();
        chainId = Number(network.chainId);
        
        // Get signer
        signer = await ethersProvider.getSigner();
        
        // Get token balance using our blockchain integration
        try {
          const balanceInfo = await getTokenBalance(null, address, chainId);
          balance = balanceInfo.balance;
        } catch (error) {
          console.error("Error getting token balance:", error);
          // Fallback to basic balance check
          const balanceWei = await ethersProvider.getBalance(address);
          balance = ethers.formatEther(balanceWei);
        }
      }

      setWallet({
        address,
        chainId,
        balance,
        provider,
        connected: true,
        walletType,
        signer
      });

      // Save wallet type to localStorage for persistence
      if (walletType) {
        localStorage.setItem("phoenixWalletType", walletType);
      }
    } catch (error) {
      console.error("Error updating wallet info:", error);
      setError("Failed to update wallet information");
    }
  };

  const connect = async (walletType: WalletType): Promise<boolean> => {
    setIsConnecting(true);
    setError(null);
    
    try {
      // Default to Avalanche Fuji testnet for development, can be switched later
      const defaultChainId = process.env.NODE_ENV === 'production' 
        ? CHAIN_IDS.AVALANCHE_MAINNET 
        : CHAIN_IDS.AVALANCHE_FUJI;
      
      // Use our blockchain integration library
      const walletInfo = await connectWallet(walletType as BlockchainWalletType, defaultChainId);
      
      // Update wallet state with the connected wallet info
      setWallet({
        address: walletInfo.address,
        chainId: walletInfo.chainId,
        balance: "0", // We'll fetch this in updateWalletInfo
        provider: walletInfo.provider,
        connected: walletInfo.connected,
        walletType: walletType,
        signer: null // We'll get this in updateWalletInfo
      });
      
      // Fetch additional wallet info like balance and signer
      if (walletInfo.connected) {
        await updateWalletInfo(walletInfo.address, walletInfo.provider, walletType);
      }
      
      setIsConnecting(false);
      return walletInfo.connected;
    } catch (error: any) {
      console.error("Wallet connection error:", error);
      setError(error.message || "Failed to connect wallet");
      setIsConnecting(false);
      return false;
    }
  };

  const disconnect = () => {
    // Use our blockchain integration to disconnect
    disconnectWallet();
    
    // Reset wallet state
    setWallet(initialWalletState);
    localStorage.removeItem("phoenixWalletType");
  };

  const switchChain = async (chainId: number): Promise<boolean> => {
    if (!wallet.provider) {
      setError("Wallet not connected");
      return false;
    }

    try {
      // Only allow Ethereum and Avalanche chains
      if (![
        CHAIN_IDS.ETHEREUM_MAINNET,
        CHAIN_IDS.ETHEREUM_SEPOLIA,
        CHAIN_IDS.AVALANCHE_MAINNET,
        CHAIN_IDS.AVALANCHE_FUJI
      ].includes(chainId)) {
        setError(`Chain ID ${chainId} is not supported by Phoenix Protocol`);
        return false;
      }
      
      // Get chain metadata
      const chainMetadata = getChainMetadata(chainId);
      if (!chainMetadata) {
        setError(`Chain ID ${chainId} is not supported`);
        return false;
      }
      
      // Convert chainId to hex
      const chainIdHex = `0x${chainId.toString(16)}`;
      
      try {
        // Try to switch to the chain
        await wallet.provider.request({
          method: "wallet_switchEthereumChain",
          params: [{ chainId: chainIdHex }],
        });
      } catch (switchError: any) {
        // If the chain hasn't been added to the user's wallet
        if (switchError.code === 4902) {
          // Add the chain to the wallet
          await wallet.provider.request({
            method: "wallet_addEthereumChain",
            params: [
              {
                chainId: chainIdHex,
                chainName: chainMetadata.name,
                nativeCurrency: chainMetadata.nativeCurrency,
                rpcUrls: [RPC_URLS[chainId]],
                blockExplorerUrls: [
                  chainId === CHAIN_IDS.ETHEREUM_MAINNET
                    ? "https://etherscan.io"
                    : chainId === CHAIN_IDS.ETHEREUM_SEPOLIA
                    ? "https://sepolia.etherscan.io"
                    : chainId === CHAIN_IDS.AVALANCHE_MAINNET
                    ? "https://snowtrace.io"
                    : "https://testnet.snowtrace.io",
                ],
              },
            ],
          });
        } else {
          throw switchError;
        }
      }
      
      // Update wallet info after chain switch
      await updateWalletInfo(wallet.address, wallet.provider, wallet.walletType);
      
      return true;
    } catch (error: any) {
      console.error("Error switching chain:", error);
      setError(error.message || "Failed to switch chain");
      return false;
    }
  };

  // Helper function to get the current signer
  const getSigner = (): ethers.Signer | null => {
    return wallet.signer;
  };

  return (
    <WalletContext.Provider
      value={{
        wallet,
        connect,
        disconnect,
        isConnecting,
        error,
        switchChain,
        getSigner
      }}
    >
      {children}
    </WalletContext.Provider>
  );
};

// Add type definitions for window object
declare global {
  interface Window {
    ethereum?: any;
    avalanche?: any;
    coinbaseWalletExtension?: any;
  }
}