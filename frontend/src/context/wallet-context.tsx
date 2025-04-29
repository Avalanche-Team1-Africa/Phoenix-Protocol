import React, { createContext, useContext, useState, useEffect, ReactNode } from "react";
import { ethers } from "ethers";

// Define wallet types
export type WalletType = "metamask" | "core" | "phantom" | "walletconnect" | "coinbase" | "brave" | "trust";

interface WalletInfo {
  address: string;
  chainId: number;
  balance: string;
  provider: any;
  connected: boolean;
  walletType: WalletType | null;
}

interface WalletContextType {
  wallet: WalletInfo;
  connect: (walletType: WalletType) => Promise<boolean>;
  disconnect: () => void;
  isConnecting: boolean;
  error: string | null;
  switchChain: (chainId: number) => Promise<boolean>;
}

const initialWalletState: WalletInfo = {
  address: "",
  chainId: 0,
  balance: "0",
  provider: null,
  connected: false,
  walletType: null,
};

const WalletContext = createContext<WalletContextType>({
  wallet: initialWalletState,
  connect: async () => false,
  disconnect: () => {},
  isConnecting: false,
  error: null,
  switchChain: async () => false,
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
      const ethersProvider = new ethers.BrowserProvider(provider);
      const network = await ethersProvider.getNetwork();
      const chainId = Number(network.chainId);
      const balanceWei = await ethersProvider.getBalance(address);
      const balance = ethers.formatEther(balanceWei);

      setWallet({
        address,
        chainId,
        balance,
        provider,
        connected: true,
        walletType,
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
      let provider;
      
      switch (walletType) {
        case "metamask":
          if (!window.ethereum) {
            throw new Error("MetaMask is not installed");
          }
          provider = window.ethereum;
          break;
          
        case "core":
          if (!window.avalanche) {
            throw new Error("Core Wallet is not installed");
          }
          provider = window.avalanche;
          break;
          
        case "phantom":
          if (!window.phantom?.solana) {
            throw new Error("Phantom wallet is not installed");
          }
          // For Phantom (Solana), we'd need a different approach
          // This is a simplified example
          provider = window.phantom.solana;
          break;
          
        case "walletconnect":
          // WalletConnect implementation would go here
          throw new Error("WalletConnect integration not implemented yet");
          
        case "coinbase":
          if (!window.coinbaseWalletExtension) {
            throw new Error("Coinbase Wallet is not installed");
          }
          provider = window.coinbaseWalletExtension;
          break;
          
        default:
          throw new Error(`Unsupported wallet type: ${walletType}`);
      }
      
      // Request account access
      const accounts = await provider.request({ method: "eth_requestAccounts" });
      const address = accounts[0];
      
      await updateWalletInfo(address, provider, walletType);
      
      setIsConnecting(false);
      return true;
    } catch (error: any) {
      console.error("Wallet connection error:", error);
      setError(error.message || "Failed to connect wallet");
      setIsConnecting(false);
      return false;
    }
  };

  const disconnect = () => {
    setWallet(initialWalletState);
    localStorage.removeItem("phoenixWalletType");
  };

  const switchChain = async (chainId: number): Promise<boolean> => {
    if (!wallet.provider) {
      setError("Wallet not connected");
      return false;
    }

    try {
      // Convert chainId to hex
      const chainIdHex = `0x${chainId.toString(16)}`;
      
      await wallet.provider.request({
        method: "wallet_switchEthereumChain",
        params: [{ chainId: chainIdHex }],
      });
      
      return true;
    } catch (error: any) {
      // If the chain hasn't been added to the user's wallet
      if (error.code === 4902) {
        // Implementation to add the chain would go here
        setError("This chain needs to be added to your wallet first");
      } else {
        console.error("Error switching chain:", error);
        setError(error.message || "Failed to switch chain");
      }
      return false;
    }
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
    phantom?: {
      solana?: any;
    };
    coinbaseWalletExtension?: any;
  }
}