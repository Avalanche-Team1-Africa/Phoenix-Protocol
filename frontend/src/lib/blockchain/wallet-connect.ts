import { ethers } from "ethers";
import { CHAIN_IDS, isChainSupported, getChainMetadata } from "./providers";

// Wallet connection types
export type WalletType = "metamask" | "core" | "coinbase" | "walletconnect" | "trust" | "phantom";

export type WalletInfo = {
  address: string;
  chainId: number;
  connected: boolean;
  type: WalletType;
  provider?: any;
  signer?: ethers.Signer;
};

// WalletConnect provider
let walletConnectProvider: any = null;

// Detect if MetaMask is installed
function isMetaMaskInstalled(): boolean {
  return typeof window !== "undefined" && 
    window.ethereum !== undefined && 
    window.ethereum.isMetaMask === true;
}

// Detect if Core Wallet is installed
function isCoreWalletInstalled(): boolean {
  return typeof window !== "undefined" && 
    window.ethereum !== undefined && 
    (window.ethereum.isCoreWallet === true || 
     // Some versions of Core Wallet might use a different identifier
     window.avalanche !== undefined);
}

// Detect if Coinbase Wallet is installed
function isCoinbaseWalletInstalled(): boolean {
  return typeof window !== "undefined" && 
    window.ethereum !== undefined && 
    (window.ethereum.isCoinbaseWallet === true || 
     window.ethereum.providers?.some((p: any) => p.isCoinbaseWallet === true));
}

// Detect if Trust Wallet is installed
function isTrustWalletInstalled(): boolean {
  return typeof window !== "undefined" && 
    window.ethereum !== undefined && 
    window.ethereum.isTrust === true;
}

// Detect if Phantom Wallet is installed
function isPhantomWalletInstalled(): boolean {
  return typeof window !== "undefined" && 
    window.phantom !== undefined;
}

// Get the appropriate provider for the selected wallet
async function getWalletProvider(walletType: WalletType): Promise<any> {
  switch (walletType) {
    case "metamask":
      if (!isMetaMaskInstalled()) {
        window.open('https://metamask.io/download/', '_blank');
        throw new Error("MetaMask is not installed. Please install MetaMask and try again.");
      }
      
      // If multiple providers exist, find MetaMask specifically
      if (window.ethereum.providers) {
        const metaMaskProvider = window.ethereum.providers.find((p: any) => p.isMetaMask && !p.isBraveWallet);
        return metaMaskProvider || window.ethereum;
      }
      return window.ethereum;
      
    case "core":
      if (!isCoreWalletInstalled()) {
        window.open('https://core.app/download', '_blank');
        throw new Error("Core Wallet is not installed. Please install Core Wallet and try again.");
      }
      return window.ethereum;
      
    case "coinbase":
      if (!isCoinbaseWalletInstalled()) {
        window.open('https://www.coinbase.com/wallet/downloads', '_blank');
        throw new Error("Coinbase Wallet is not installed. Please install Coinbase Wallet and try again.");
      }
      
      // If multiple providers exist, find Coinbase specifically
      if (window.ethereum.providers) {
        const coinbaseProvider = window.ethereum.providers.find((p: any) => p.isCoinbaseWallet);
        return coinbaseProvider || window.ethereum;
      }
      return window.ethereum;
      
    case "trust":
      if (!isTrustWalletInstalled()) {
        window.open('https://trustwallet.com/download', '_blank');
        throw new Error("Trust Wallet is not installed. Please install Trust Wallet and try again.");
      }
      return window.ethereum;
      
    case "phantom":
      if (!isPhantomWalletInstalled()) {
        window.open('https://phantom.app/download', '_blank');
        throw new Error("Phantom Wallet is not installed. Please install Phantom Wallet and try again.");
      }
      return window.phantom?.ethereum || window.ethereum;
      
    case "walletconnect":
      try {
        // Dynamically import WalletConnect
        const WalletConnectProvider = (await import('@walletconnect/web3-provider')).default;
        
        // Initialize WalletConnect Provider
        walletConnectProvider = new WalletConnectProvider({
          rpc: {
            [CHAIN_IDS.ETHEREUM_MAINNET]: "https://eth-mainnet.g.alchemy.com/v2/demo",
            [CHAIN_IDS.ETHEREUM_SEPOLIA]: "https://eth-sepolia.g.alchemy.com/v2/demo",
            [CHAIN_IDS.AVALANCHE_MAINNET]: "https://api.avax.network/ext/bc/C/rpc",
            [CHAIN_IDS.AVALANCHE_FUJI]: "https://api.avax-test.network/ext/bc/C/rpc",
          },
          qrcodeModalOptions: {
            mobileLinks: ["metamask", "trust", "rainbow", "argent", "imtoken"],
          },
        });
        
        // Enable session (triggers QR Code modal)
        await walletConnectProvider.enable();
        return walletConnectProvider;
      } catch (error) {
        console.error("WalletConnect initialization error:", error);
        throw new Error("Failed to initialize WalletConnect. Please try again.");
      }
      
    default:
      if (!window.ethereum) {
        throw new Error("No Ethereum provider found. Please install a wallet.");
      }
      return window.ethereum;
  }
}

// Connect to wallet
export async function connectWallet(
  walletType: WalletType,
  preferredChainId: number = CHAIN_IDS.ETHEREUM_MAINNET
): Promise<WalletInfo> {
  try {
    // For EVM-compatible chains
    if (typeof window === "undefined") {
      throw new Error("Window object not available. Are you running in a browser?");
    }
    
    // Get the appropriate provider for the selected wallet
    const provider = await getWalletProvider(walletType);
    
    // Request account access - this will trigger the wallet popup
    let accounts;
    let address;
    
    if (walletType === "walletconnect") {
      // WalletConnect handles accounts differently
      accounts = provider.accounts;
      address = accounts[0];
    } else {
      accounts = await provider.request({ method: "eth_requestAccounts" });
      address = accounts[0];
    }
    
    // Get current chain ID
    let chainId;
    
    if (walletType === "walletconnect") {
      // WalletConnect handles chainId differently
      chainId = provider.chainId;
    } else {
      const chainIdHex = await provider.request({ method: "eth_chainId" });
      chainId = parseInt(chainIdHex, 16);
    }
    
    // Switch to preferred chain if needed and supported
    if (chainId !== preferredChainId && isChainSupported(preferredChainId)) {
      try {
        // WalletConnect doesn't support chain switching in the same way
        if (walletType !== "walletconnect") {
          await provider.request({
            method: "wallet_switchEthereumChain",
            params: [{ chainId: `0x${preferredChainId.toString(16)}` }],
          });
          chainId = preferredChainId;
        } else {
          // For WalletConnect, we need to inform the user to switch manually
          console.warn("Please switch networks manually in your WalletConnect wallet");
        }
      } catch (error: any) {
        // Chain not added to wallet
        if (error.code === 4902) {
          // Add the chain to the wallet
          const chainMetadata = getChainMetadata(preferredChainId);
          if (chainMetadata) {
            try {
              await provider.request({
                method: "wallet_addEthereumChain",
                params: [
                  {
                    chainId: `0x${preferredChainId.toString(16)}`,
                    chainName: chainMetadata.name,
                    nativeCurrency: chainMetadata.nativeCurrency,
                    rpcUrls: [CHAIN_IDS.AVALANCHE_FUJI === preferredChainId 
                      ? "https://api.avax-test.network/ext/bc/C/rpc" 
                      : "https://api.avax.network/ext/bc/C/rpc"],
                    blockExplorerUrls: [
                      preferredChainId === CHAIN_IDS.AVALANCHE_MAINNET
                        ? "https://snowtrace.io"
                        : "https://testnet.snowtrace.io",
                    ],
                  },
                ],
              });
              // Try switching again after adding
              await provider.request({
                method: "wallet_switchEthereumChain",
                params: [{ chainId: `0x${preferredChainId.toString(16)}` }],
              });
              chainId = preferredChainId;
            } catch (addError) {
              console.error("Failed to add chain to wallet:", addError);
            }
          }
        } else {
          console.error("Failed to switch chain:", error);
        }
      }
    }
    
    // Create ethers provider and signer
    let ethersProvider;
    let signer;
    
    if (walletType === "walletconnect") {
      // For WalletConnect, we use the provider directly
      ethersProvider = new ethers.BrowserProvider(provider);
    } else {
      ethersProvider = new ethers.BrowserProvider(provider);
    }
    
    try {
      signer = await ethersProvider.getSigner();
    } catch (error) {
      console.error("Failed to get signer:", error);
      // Continue without signer, we'll handle this in the wallet context
    }
    
    // Save the last connected wallet type to localStorage
    localStorage.setItem("phoenixLastWalletType", walletType);
    
    return {
      address,
      chainId,
      connected: true,
      type: walletType,
      provider: ethersProvider,
      signer,
    };
  } catch (error) {
    console.error("Wallet connection failed:", error);
    throw error;
  }
}

// Disconnect wallet
export async function disconnectWallet(walletType?: WalletType): Promise<void> {
  try {
    // Handle WalletConnect disconnect
    if (walletType === "walletconnect" && walletConnectProvider) {
      await walletConnectProvider.disconnect();
      walletConnectProvider = null;
    }
    
    // Remove from localStorage
    localStorage.removeItem("phoenixLastWalletType");
    localStorage.removeItem("phoenixWalletType");
    
    // For other wallets, we just let the app state handle it
    return;
  } catch (error) {
    console.error("Error disconnecting wallet:", error);
  }
}

// Sign message
export async function signMessage(
  message: string,
  signer: ethers.Signer
): Promise<string> {
  try {
    const signature = await signer.signMessage(message);
    return signature;
  } catch (error) {
    console.error("Message signing failed:", error);
    throw error;
  }
}

// Verify signature
export function verifySignature(
  message: string,
  signature: string,
  address: string
): boolean {
  try {
    const recoveredAddress = ethers.verifyMessage(message, signature);
    return recoveredAddress.toLowerCase() === address.toLowerCase();
  } catch (error) {
    console.error("Signature verification failed:", error);
    return false;
  }
}