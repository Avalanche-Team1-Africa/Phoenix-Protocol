import { ethers } from "ethers";
import { CHAIN_IDS, isChainSupported, getChainMetadata } from "./providers";

// Wallet connection types
export type WalletType = "metamask" | "core";

export type WalletInfo = {
  address: string;
  chainId: number;
  connected: boolean;
  type: WalletType;
  provider?: any;
  signer?: ethers.Signer;
};

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
    
    let provider;
    
    switch (walletType) {
      case "metamask":
        if (!isMetaMaskInstalled()) {
          // If MetaMask is not installed, open the MetaMask download page
          window.open('https://metamask.io/download/', '_blank');
          throw new Error("MetaMask is not installed. Please install MetaMask and try again.");
        }
        provider = window.ethereum;
        break;
        
      case "core":
        if (!isCoreWalletInstalled()) {
          // If Core Wallet is not installed, open the Core Wallet download page
          window.open('https://core.app/download', '_blank');
          throw new Error("Core Wallet is not installed. Please install Core Wallet and try again.");
        }
        provider = window.ethereum;
        break;
        
      default:
        if (!window.ethereum) {
          throw new Error("No Ethereum provider found. Please install a wallet.");
        }
        provider = window.ethereum;
    }
    
    // Ensure the wallet is the one the user wants to connect with
    if (walletType === "metamask" && !isMetaMaskInstalled()) {
      throw new Error("MetaMask is not the active wallet. Please switch to MetaMask in your browser.");
    }
    
    if (walletType === "core" && !isCoreWalletInstalled()) {
      throw new Error("Core Wallet is not the active wallet. Please switch to Core Wallet in your browser.");
    }
    
    // Request account access - this will trigger the wallet popup
    const accounts = await provider.request({ method: "eth_requestAccounts" });
    const address = accounts[0];
    
    // Get current chain ID
    const chainIdHex = await provider.request({ method: "eth_chainId" });
    let chainId = parseInt(chainIdHex, 16);
    
    // Switch to preferred chain if needed and supported
    if (chainId !== preferredChainId && isChainSupported(preferredChainId)) {
      try {
        await provider.request({
          method: "wallet_switchEthereumChain",
          params: [{ chainId: `0x${preferredChainId.toString(16)}` }],
        });
        chainId = preferredChainId;
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
    const ethersProvider = new ethers.BrowserProvider(provider);
    const signer = await ethersProvider.getSigner();
    
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
export function disconnectWallet(): void {
  // In a real app, we would handle proper disconnection based on wallet type
  // For now, we just return and let the app state handle it
  return;
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