import { ethers } from "ethers";
import { CHAIN_IDS, isChainSupported } from "./providers";

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

// Connect to wallet
export async function connectWallet(
  walletType: WalletType,
  preferredChainId: number = CHAIN_IDS.ETHEREUM_MAINNET
): Promise<WalletInfo> {
  try {
    // For EVM-compatible chains
    if (typeof window === "undefined" || !window.ethereum) {
      throw new Error("No Ethereum provider found. Please install a wallet.");
    }
    
    let provider;
    
    switch (walletType) {
      case "metamask":
        provider = window.ethereum;
        break;
      case "core":
        // Core wallet also uses window.ethereum
        provider = window.ethereum;
        break;
      default:
        provider = window.ethereum;
    }
    
    // Request account access
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
          // In a real app, we would add the chain to the wallet
          console.warn("Chain not added to wallet");
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