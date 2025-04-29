import { ethers } from "ethers";
import { getProvider, CHAIN_IDS } from "./providers";

// Standard ERC20 ABI for token interactions
const ERC20_ABI = [
  "function balanceOf(address owner) view returns (uint256)",
  "function decimals() view returns (uint8)",
  "function symbol() view returns (string)",
  "function transfer(address to, uint amount) returns (bool)",
  "function allowance(address owner, address spender) view returns (uint256)",
  "function approve(address spender, uint256 amount) returns (bool)",
  "event Transfer(address indexed from, address indexed to, uint amount)",
];

// Transaction types
export type TransactionRequest = {
  chainId: number;
  to: string;
  from: string;
  value?: string;
  data?: string;
  gasLimit?: string;
  gasPrice?: string;
};

export type TransactionResponse = {
  hash: string;
  chainId: number;
  from: string;
  to: string;
  value: string;
  data: string;
  gasLimit: string;
  gasPrice: string;
  status: "pending" | "confirmed" | "failed";
  blockNumber?: number;
  blockHash?: string;
  timestamp?: number;
};

// Send a transaction
export async function sendTransaction(
  request: TransactionRequest,
  signer: ethers.Signer
): Promise<TransactionResponse> {
  try {
    const tx = await signer.sendTransaction({
      to: request.to,
      value: request.value ? ethers.parseEther(request.value) : undefined,
      data: request.data,
      gasLimit: request.gasLimit ? BigInt(request.gasLimit) : undefined,
      gasPrice: request.gasPrice ? BigInt(request.gasPrice) : undefined,
    });
    
    const receipt = await tx.wait();
    
    return {
      hash: tx.hash,
      chainId: request.chainId,
      from: request.from,
      to: request.to,
      value: request.value || "0",
      data: request.data || "0x",
      gasLimit: receipt?.gasLimit.toString() || "0",
      gasPrice: receipt?.gasPrice?.toString() || "0",
      status: receipt?.status === 1 ? "confirmed" : "failed",
      blockNumber: receipt?.blockNumber,
      blockHash: receipt?.blockHash,
      timestamp: Date.now(),
    };
  } catch (error) {
    console.error("Transaction failed:", error);
    throw error;
  }
}

// Estimate gas for a transaction
export async function estimateGas(
  request: TransactionRequest
): Promise<{ gasLimit: string; gasPrice: string; totalCost: string }> {
  try {
    const provider = getProvider(request.chainId);
    if (!provider) {
      throw new Error(`Provider not available for chain ID ${request.chainId}`);
    }
    
    const gasLimit = await provider.estimateGas({
      to: request.to,
      from: request.from,
      value: request.value ? ethers.parseEther(request.value) : undefined,
      data: request.data,
    });
    
    const gasPrice = await provider.getFeeData();
    const totalCost = gasLimit * (gasPrice.gasPrice || 0n);
    
    return {
      gasLimit: gasLimit.toString(),
      gasPrice: (gasPrice.gasPrice || 0n).toString(),
      totalCost: ethers.formatEther(totalCost),
    };
  } catch (error) {
    console.error("Gas estimation failed:", error);
    throw error;
  }
}

// Get token balance
export async function getTokenBalance(
  tokenAddress: string | null,
  walletAddress: string,
  chainId: number
): Promise<{ balance: string; symbol: string; decimals: number }> {
  try {
    const provider = getProvider(chainId);
    if (!provider) {
      throw new Error(`Provider not available for chain ID ${chainId}`);
    }
    
    if (!tokenAddress) {
      // Native token (ETH, AVAX, etc.)
      const balance = await provider.getBalance(walletAddress);
      const metadata = {
        symbol: chainId === CHAIN_IDS.AVALANCHE_MAINNET || chainId === CHAIN_IDS.AVALANCHE_FUJI
          ? "AVAX"
          : chainId === CHAIN_IDS.CARDANO_MAINNET || chainId === CHAIN_IDS.CARDANO_TESTNET
          ? "ADA"
          : "ETH",
        decimals: chainId === CHAIN_IDS.CARDANO_MAINNET || chainId === CHAIN_IDS.CARDANO_TESTNET ? 6 : 18,
      };
      
      return {
        balance: ethers.formatUnits(balance, metadata.decimals),
        symbol: metadata.symbol,
        decimals: metadata.decimals,
      };
    } else {
      // ERC20 token
      const tokenContract = new ethers.Contract(tokenAddress, ERC20_ABI, provider);
      const balance = await tokenContract.balanceOf(walletAddress);
      const decimals = await tokenContract.decimals();
      const symbol = await tokenContract.symbol();
      
      return {
        balance: ethers.formatUnits(balance, decimals),
        symbol,
        decimals,
      };
    }
  } catch (error) {
    console.error("Error getting token balance:", error);
    throw error;
  }
}

// Transfer tokens
export async function transferTokens(
  tokenAddress: string | null,
  from: string,
  to: string,
  amount: string,
  chainId: number,
  signer: ethers.Signer
): Promise<TransactionResponse> {
  try {
    if (!tokenAddress) {
      // Native token transfer
      const request: TransactionRequest = {
        chainId,
        from,
        to,
        value: amount,
      };
      
      return sendTransaction(request, signer);
    } else {
      // ERC20 token transfer
      const tokenContract = new ethers.Contract(tokenAddress, ERC20_ABI, signer);
      const decimals = await tokenContract.decimals();
      const parsedAmount = ethers.parseUnits(amount, decimals);
      
      const tx = await tokenContract.transfer(to, parsedAmount);
      const receipt = await tx.wait();
      
      return {
        hash: tx.hash,
        chainId,
        from,
        to: tokenAddress,
        value: "0",
        data: tx.data || "0x",
        gasLimit: receipt?.gasLimit.toString() || "0",
        gasPrice: receipt?.gasPrice?.toString() || "0",
        status: receipt?.status === 1 ? "confirmed" : "failed",
        blockNumber: receipt?.blockNumber,
        blockHash: receipt?.blockHash,
        timestamp: Date.now(),
      };
    }
  } catch (error) {
    console.error("Token transfer failed:", error);
    throw error;
  }
}

// Get transaction by hash
export async function getTransaction(
  txHash: string,
  chainId: number
): Promise<TransactionResponse | null> {
  try {
    const provider = getProvider(chainId);
    if (!provider) {
      throw new Error(`Provider not available for chain ID ${chainId}`);
    }
    
    const tx = await provider.getTransaction(txHash);
    if (!tx) return null;
    
    const receipt = await provider.getTransactionReceipt(txHash);
    
    return {
      hash: tx.hash,
      chainId,
      from: tx.from,
      to: tx.to || "",
      value: tx.value ? ethers.formatEther(tx.value) : "0",
      data: tx.data,
      gasLimit: tx.gasLimit.toString(),
      gasPrice: tx.gasPrice?.toString() || "0",
      status: receipt?.status === 1 ? "confirmed" : receipt ? "failed" : "pending",
      blockNumber: receipt?.blockNumber,
      blockHash: receipt?.blockHash,
      timestamp: Date.now(),
    };
  } catch (error) {
    console.error("Error getting transaction:", error);
    return null;
  }
}