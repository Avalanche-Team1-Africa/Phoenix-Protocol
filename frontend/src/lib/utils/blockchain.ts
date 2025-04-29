import { ethers } from "ethers";

/**
 * Formats an Ethereum address for display
 * @param address The full Ethereum address
 * @param startLength Number of characters to show at the start
 * @param endLength Number of characters to show at the end
 * @returns Formatted address string (e.g., 0x1234...5678)
 */
export function formatAddress(
  address: string,
  startLength = 6,
  endLength = 4
): string {
  if (!address) return "";
  if (!ethers.isAddress(address)) return address;
  
  return `${address.substring(0, startLength)}...${address.substring(
    address.length - endLength
  )}`;
}

/**
 * Formats an amount of tokens with proper decimal places
 * @param amount The token amount (in wei or smallest unit)
 * @param decimals Number of decimals for the token
 * @returns Formatted amount as a string
 */
export function formatTokenAmount(
  amount: string | number,
  decimals = 18
): string {
  try {
    const value = ethers.formatUnits(amount.toString(), decimals);
    // Remove trailing zeros
    return parseFloat(value).toString();
  } catch (error) {
    console.error("Error formatting token amount:", error);
    return "0";
  }
}

/**
 * Creates a human-readable transaction description
 * @param txType The type of transaction (swap, stake, transfer, etc.)
 * @param payload The transaction details
 * @returns A human-readable string describing the transaction
 */
export function createIntentDescription(
  txType: string,
  payload: Record<string, any>
): string {
  switch (txType.toLowerCase()) {
    case "swap":
      return `Swap ${formatTokenAmount(payload.amountIn)} ${
        payload.tokenInSymbol
      } to ${payload.tokenOutSymbol} with ${payload.slippage}% slippage`;
    
    case "transfer":
      return `Transfer ${formatTokenAmount(payload.amount)} ${
        payload.tokenSymbol
      } to ${formatAddress(payload.recipient)}`;
    
    case "stake":
      return `Stake ${formatTokenAmount(payload.amount)} ${
        payload.tokenSymbol
      } for ${payload.duration} days`;
    
    case "mint":
      return `Mint ${payload.quantity} ${payload.nftName} NFT${
        payload.quantity > 1 ? "s" : ""
      } for ${formatTokenAmount(payload.price)} ${payload.currencySymbol}`;
    
    default:
      return `Execute ${txType} transaction with the provided parameters`;
  }
}

/**
 * Mock function to simulate intent verification
 * In a real implementation, this would verify the transaction against the stored intent
 */
export function verifyTransactionMatchesIntent(
  intentId: string,
  txReceipt: any
): { matches: boolean; discrepancies: string[] } {
  // This is a mock implementation
  // In a real app, this would compare the transaction data with the stored intent
  
  // Randomly return a mismatch for demo purposes (10% chance)
  const randomMismatch = Math.random() < 0.1;
  
  if (randomMismatch) {
    return {
      matches: false,
      discrepancies: [
        "Token amount exceeds intended amount by 15%",
        "Recipient address does not match intended recipient",
      ],
    };
  }
  
  return {
    matches: true,
    discrepancies: [],
  };
}

/**
 * Get the chain name from a chain ID
 * @param chainId The numeric chain ID
 * @returns The human-readable chain name
 */
export function getChainName(chainId: number): string {
  const chains: Record<number, string> = {
    1: "Ethereum Mainnet",
    5: "Goerli Testnet",
    11155111: "Sepolia Testnet",
    43114: "Avalanche C-Chain",
    43113: "Avalanche Fuji Testnet",
    2: "Cardano Mainnet", // Using 2 for Cardano to avoid collision with Ethereum
    3: "Cardano Testnet",
  };
  
  return chains[chainId] || `Chain ID: ${chainId}`;
}

/**
 * Get the explorer URL for a transaction hash
 * @param txHash The transaction hash
 * @param chainId The chain ID
 * @returns The explorer URL
 */
export function getExplorerUrl(txHash: string, chainId: number): string {
  const explorers: Record<number, string> = {
    1: "https://etherscan.io/tx/",
    5: "https://goerli.etherscan.io/tx/",
    11155111: "https://sepolia.etherscan.io/tx/",
    43114: "https://snowtrace.io/tx/",
    43113: "https://testnet.snowtrace.io/tx/",
    2: "https://cardanoscan.io/transaction/", // Cardano
    3: "https://testnet.cardanoscan.io/transaction/", // Cardano testnet
  };
  
  const baseUrl = explorers[chainId] || "https://etherscan.io/tx/";
  return `${baseUrl}${txHash}`;
}

/**
 * Estimate gas for a transaction
 * @param provider The ethers provider
 * @param txParams The transaction parameters
 * @returns The estimated gas limit and price
 */
export async function estimateGas(
  provider: ethers.BrowserProvider,
  txParams: {
    to: string;
    from: string;
    value?: string;
    data?: string;
  }
): Promise<{ gasLimit: bigint; gasPrice: bigint; totalCost: string }> {
  try {
    const gasLimit = await provider.estimateGas({
      to: txParams.to,
      from: txParams.from,
      value: txParams.value ? ethers.parseEther(txParams.value) : undefined,
      data: txParams.data,
    });
    
    const gasPrice = await provider.getGasPrice();
    const totalCost = gasLimit * gasPrice;
    
    return {
      gasLimit,
      gasPrice,
      totalCost: ethers.formatEther(totalCost),
    };
  } catch (error) {
    console.error("Error estimating gas:", error);
    throw new Error("Failed to estimate gas for transaction");
  }
}

/**
 * Get token balance for an address
 * @param provider The ethers provider
 * @param tokenAddress The token contract address (or null for native token)
 * @param walletAddress The wallet address to check
 * @returns The token balance
 */
export async function getTokenBalance(
  provider: ethers.BrowserProvider,
  tokenAddress: string | null,
  walletAddress: string
): Promise<string> {
  try {
    if (!tokenAddress) {
      // Native token (ETH, AVAX, etc.)
      const balance = await provider.getBalance(walletAddress);
      return ethers.formatEther(balance);
    } else {
      // ERC20 token
      const erc20Abi = [
        "function balanceOf(address owner) view returns (uint256)",
        "function decimals() view returns (uint8)",
      ];
      
      const tokenContract = new ethers.Contract(tokenAddress, erc20Abi, provider);
      const balance = await tokenContract.balanceOf(walletAddress);
      const decimals = await tokenContract.decimals();
      
      return ethers.formatUnits(balance, decimals);
    }
  } catch (error) {
    console.error("Error getting token balance:", error);
    return "0";
  }
}