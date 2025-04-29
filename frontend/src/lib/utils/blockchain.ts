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