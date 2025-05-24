// Error types
export enum ErrorType {
  WALLET_CONNECTION = "wallet_connection",
  TRANSACTION = "transaction",
  NETWORK = "network",
  CONTRACT = "contract",
  USER_REJECTED = "user_rejected",
  VALIDATION = "validation",
  UNKNOWN = "unknown",
}

// Error severity
export enum ErrorSeverity {
  CRITICAL = "critical",
  HIGH = "high",
  MEDIUM = "medium",
  LOW = "low",
  INFO = "info",
}

// Error interface
export interface AppError {
  type: ErrorType;
  code: string;
  message: string;
  details?: string;
  severity: ErrorSeverity;
  recoverable: boolean;
  suggestedAction?: string;
  originalError?: any;
}

// Common error codes
export const ErrorCodes = {
  // Wallet errors
  WALLET_NOT_INSTALLED: "wallet_not_installed",
  WALLET_CONNECTION_REJECTED: "wallet_connection_rejected",
  WALLET_ALREADY_CONNECTED: "wallet_already_connected",
  WALLET_DISCONNECTED: "wallet_disconnected",
  WALLET_LOCKED: "wallet_locked",
  WALLET_TIMEOUT: "wallet_timeout",
  
  // Transaction errors
  TX_REJECTED: "tx_rejected",
  TX_FAILED: "tx_failed",
  TX_OUT_OF_GAS: "tx_out_of_gas",
  TX_UNDERPRICED: "tx_underpriced",
  TX_REPLACED: "tx_replaced",
  TX_DROPPED: "tx_dropped",
  
  // Network errors
  NETWORK_UNSUPPORTED: "network_unsupported",
  NETWORK_MISMATCH: "network_mismatch",
  NETWORK_CONGESTED: "network_congested",
  NETWORK_DISCONNECTED: "network_disconnected",
  
  // Contract errors
  CONTRACT_ERROR: "contract_error",
  CONTRACT_REVERTED: "contract_reverted",
  CONTRACT_NOT_FOUND: "contract_not_found",
  
  // Validation errors
  INVALID_ADDRESS: "invalid_address",
  INVALID_AMOUNT: "invalid_amount",
  INSUFFICIENT_BALANCE: "insufficient_balance",
  SLIPPAGE_TOO_HIGH: "slippage_too_high",
  
  // General errors
  UNKNOWN_ERROR: "unknown_error",
};

// Error messages map
const errorMessages: Record<string, string> = {
  // Wallet errors
  [ErrorCodes.WALLET_NOT_INSTALLED]: "Wallet extension is not installed",
  [ErrorCodes.WALLET_CONNECTION_REJECTED]: "Wallet connection was rejected",
  [ErrorCodes.WALLET_ALREADY_CONNECTED]: "Wallet is already connected",
  [ErrorCodes.WALLET_DISCONNECTED]: "Wallet was disconnected",
  [ErrorCodes.WALLET_LOCKED]: "Wallet is locked, please unlock it",
  [ErrorCodes.WALLET_TIMEOUT]: "Wallet connection timed out",
  
  // Transaction errors
  [ErrorCodes.TX_REJECTED]: "Transaction was rejected by the user",
  [ErrorCodes.TX_FAILED]: "Transaction failed to execute",
  [ErrorCodes.TX_OUT_OF_GAS]: "Transaction ran out of gas",
  [ErrorCodes.TX_UNDERPRICED]: "Transaction gas price is too low",
  [ErrorCodes.TX_REPLACED]: "Transaction was replaced by another transaction",
  [ErrorCodes.TX_DROPPED]: "Transaction was dropped from the mempool",
  
  // Network errors
  [ErrorCodes.NETWORK_UNSUPPORTED]: "Network is not supported",
  [ErrorCodes.NETWORK_MISMATCH]: "Network mismatch, please switch networks",
  [ErrorCodes.NETWORK_CONGESTED]: "Network is congested, transaction may be delayed",
  [ErrorCodes.NETWORK_DISCONNECTED]: "Network connection lost",
  
  // Contract errors
  [ErrorCodes.CONTRACT_ERROR]: "Smart contract error",
  [ErrorCodes.CONTRACT_REVERTED]: "Smart contract reverted the transaction",
  [ErrorCodes.CONTRACT_NOT_FOUND]: "Smart contract not found at the specified address",
  
  // Validation errors
  [ErrorCodes.INVALID_ADDRESS]: "Invalid address format",
  [ErrorCodes.INVALID_AMOUNT]: "Invalid amount",
  [ErrorCodes.INSUFFICIENT_BALANCE]: "Insufficient balance",
  [ErrorCodes.SLIPPAGE_TOO_HIGH]: "Slippage is too high",
  
  // General errors
  [ErrorCodes.UNKNOWN_ERROR]: "An unknown error occurred",
};

// Suggested actions map
const suggestedActions: Record<string, string> = {
  // Wallet errors
  [ErrorCodes.WALLET_NOT_INSTALLED]: "Install the wallet extension and refresh the page",
  [ErrorCodes.WALLET_CONNECTION_REJECTED]: "Try connecting again and approve the connection request",
  [ErrorCodes.WALLET_ALREADY_CONNECTED]: "You can continue using the application",
  [ErrorCodes.WALLET_DISCONNECTED]: "Reconnect your wallet to continue",
  [ErrorCodes.WALLET_LOCKED]: "Open your wallet extension and unlock it",
  [ErrorCodes.WALLET_TIMEOUT]: "Check your internet connection and try again",
  
  // Transaction errors
  [ErrorCodes.TX_REJECTED]: "Try submitting the transaction again if you want to proceed",
  [ErrorCodes.TX_FAILED]: "Check the transaction details and try again",
  [ErrorCodes.TX_OUT_OF_GAS]: "Increase the gas limit and try again",
  [ErrorCodes.TX_UNDERPRICED]: "Increase the gas price and try again",
  [ErrorCodes.TX_REPLACED]: "No action needed, your transaction was replaced",
  [ErrorCodes.TX_DROPPED]: "Submit the transaction again with a higher gas price",
  
  // Network errors
  [ErrorCodes.NETWORK_UNSUPPORTED]: "Switch to a supported network",
  [ErrorCodes.NETWORK_MISMATCH]: "Switch to the required network in your wallet",
  [ErrorCodes.NETWORK_CONGESTED]: "Wait for network congestion to decrease or increase gas price",
  [ErrorCodes.NETWORK_DISCONNECTED]: "Check your internet connection and try again",
  
  // Contract errors
  [ErrorCodes.CONTRACT_ERROR]: "Contact support for assistance",
  [ErrorCodes.CONTRACT_REVERTED]: "Check the transaction parameters and try again",
  [ErrorCodes.CONTRACT_NOT_FOUND]: "Verify the contract address and network",
  
  // Validation errors
  [ErrorCodes.INVALID_ADDRESS]: "Check the address format and try again",
  [ErrorCodes.INVALID_AMOUNT]: "Enter a valid amount and try again",
  [ErrorCodes.INSUFFICIENT_BALANCE]: "Add funds to your wallet or reduce the amount",
  [ErrorCodes.SLIPPAGE_TOO_HIGH]: "Increase the slippage tolerance or try a smaller amount",
};

// Error severity map
const errorSeverity: Record<string, ErrorSeverity> = {
  // Wallet errors
  [ErrorCodes.WALLET_NOT_INSTALLED]: ErrorSeverity.MEDIUM,
  [ErrorCodes.WALLET_CONNECTION_REJECTED]: ErrorSeverity.LOW,
  [ErrorCodes.WALLET_ALREADY_CONNECTED]: ErrorSeverity.INFO,
  [ErrorCodes.WALLET_DISCONNECTED]: ErrorSeverity.MEDIUM,
  [ErrorCodes.WALLET_LOCKED]: ErrorSeverity.MEDIUM,
  [ErrorCodes.WALLET_TIMEOUT]: ErrorSeverity.MEDIUM,
  
  // Transaction errors
  [ErrorCodes.TX_REJECTED]: ErrorSeverity.LOW,
  [ErrorCodes.TX_FAILED]: ErrorSeverity.HIGH,
  [ErrorCodes.TX_OUT_OF_GAS]: ErrorSeverity.HIGH,
  [ErrorCodes.TX_UNDERPRICED]: ErrorSeverity.MEDIUM,
  [ErrorCodes.TX_REPLACED]: ErrorSeverity.INFO,
  [ErrorCodes.TX_DROPPED]: ErrorSeverity.MEDIUM,
  
  // Network errors
  [ErrorCodes.NETWORK_UNSUPPORTED]: ErrorSeverity.HIGH,
  [ErrorCodes.NETWORK_MISMATCH]: ErrorSeverity.MEDIUM,
  [ErrorCodes.NETWORK_CONGESTED]: ErrorSeverity.MEDIUM,
  [ErrorCodes.NETWORK_DISCONNECTED]: ErrorSeverity.HIGH,
  
  // Contract errors
  [ErrorCodes.CONTRACT_ERROR]: ErrorSeverity.HIGH,
  [ErrorCodes.CONTRACT_REVERTED]: ErrorSeverity.HIGH,
  [ErrorCodes.CONTRACT_NOT_FOUND]: ErrorSeverity.CRITICAL,
  
  // Validation errors
  [ErrorCodes.INVALID_ADDRESS]: ErrorSeverity.MEDIUM,
  [ErrorCodes.INVALID_AMOUNT]: ErrorSeverity.MEDIUM,
  [ErrorCodes.INSUFFICIENT_BALANCE]: ErrorSeverity.HIGH,
  [ErrorCodes.SLIPPAGE_TOO_HIGH]: ErrorSeverity.MEDIUM,
};

// Recoverable errors
const recoverableErrors = [
  ErrorCodes.WALLET_CONNECTION_REJECTED,
  ErrorCodes.WALLET_DISCONNECTED,
  ErrorCodes.WALLET_LOCKED,
  ErrorCodes.WALLET_TIMEOUT,
  ErrorCodes.TX_REJECTED,
  ErrorCodes.TX_UNDERPRICED,
  ErrorCodes.TX_DROPPED,
  ErrorCodes.NETWORK_MISMATCH,
  ErrorCodes.NETWORK_CONGESTED,
  ErrorCodes.NETWORK_DISCONNECTED,
  ErrorCodes.INVALID_ADDRESS,
  ErrorCodes.INVALID_AMOUNT,
  ErrorCodes.SLIPPAGE_TOO_HIGH,
];

// Parse Ethereum provider errors
export function parseProviderError(error: any): AppError {
  // Default to unknown error
  let errorCode = ErrorCodes.UNKNOWN_ERROR;
  let errorType = ErrorType.UNKNOWN;
  
  // Extract error message
  const errorMessage = error?.message || "Unknown error occurred";
  
  // Check for common error patterns
  if (typeof error === "object") {
    // User rejected transaction
    if (
      error.code === 4001 || 
      errorMessage.includes("User denied") || 
      errorMessage.includes("User rejected")
    ) {
      errorCode = ErrorCodes.TX_REJECTED;
      errorType = ErrorType.USER_REJECTED;
    }
    // Wallet not installed
    else if (
      errorMessage.includes("not installed") || 
      errorMessage.includes("No provider found")
    ) {
      errorCode = ErrorCodes.WALLET_NOT_INSTALLED;
      errorType = ErrorType.WALLET_CONNECTION;
    }
    // Network errors
    else if (
      error.code === 4902 || 
      errorMessage.includes("Unrecognized chain ID") ||
      errorMessage.includes("network") ||
      errorMessage.includes("chain")
    ) {
      errorCode = ErrorCodes.NETWORK_UNSUPPORTED;
      errorType = ErrorType.NETWORK;
    }
    // Contract errors
    else if (
      errorMessage.includes("execution reverted") || 
      errorMessage.includes("revert") ||
      errorMessage.includes("UNPREDICTABLE_GAS_LIMIT")
    ) {
      errorCode = ErrorCodes.CONTRACT_REVERTED;
      errorType = ErrorType.CONTRACT;
    }
    // Gas errors
    else if (
      errorMessage.includes("gas") || 
      errorMessage.includes("fee")
    ) {
      if (errorMessage.includes("out of gas")) {
        errorCode = ErrorCodes.TX_OUT_OF_GAS;
      } else {
        errorCode = ErrorCodes.TX_UNDERPRICED;
      }
      errorType = ErrorType.TRANSACTION;
    }
    // Balance errors
    else if (
      errorMessage.includes("insufficient funds") || 
      errorMessage.includes("insufficient balance")
    ) {
      errorCode = ErrorCodes.INSUFFICIENT_BALANCE;
      errorType = ErrorType.VALIDATION;
    }
  }
  
  return {
    type: errorType,
    code: errorCode,
    message: errorMessages[errorCode] || errorMessage,
    details: errorMessage,
    severity: errorSeverity[errorCode] || ErrorSeverity.MEDIUM,
    recoverable: recoverableErrors.includes(errorCode),
    suggestedAction: suggestedActions[errorCode],
    originalError: error,
  };
}

// Create a custom error
export function createAppError(
  code: string,
  details?: string,
  originalError?: any
): AppError {
  return {
    type: getErrorTypeFromCode(code),
    code,
    message: errorMessages[code] || "An error occurred",
    details,
    severity: errorSeverity[code] || ErrorSeverity.MEDIUM,
    recoverable: recoverableErrors.includes(code),
    suggestedAction: suggestedActions[code],
    originalError,
  };
}

// Get error type from code
function getErrorTypeFromCode(code: string): ErrorType {
  if (code.startsWith("wallet_")) return ErrorType.WALLET_CONNECTION;
  if (code.startsWith("tx_")) return ErrorType.TRANSACTION;
  if (code.startsWith("network_")) return ErrorType.NETWORK;
  if (code.startsWith("contract_")) return ErrorType.CONTRACT;
  if (code === "user_rejected") return ErrorType.USER_REJECTED;
  if (
    code === "invalid_address" || 
    code === "invalid_amount" || 
    code === "insufficient_balance" || 
    code === "slippage_too_high"
  ) {
    return ErrorType.VALIDATION;
  }
  return ErrorType.UNKNOWN;
}