const ethers = require('ethers');
const config = require('../config');
const { logger } = require('./logger');

// Initialize provider
const getProvider = (chainId) => {
  // Default to the configured provider
  let providerUrl = config.BLOCKCHAIN_PROVIDER_URL;
  
  // Override based on chainId if needed
  if (chainId) {
    switch (chainId.toString()) {
      case '1':
        providerUrl = 'https://mainnet.infura.io/v3/your-infura-key';
        break;
      case '11155111':
        providerUrl = 'https://sepolia.infura.io/v3/your-infura-key';
        break;
      case '43114':
        providerUrl = 'https://api.avax.network/ext/bc/C/rpc';
        break;
      case '43113':
        providerUrl = 'https://api.avax-test.network/ext/bc/C/rpc';
        break;
      default:
        // Use default
        break;
    }
  }
  
  return new ethers.JsonRpcProvider(providerUrl);
};

/**
 * Get transaction details from blockchain
 */
const getTransaction = async (txHash, chainId) => {
  try {
    const provider = getProvider(chainId);
    const tx = await provider.getTransaction(txHash);
    
    if (!tx) {
      throw new Error('Transaction not found');
    }
    
    return tx;
  } catch (error) {
    logger.error(`Error getting transaction ${txHash}: ${error.message}`);
    throw error;
  }
};

/**
 * Get transaction receipt from blockchain
 */
const getTransactionReceipt = async (txHash, chainId) => {
  try {
    const provider = getProvider(chainId);
    const receipt = await provider.getTransactionReceipt(txHash);
    
    if (!receipt) {
      throw new Error('Transaction receipt not found');
    }
    
    return receipt;
  } catch (error) {
    logger.error(`Error getting transaction receipt ${txHash}: ${error.message}`);
    throw error;
  }
};

/**
 * Verify message signature
 */
const verifySignature = (message, signature, expectedAddress) => {
  try {
    const recoveredAddress = ethers.verifyMessage(message, signature);
    return recoveredAddress.toLowerCase() === expectedAddress.toLowerCase();
  } catch (error) {
    logger.error(`Error verifying signature: ${error.message}`);
    return false;
  }
};

/**
 * Format address for display
 */
const formatAddress = (address, start = 6, end = 4) => {
  if (!address) return '';
  return `${address.substring(0, start)}...${address.substring(address.length - end)}`;
};

/**
 * Get token balance
 */
const getTokenBalance = async (tokenAddress, walletAddress, chainId) => {
  try {
    const provider = getProvider(chainId);
    
    // ERC20 token ABI (minimal for balanceOf)
    const abi = [
      'function balanceOf(address owner) view returns (uint256)',
      'function decimals() view returns (uint8)',
      'function symbol() view returns (string)'
    ];
    
    const tokenContract = new ethers.Contract(tokenAddress, abi, provider);
    
    const balance = await tokenContract.balanceOf(walletAddress);
    const decimals = await tokenContract.decimals();
    const symbol = await tokenContract.symbol();
    
    return {
      balance: balance.toString(),
      decimals,
      symbol,
      formatted: ethers.formatUnits(balance, decimals)
    };
  } catch (error) {
    logger.error(`Error getting token balance: ${error.message}`);
    throw error;
  }
};

/**
 * Get native token balance (ETH, AVAX, etc.)
 */
const getNativeBalance = async (walletAddress, chainId) => {
  try {
    const provider = getProvider(chainId);
    const balance = await provider.getBalance(walletAddress);
    
    return {
      balance: balance.toString(),
      decimals: 18,
      symbol: getChainNativeSymbol(chainId),
      formatted: ethers.formatEther(balance)
    };
  } catch (error) {
    logger.error(`Error getting native balance: ${error.message}`);
    throw error;
  }
};

/**
 * Get chain native token symbol
 */
const getChainNativeSymbol = (chainId) => {
  switch (chainId.toString()) {
    case '1':
    case '11155111':
      return 'ETH';
    case '43114':
    case '43113':
      return 'AVAX';
    default:
      return 'ETH';
  }
};

/**
 * Get chain name
 */
const getChainName = (chainId) => {
  switch (chainId.toString()) {
    case '1':
      return 'Ethereum Mainnet';
    case '11155111':
      return 'Sepolia Testnet';
    case '43114':
      return 'Avalanche C-Chain';
    case '43113':
      return 'Avalanche Fuji Testnet';
    default:
      return `Chain ID: ${chainId}`;
  }
};

module.exports = {
  getProvider,
  getTransaction,
  getTransactionReceipt,
  verifySignature,
  formatAddress,
  getTokenBalance,
  getNativeBalance,
  getChainNativeSymbol,
  getChainName
};