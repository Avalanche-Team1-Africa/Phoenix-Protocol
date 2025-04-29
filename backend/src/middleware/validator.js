/**
 * Validation middleware for request data
 */

/**
 * Validate wallet address
 */
const validateWalletAddress = (req, res, next) => {
  const { walletAddress } = req.body;
  
  if (!walletAddress) {
    return res.status(400).json({ 
      error: { 
        message: 'Wallet address is required', 
        code: 'MISSING_WALLET_ADDRESS' 
      } 
    });
  }
  
  // Check if address is valid Ethereum address
  const addressRegex = /^0x[a-fA-F0-9]{40}$/;
  if (!addressRegex.test(walletAddress)) {
    return res.status(400).json({ 
      error: { 
        message: 'Invalid wallet address format', 
        code: 'INVALID_WALLET_ADDRESS' 
      } 
    });
  }
  
  next();
};

/**
 * Validate email
 */
const validateEmail = (req, res, next) => {
  const { email } = req.body;
  
  if (!email) {
    return res.status(400).json({ 
      error: { 
        message: 'Email is required', 
        code: 'MISSING_EMAIL' 
      } 
    });
  }
  
  // Check if email is valid
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(email)) {
    return res.status(400).json({ 
      error: { 
        message: 'Invalid email format', 
        code: 'INVALID_EMAIL' 
      } 
    });
  }
  
  next();
};

/**
 * Validate password
 */
const validatePassword = (req, res, next) => {
  const { password } = req.body;
  
  if (!password) {
    return res.status(400).json({ 
      error: { 
        message: 'Password is required', 
        code: 'MISSING_PASSWORD' 
      } 
    });
  }
  
  // Check if password meets requirements
  if (password.length < 8) {
    return res.status(400).json({ 
      error: { 
        message: 'Password must be at least 8 characters long', 
        code: 'PASSWORD_TOO_SHORT' 
      } 
    });
  }
  
  // Check for complexity (at least one uppercase, one lowercase, one number)
  const passwordRegex = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d).+$/;
  if (!passwordRegex.test(password)) {
    return res.status(400).json({ 
      error: { 
        message: 'Password must contain at least one uppercase letter, one lowercase letter, and one number', 
        code: 'PASSWORD_TOO_WEAK' 
      } 
    });
  }
  
  next();
};

/**
 * Validate transaction hash
 */
const validateTransactionHash = (req, res, next) => {
  const { transactionHash } = req.body;
  
  if (!transactionHash) {
    return res.status(400).json({ 
      error: { 
        message: 'Transaction hash is required', 
        code: 'MISSING_TRANSACTION_HASH' 
      } 
    });
  }
  
  // Check if hash is valid
  const hashRegex = /^0x[a-fA-F0-9]{64}$/;
  if (!hashRegex.test(transactionHash)) {
    return res.status(400).json({ 
      error: { 
        message: 'Invalid transaction hash format', 
        code: 'INVALID_TRANSACTION_HASH' 
      } 
    });
  }
  
  next();
};

/**
 * Validate chain ID
 */
const validateChainId = (req, res, next) => {
  const { chainId } = req.body;
  
  if (!chainId) {
    return res.status(400).json({ 
      error: { 
        message: 'Chain ID is required', 
        code: 'MISSING_CHAIN_ID' 
      } 
    });
  }
  
  // Check if chain ID is supported
  const supportedChains = ['1', '11155111', '43114', '43113'];
  if (!supportedChains.includes(chainId.toString())) {
    return res.status(400).json({ 
      error: { 
        message: 'Unsupported chain ID', 
        code: 'UNSUPPORTED_CHAIN' 
      } 
    });
  }
  
  next();
};

module.exports = {
  validateWalletAddress,
  validateEmail,
  validatePassword,
  validateTransactionHash,
  validateChainId
};