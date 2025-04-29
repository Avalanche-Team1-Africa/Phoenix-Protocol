const Transaction = require('../models/Transaction');
const Intent = require('../models/Intent');
const { logger } = require('../utils/logger');

/**
 * Get user transactions
 */
exports.getUserTransactions = async (req, res, next) => {
  try {
    const page = parseInt(req.query.page) || 1;
    const limit = parseInt(req.query.limit) || 10;
    const skip = (page - 1) * limit;
    const status = req.query.status;
    const type = req.query.type;
    
    // Build query
    const query = { user: req.user._id };
    
    if (status) {
      query.status = status;
    }
    
    if (type) {
      query.type = type;
    }
    
    // Execute query
    const transactions = await Transaction.find(query)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit)
      .populate('intentId', 'type status');
    
    const total = await Transaction.countDocuments(query);
    
    res.status(200).json({
      transactions,
      pagination: {
        total,
        page,
        limit,
        pages: Math.ceil(total / limit)
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get transaction by ID
 */
exports.getTransactionById = async (req, res, next) => {
  try {
    const transaction = await Transaction.findById(req.params.id)
      .populate('intentId', 'type status parameters');
    
    if (!transaction) {
      return res.status(404).json({ 
        error: { 
          message: 'Transaction not found', 
          code: 'TRANSACTION_NOT_FOUND' 
        } 
      });
    }
    
    // Check if user has access to this transaction
    if (transaction.user.toString() !== req.user._id.toString()) {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    res.status(200).json({ transaction });
  } catch (error) {
    next(error);
  }
};

/**
 * Create a new transaction record
 */
exports.createTransaction = async (req, res, next) => {
  try {
    const {
      transactionHash,
      chainId,
      type,
      status,
      amount,
      tokenAddress,
      tokenSymbol,
      tokenDecimals,
      from,
      to,
      gasUsed,
      gasPrice,
      blockNumber,
      blockTimestamp,
      intentId,
      metadata
    } = req.body;
    
    // Validate required fields
    if (!transactionHash || !chainId || !type || !amount || !from || !to) {
      return res.status(400).json({ 
        error: { 
          message: 'Missing required fields', 
          code: 'MISSING_FIELDS' 
        } 
      });
    }
    
    // Check if transaction already exists
    const existingTransaction = await Transaction.findOne({ transactionHash });
    
    if (existingTransaction) {
      return res.status(400).json({ 
        error: { 
          message: 'Transaction already exists', 
          code: 'TRANSACTION_EXISTS' 
        } 
      });
    }
    
    // Create transaction
    const transaction = new Transaction({
      user: req.user._id,
      walletAddress: req.user.walletAddress,
      transactionHash,
      chainId,
      type,
      status: status || 'pending',
      amount,
      tokenAddress,
      tokenSymbol,
      tokenDecimals,
      from,
      to,
      gasUsed,
      gasPrice,
      blockNumber,
      blockTimestamp,
      intentId,
      metadata
    });
    
    await transaction.save();
    
    // If there's an intent ID, update the intent status
    if (intentId) {
      await Intent.findByIdAndUpdate(intentId, {
        $set: {
          status: 'executed',
          executedAt: new Date(),
          executedTxHash: transactionHash
        }
      });
    }
    
    res.status(201).json({ transaction });
  } catch (error) {
    next(error);
  }
};

/**
 * Update transaction status
 */
exports.updateTransaction = async (req, res, next) => {
  try {
    const { status, blockNumber, blockTimestamp, gasUsed, gasPrice } = req.body;
    
    // Find transaction
    const transaction = await Transaction.findById(req.params.id);
    
    if (!transaction) {
      return res.status(404).json({ 
        error: { 
          message: 'Transaction not found', 
          code: 'TRANSACTION_NOT_FOUND' 
        } 
      });
    }
    
    // Check if user has access to this transaction
    if (transaction.user.toString() !== req.user._id.toString()) {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    // Update transaction
    if (status) transaction.status = status;
    if (blockNumber) transaction.blockNumber = blockNumber;
    if (blockTimestamp) transaction.blockTimestamp = blockTimestamp;
    if (gasUsed) transaction.gasUsed = gasUsed;
    if (gasPrice) transaction.gasPrice = gasPrice;
    
    await transaction.save();
    
    res.status(200).json({ transaction });
  } catch (error) {
    next(error);
  }
};

/**
 * Get transactions by wallet address
 */
exports.getTransactionsByAddress = async (req, res, next) => {
  try {
    const address = req.params.address.toLowerCase();
    const page = parseInt(req.query.page) || 1;
    const limit = parseInt(req.query.limit) || 10;
    const skip = (page - 1) * limit;
    
    // Check if user has access to this address
    if (req.user.walletAddress.toLowerCase() !== address && req.user.role !== 'admin') {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    // Execute query
    const transactions = await Transaction.find({ walletAddress: address })
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit)
      .populate('intentId', 'type status');
    
    const total = await Transaction.countDocuments({ walletAddress: address });
    
    res.status(200).json({
      transactions,
      pagination: {
        total,
        page,
        limit,
        pages: Math.ceil(total / limit)
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get transaction by hash
 */
exports.getTransactionByHash = async (req, res, next) => {
  try {
    const hash = req.params.hash;
    
    const transaction = await Transaction.findOne({ transactionHash: hash })
      .populate('intentId', 'type status parameters');
    
    if (!transaction) {
      return res.status(404).json({ 
        error: { 
          message: 'Transaction not found', 
          code: 'TRANSACTION_NOT_FOUND' 
        } 
      });
    }
    
    // Check if user has access to this transaction
    if (transaction.user.toString() !== req.user._id.toString() && req.user.role !== 'admin') {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    res.status(200).json({ transaction });
  } catch (error) {
    next(error);
  }
};

/**
 * Verify transaction against intent
 */
exports.verifyTransaction = async (req, res, next) => {
  try {
    const { transactionData, intentId } = req.body;
    
    if (!transactionData || !intentId) {
      return res.status(400).json({ 
        error: { 
          message: 'Transaction data and intent ID are required', 
          code: 'MISSING_DATA' 
        } 
      });
    }
    
    // Find intent
    const intent = await Intent.findById(intentId);
    
    if (!intent) {
      return res.status(404).json({ 
        error: { 
          message: 'Intent not found', 
          code: 'INTENT_NOT_FOUND' 
        } 
      });
    }
    
    // Check if user has access to this intent
    if (intent.user.toString() !== req.user._id.toString()) {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    // Verify transaction against intent
    // This is a simplified implementation - in a real app, you would do more thorough validation
    const isValid = verifyTransactionAgainstIntent(transactionData, intent);
    
    if (!isValid.valid) {
      return res.status(200).json({
        valid: false,
        reason: isValid.reason,
        details: isValid.details
      });
    }
    
    res.status(200).json({
      valid: true,
      intent: {
        id: intent._id,
        type: intent.type,
        status: intent.status
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Helper function to verify transaction against intent
 */
function verifyTransactionAgainstIntent(transaction, intent) {
  // Check if intent is expired
  if (intent.status === 'expired') {
    return {
      valid: false,
      reason: 'INTENT_EXPIRED',
      details: 'The intent has expired'
    };
  }
  
  // Check if intent is already executed
  if (intent.status === 'executed') {
    return {
      valid: false,
      reason: 'INTENT_ALREADY_EXECUTED',
      details: 'The intent has already been executed'
    };
  }
  
  // Check if intent is cancelled
  if (intent.status === 'cancelled') {
    return {
      valid: false,
      reason: 'INTENT_CANCELLED',
      details: 'The intent has been cancelled'
    };
  }
  
  // Check if transaction type matches intent type
  if (transaction.type !== intent.type) {
    return {
      valid: false,
      reason: 'TYPE_MISMATCH',
      details: `Expected ${intent.type}, got ${transaction.type}`
    };
  }
  
  // Check if recipient matches
  if (transaction.to.toLowerCase() !== intent.recipient.toLowerCase()) {
    return {
      valid: false,
      reason: 'RECIPIENT_MISMATCH',
      details: `Expected ${intent.recipient}, got ${transaction.to}`
    };
  }
  
  // Check if token matches (if applicable)
  if (intent.tokenAddress && transaction.tokenAddress) {
    if (transaction.tokenAddress.toLowerCase() !== intent.tokenAddress.toLowerCase()) {
      return {
        valid: false,
        reason: 'TOKEN_MISMATCH',
        details: `Expected ${intent.tokenAddress}, got ${transaction.tokenAddress}`
      };
    }
  }
  
  // Check if amount is within acceptable range (allow some slippage for swaps)
  const intentAmount = parseFloat(intent.amount);
  const txAmount = parseFloat(transaction.amount);
  
  if (intent.type === 'swap') {
    // For swaps, allow up to 5% slippage
    const minAcceptable = intentAmount * 0.95;
    
    if (txAmount < minAcceptable) {
      return {
        valid: false,
        reason: 'AMOUNT_TOO_LOW',
        details: `Expected at least ${minAcceptable}, got ${txAmount}`
      };
    }
  } else {
    // For other transaction types, amount should match exactly
    if (txAmount !== intentAmount) {
      return {
        valid: false,
        reason: 'AMOUNT_MISMATCH',
        details: `Expected ${intentAmount}, got ${txAmount}`
      };
    }
  }
  
  // All checks passed
  return { valid: true };
}