const Intent = require('../models/Intent');
const { logger } = require('../utils/logger');

/**
 * Get user intents
 */
exports.getUserIntents = async (req, res, next) => {
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
    const intents = await Intent.find(query)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit);
    
    const total = await Intent.countDocuments(query);
    
    res.status(200).json({
      intents,
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
 * Get intent by ID
 */
exports.getIntentById = async (req, res, next) => {
  try {
    const intent = await Intent.findById(req.params.id);
    
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
    
    res.status(200).json({ intent });
  } catch (error) {
    next(error);
  }
};

/**
 * Create a new intent
 */
exports.createIntent = async (req, res, next) => {
  try {
    const {
      chainId,
      type,
      tokenAddress,
      tokenSymbol,
      amount,
      recipient,
      expiresAt,
      parameters,
      nonce
    } = req.body;
    
    // Validate required fields
    if (!chainId || !type || !amount || !recipient || !expiresAt || !nonce) {
      return res.status(400).json({ 
        error: { 
          message: 'Missing required fields', 
          code: 'MISSING_FIELDS' 
        } 
      });
    }
    
    // Create intent
    const intent = new Intent({
      user: req.user._id,
      walletAddress: req.user.walletAddress,
      chainId,
      type,
      tokenAddress,
      tokenSymbol,
      amount,
      recipient,
      expiresAt: new Date(expiresAt),
      parameters: parameters || {},
      nonce
    });
    
    await intent.save();
    
    res.status(201).json({ intent });
  } catch (error) {
    next(error);
  }
};

/**
 * Update intent status
 */
exports.updateIntent = async (req, res, next) => {
  try {
    const { status } = req.body;
    
    if (!status || !['pending', 'executed', 'expired', 'cancelled'].includes(status)) {
      return res.status(400).json({ 
        error: { 
          message: 'Invalid status', 
          code: 'INVALID_STATUS' 
        } 
      });
    }
    
    // Find intent
    const intent = await Intent.findById(req.params.id);
    
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
    
    // Update intent
    intent.status = status;
    
    if (status === 'executed') {
      intent.executedAt = new Date();
    }
    
    await intent.save();
    
    res.status(200).json({ intent });
  } catch (error) {
    next(error);
  }
};

/**
 * Cancel an intent
 */
exports.cancelIntent = async (req, res, next) => {
  try {
    // Find intent
    const intent = await Intent.findById(req.params.id);
    
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
    
    // Check if intent can be cancelled
    if (intent.status === 'executed') {
      return res.status(400).json({ 
        error: { 
          message: 'Cannot cancel executed intent', 
          code: 'INTENT_ALREADY_EXECUTED' 
        } 
      });
    }
    
    if (intent.status === 'cancelled') {
      return res.status(400).json({ 
        error: { 
          message: 'Intent already cancelled', 
          code: 'INTENT_ALREADY_CANCELLED' 
        } 
      });
    }
    
    // Cancel intent
    intent.status = 'cancelled';
    await intent.save();
    
    res.status(200).json({
      message: 'Intent cancelled successfully',
      intent
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get intents by wallet address
 */
exports.getIntentsByAddress = async (req, res, next) => {
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
    const intents = await Intent.find({ walletAddress: address })
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit);
    
    const total = await Intent.countDocuments({ walletAddress: address });
    
    res.status(200).json({
      intents,
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
 * Execute an intent
 */
exports.executeIntent = async (req, res, next) => {
  try {
    const { transactionHash } = req.body;
    
    if (!transactionHash) {
      return res.status(400).json({ 
        error: { 
          message: 'Transaction hash is required', 
          code: 'MISSING_TRANSACTION_HASH' 
        } 
      });
    }
    
    // Find intent
    const intent = await Intent.findById(req.params.id);
    
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
    
    // Check if intent can be executed
    if (intent.status === 'executed') {
      return res.status(400).json({ 
        error: { 
          message: 'Intent already executed', 
          code: 'INTENT_ALREADY_EXECUTED' 
        } 
      });
    }
    
    if (intent.status === 'cancelled') {
      return res.status(400).json({ 
        error: { 
          message: 'Cannot execute cancelled intent', 
          code: 'INTENT_CANCELLED' 
        } 
      });
    }
    
    if (intent.status === 'expired' || new Date(intent.expiresAt) < new Date()) {
      return res.status(400).json({ 
        error: { 
          message: 'Intent has expired', 
          code: 'INTENT_EXPIRED' 
        } 
      });
    }
    
    // Execute intent
    intent.status = 'executed';
    intent.executedAt = new Date();
    intent.executedTxHash = transactionHash;
    await intent.save();
    
    res.status(200).json({
      message: 'Intent executed successfully',
      intent
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Sign an intent
 */
exports.signIntent = async (req, res, next) => {
  try {
    const { signature } = req.body;
    
    if (!signature) {
      return res.status(400).json({ 
        error: { 
          message: 'Signature is required', 
          code: 'MISSING_SIGNATURE' 
        } 
      });
    }
    
    // Find intent
    const intent = await Intent.findById(req.params.id);
    
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
    
    // Update intent with signature
    intent.signature = signature;
    await intent.save();
    
    res.status(200).json({
      message: 'Intent signed successfully',
      intent
    });
  } catch (error) {
    next(error);
  }
};