const Transaction = require('../models/Transaction');
const { logger } = require('../utils/logger');

/**
 * Request transaction recovery
 */
exports.requestRecovery = async (req, res, next) => {
  try {
    const { transactionId, reason } = req.body;
    
    if (!transactionId || !reason) {
      return res.status(400).json({ 
        error: { 
          message: 'Transaction ID and reason are required', 
          code: 'MISSING_FIELDS' 
        } 
      });
    }
    
    // Find transaction
    const transaction = await Transaction.findById(transactionId);
    
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
    
    // Check if recovery is already requested
    if (transaction.recoveryStatus !== 'none') {
      return res.status(400).json({ 
        error: { 
          message: `Recovery already ${transaction.recoveryStatus}`, 
          code: 'RECOVERY_ALREADY_REQUESTED' 
        } 
      });
    }
    
    // Update transaction
    transaction.recoveryStatus = 'requested';
    transaction.recoveryReason = reason;
    await transaction.save();
    
    res.status(200).json({
      message: 'Recovery request submitted successfully',
      transaction
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get user recovery requests
 */
exports.getUserRecoveryRequests = async (req, res, next) => {
  try {
    const page = parseInt(req.query.page) || 1;
    const limit = parseInt(req.query.limit) || 10;
    const skip = (page - 1) * limit;
    const status = req.query.status;
    
    // Build query
    const query = { 
      user: req.user._id,
      recoveryStatus: { $ne: 'none' }
    };
    
    if (status && status !== 'all') {
      query.recoveryStatus = status;
    }
    
    // Execute query
    const transactions = await Transaction.find(query)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit);
    
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
 * Get recovery request by ID
 */
exports.getRecoveryRequestById = async (req, res, next) => {
  try {
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
    if (transaction.user.toString() !== req.user._id.toString() && req.user.role !== 'admin') {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    // Check if recovery is requested
    if (transaction.recoveryStatus === 'none') {
      return res.status(400).json({ 
        error: { 
          message: 'No recovery requested for this transaction', 
          code: 'NO_RECOVERY_REQUESTED' 
        } 
      });
    }
    
    res.status(200).json({ transaction });
  } catch (error) {
    next(error);
  }
};

/**
 * Update recovery request status
 */
exports.updateRecoveryRequest = async (req, res, next) => {
  try {
    const { status } = req.body;
    
    if (!status || !['requested', 'approved', 'rejected', 'completed'].includes(status)) {
      return res.status(400).json({ 
        error: { 
          message: 'Invalid status', 
          code: 'INVALID_STATUS' 
        } 
      });
    }
    
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
    // For status updates, we need to check if user is admin or the transaction owner
    const isAdmin = req.user.role === 'admin';
    const isOwner = transaction.user.toString() === req.user._id.toString();
    
    if (!isAdmin && !isOwner) {
      return res.status(403).json({ 
        error: { 
          message: 'Access denied', 
          code: 'ACCESS_DENIED' 
        } 
      });
    }
    
    // Some status changes are restricted to admins
    if ((status === 'approved' || status === 'rejected') && !isAdmin) {
      return res.status(403).json({ 
        error: { 
          message: 'Only admins can approve or reject recovery requests', 
          code: 'ADMIN_REQUIRED' 
        } 
      });
    }
    
    // Update transaction
    transaction.recoveryStatus = status;
    await transaction.save();
    
    res.status(200).json({
      message: `Recovery request ${status} successfully`,
      transaction
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Simulate transaction recovery
 */
exports.simulateRecovery = async (req, res, next) => {
  try {
    const { transactionId } = req.body;
    
    if (!transactionId) {
      return res.status(400).json({ 
        error: { 
          message: 'Transaction ID is required', 
          code: 'MISSING_TRANSACTION_ID' 
        } 
      });
    }
    
    // Find transaction
    const transaction = await Transaction.findById(transactionId);
    
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
    
    // Simulate recovery
    // In a real app, this would interact with blockchain to simulate the recovery
    // For demo purposes, we'll just return a success response
    
    res.status(200).json({
      simulation: {
        possible: true,
        estimatedGas: '150000',
        estimatedCost: '0.005 ETH',
        recoveryMethod: 'contract call',
        success: true
      },
      transaction
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Execute transaction recovery
 */
exports.executeRecovery = async (req, res, next) => {
  try {
    const { recoveryTxHash } = req.body;
    
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
    
    // Check if recovery is approved
    if (transaction.recoveryStatus !== 'approved') {
      return res.status(400).json({ 
        error: { 
          message: 'Recovery must be approved before execution', 
          code: 'RECOVERY_NOT_APPROVED' 
        } 
      });
    }
    
    // In a real app, this would execute the recovery transaction on the blockchain
    // For demo purposes, we'll just update the status
    
    // Update transaction
    transaction.recoveryStatus = 'completed';
    transaction.metadata = {
      ...transaction.metadata,
      recoveryTxHash,
      recoveryCompletedAt: new Date()
    };
    await transaction.save();
    
    res.status(200).json({
      message: 'Recovery executed successfully',
      transaction
    });
  } catch (error) {
    next(error);
  }
};