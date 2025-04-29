const mongoose = require('mongoose');

const TransactionSchema = new mongoose.Schema({
  user: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    required: true
  },
  walletAddress: {
    type: String,
    required: true,
    lowercase: true,
    trim: true
  },
  transactionHash: {
    type: String,
    required: true,
    unique: true,
    trim: true
  },
  chainId: {
    type: Number,
    required: true
  },
  type: {
    type: String,
    enum: ['send', 'receive', 'swap', 'stake', 'unstake', 'approve', 'other'],
    required: true
  },
  status: {
    type: String,
    enum: ['pending', 'confirmed', 'failed', 'reverted'],
    default: 'pending'
  },
  amount: {
    type: String,
    required: true
  },
  tokenAddress: {
    type: String,
    default: null
  },
  tokenSymbol: {
    type: String,
    default: null
  },
  tokenDecimals: {
    type: Number,
    default: 18
  },
  from: {
    type: String,
    required: true,
    lowercase: true
  },
  to: {
    type: String,
    required: true,
    lowercase: true
  },
  gasUsed: {
    type: String
  },
  gasPrice: {
    type: String
  },
  blockNumber: {
    type: Number
  },
  blockTimestamp: {
    type: Date
  },
  intentId: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Intent',
    default: null
  },
  metadata: {
    type: mongoose.Schema.Types.Mixed,
    default: {}
  },
  recoveryStatus: {
    type: String,
    enum: ['none', 'requested', 'approved', 'rejected', 'completed'],
    default: 'none'
  },
  recoveryReason: {
    type: String,
    default: null
  }
}, {
  timestamps: true
});

// Indexes for faster queries
TransactionSchema.index({ user: 1, createdAt: -1 });
TransactionSchema.index({ walletAddress: 1, createdAt: -1 });
TransactionSchema.index({ transactionHash: 1 }, { unique: true });
TransactionSchema.index({ chainId: 1, blockNumber: -1 });

// Create model
const Transaction = mongoose.model('Transaction', TransactionSchema);

module.exports = Transaction;