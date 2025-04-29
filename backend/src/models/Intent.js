const mongoose = require('mongoose');

const IntentSchema = new mongoose.Schema({
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
  chainId: {
    type: Number,
    required: true
  },
  type: {
    type: String,
    enum: ['send', 'swap', 'stake', 'approve', 'other'],
    required: true
  },
  status: {
    type: String,
    enum: ['pending', 'executed', 'expired', 'cancelled'],
    default: 'pending'
  },
  tokenAddress: {
    type: String,
    default: null
  },
  tokenSymbol: {
    type: String,
    default: null
  },
  amount: {
    type: String,
    required: true
  },
  recipient: {
    type: String,
    required: true,
    lowercase: true
  },
  expiresAt: {
    type: Date,
    required: true
  },
  executedAt: {
    type: Date,
    default: null
  },
  executedTxHash: {
    type: String,
    default: null
  },
  parameters: {
    type: mongoose.Schema.Types.Mixed,
    default: {}
  },
  signature: {
    type: String,
    default: null
  },
  nonce: {
    type: Number,
    required: true
  }
}, {
  timestamps: true
});

// Indexes for faster queries
IntentSchema.index({ user: 1, createdAt: -1 });
IntentSchema.index({ walletAddress: 1, status: 1 });
IntentSchema.index({ expiresAt: 1 }, { expireAfterSeconds: 0 });

// Create model
const Intent = mongoose.model('Intent', IntentSchema);

module.exports = Intent;