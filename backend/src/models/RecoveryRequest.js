const mongoose = require('mongoose');

const RecoveryRequestSchema = new mongoose.Schema({
  user: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    required: true
  },
  transaction: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Transaction',
    required: true
  },
  walletAddress: {
    type: String,
    required: true,
    lowercase: true,
    trim: true
  },
  status: {
    type: String,
    enum: ['pending', 'approved', 'rejected', 'completed'],
    default: 'pending'
  },
  reason: {
    type: String,
    required: true
  },
  approvedBy: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    default: null
  },
  approvedAt: {
    type: Date,
    default: null
  },
  recoveryTxHash: {
    type: String,
    default: null
  },
  completedAt: {
    type: Date,
    default: null
  },
  notes: {
    type: String,
    default: null
  },
  metadata: {
    type: mongoose.Schema.Types.Mixed,
    default: {}
  }
}, {
  timestamps: true
});

// Indexes for faster queries
RecoveryRequestSchema.index({ user: 1, createdAt: -1 });
RecoveryRequestSchema.index({ transaction: 1 }, { unique: true });
RecoveryRequestSchema.index({ walletAddress: 1, status: 1 });

// Create model
const RecoveryRequest = mongoose.model('RecoveryRequest', RecoveryRequestSchema);

module.exports = RecoveryRequest;