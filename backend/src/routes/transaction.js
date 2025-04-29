const express = require('express');
const router = express.Router();
const transactionController = require('../controllers/transactionController');
const { auth, twoFactorAuth } = require('../middleware/auth');

/**
 * @route GET /api/transactions
 * @desc Get user transactions
 * @access Private
 */
router.get('/', auth, transactionController.getUserTransactions);

/**
 * @route GET /api/transactions/:id
 * @desc Get transaction by ID
 * @access Private
 */
router.get('/:id', auth, transactionController.getTransactionById);

/**
 * @route POST /api/transactions
 * @desc Create a new transaction record
 * @access Private
 */
router.post('/', auth, transactionController.createTransaction);

/**
 * @route PUT /api/transactions/:id
 * @desc Update transaction status
 * @access Private
 */
router.put('/:id', auth, transactionController.updateTransaction);

/**
 * @route GET /api/transactions/address/:address
 * @desc Get transactions by wallet address
 * @access Private
 */
router.get('/address/:address', auth, transactionController.getTransactionsByAddress);

/**
 * @route GET /api/transactions/hash/:hash
 * @desc Get transaction by hash
 * @access Private
 */
router.get('/hash/:hash', auth, transactionController.getTransactionByHash);

/**
 * @route POST /api/transactions/verify
 * @desc Verify transaction against intent
 * @access Private
 */
router.post('/verify', auth, transactionController.verifyTransaction);

module.exports = router;