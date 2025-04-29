const express = require('express');
const router = express.Router();
const intentController = require('../controllers/intentController');
const { auth, twoFactorAuth } = require('../middleware/auth');

/**
 * @route GET /api/intents
 * @desc Get user intents
 * @access Private
 */
router.get('/', auth, intentController.getUserIntents);

/**
 * @route GET /api/intents/:id
 * @desc Get intent by ID
 * @access Private
 */
router.get('/:id', auth, intentController.getIntentById);

/**
 * @route POST /api/intents
 * @desc Create a new intent
 * @access Private
 */
router.post('/', auth, intentController.createIntent);

/**
 * @route PUT /api/intents/:id
 * @desc Update intent status
 * @access Private
 */
router.put('/:id', auth, intentController.updateIntent);

/**
 * @route DELETE /api/intents/:id
 * @desc Cancel an intent
 * @access Private
 */
router.delete('/:id', auth, twoFactorAuth, intentController.cancelIntent);

/**
 * @route GET /api/intents/address/:address
 * @desc Get intents by wallet address
 * @access Private
 */
router.get('/address/:address', auth, intentController.getIntentsByAddress);

/**
 * @route POST /api/intents/:id/execute
 * @desc Execute an intent
 * @access Private
 */
router.post('/:id/execute', auth, intentController.executeIntent);

/**
 * @route POST /api/intents/:id/sign
 * @desc Sign an intent
 * @access Private
 */
router.post('/:id/sign', auth, intentController.signIntent);

module.exports = router;