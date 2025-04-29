const express = require('express');
const router = express.Router();
const recoveryController = require('../controllers/recoveryController');
const { auth, twoFactorAuth } = require('../middleware/auth');

/**
 * @route POST /api/recovery/request
 * @desc Request transaction recovery
 * @access Private
 */
router.post('/request', auth, twoFactorAuth, recoveryController.requestRecovery);

/**
 * @route GET /api/recovery/requests
 * @desc Get user recovery requests
 * @access Private
 */
router.get('/requests', auth, recoveryController.getUserRecoveryRequests);

/**
 * @route GET /api/recovery/requests/:id
 * @desc Get recovery request by ID
 * @access Private
 */
router.get('/requests/:id', auth, recoveryController.getRecoveryRequestById);

/**
 * @route PUT /api/recovery/requests/:id
 * @desc Update recovery request status
 * @access Private
 */
router.put('/requests/:id', auth, twoFactorAuth, recoveryController.updateRecoveryRequest);

/**
 * @route POST /api/recovery/simulate
 * @desc Simulate transaction recovery
 * @access Private
 */
router.post('/simulate', auth, recoveryController.simulateRecovery);

/**
 * @route POST /api/recovery/execute/:id
 * @desc Execute transaction recovery
 * @access Private
 */
router.post('/execute/:id', auth, twoFactorAuth, recoveryController.executeRecovery);

module.exports = router;