const express = require('express');
const router = express.Router();
const authController = require('../controllers/authController');
const { auth } = require('../middleware/auth');

/**
 * @route POST /api/auth/nonce
 * @desc Get a nonce for wallet signature
 * @access Public
 */
router.post('/nonce', authController.getNonce);

/**
 * @route POST /api/auth/wallet
 * @desc Authenticate with wallet signature
 * @access Public
 */
router.post('/wallet', authController.authenticateWithWallet);

/**
 * @route POST /api/auth/login
 * @desc Login with email and password
 * @access Public
 */
router.post('/login', authController.login);

/**
 * @route POST /api/auth/register
 * @desc Register a new user
 * @access Public
 */
router.post('/register', authController.register);

/**
 * @route POST /api/auth/logout
 * @desc Logout user
 * @access Private
 */
router.post('/logout', auth, authController.logout);

/**
 * @route POST /api/auth/refresh
 * @desc Refresh access token
 * @access Public
 */
router.post('/refresh', authController.refreshToken);

/**
 * @route POST /api/auth/2fa/setup
 * @desc Setup two-factor authentication
 * @access Private
 */
router.post('/2fa/setup', auth, authController.setupTwoFactor);

/**
 * @route POST /api/auth/2fa/verify
 * @desc Verify two-factor authentication code
 * @access Private
 */
router.post('/2fa/verify', auth, authController.verifyTwoFactor);

/**
 * @route POST /api/auth/2fa/disable
 * @desc Disable two-factor authentication
 * @access Private
 */
router.post('/2fa/disable', auth, authController.disableTwoFactor);

module.exports = router;