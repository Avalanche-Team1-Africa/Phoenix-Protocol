const express = require('express');
const router = express.Router();
const userController = require('../controllers/userController');
const { auth, adminAuth, twoFactorAuth } = require('../middleware/auth');

/**
 * @route GET /api/users/me
 * @desc Get current user profile
 * @access Private
 */
router.get('/me', auth, userController.getCurrentUser);

/**
 * @route PUT /api/users/me
 * @desc Update current user profile
 * @access Private
 */
router.put('/me', auth, userController.updateCurrentUser);

/**
 * @route PUT /api/users/me/preferences
 * @desc Update user preferences
 * @access Private
 */
router.put('/me/preferences', auth, userController.updatePreferences);

/**
 * @route PUT /api/users/me/password
 * @desc Update user password
 * @access Private
 */
router.put('/me/password', auth, twoFactorAuth, userController.updatePassword);

/**
 * @route PUT /api/users/me/email
 * @desc Update user email
 * @access Private
 */
router.put('/me/email', auth, twoFactorAuth, userController.updateEmail);

/**
 * @route GET /api/users
 * @desc Get all users (admin only)
 * @access Private/Admin
 */
router.get('/', auth, adminAuth, userController.getAllUsers);

/**
 * @route GET /api/users/:id
 * @desc Get user by ID (admin only)
 * @access Private/Admin
 */
router.get('/:id', auth, adminAuth, userController.getUserById);

/**
 * @route PUT /api/users/:id
 * @desc Update user by ID (admin only)
 * @access Private/Admin
 */
router.put('/:id', auth, adminAuth, userController.updateUserById);

/**
 * @route DELETE /api/users/:id
 * @desc Delete user by ID (admin only)
 * @access Private/Admin
 */
router.delete('/:id', auth, adminAuth, userController.deleteUserById);

module.exports = router;