const express = require('express');
const router = express.Router();

// Import route modules
const authRoutes = require('./auth');
const userRoutes = require('./user');
const transactionRoutes = require('./transaction');
const intentRoutes = require('./intent');
const recoveryRoutes = require('./recovery');

// Mount routes
router.use('/auth', authRoutes);
router.use('/users', userRoutes);
router.use('/transactions', transactionRoutes);
router.use('/intents', intentRoutes);
router.use('/recovery', recoveryRoutes);

module.exports = router;