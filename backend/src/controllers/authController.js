const jwt = require('jsonwebtoken');
const speakeasy = require('speakeasy');
const ethers = require('ethers');
const User = require('../models/User');
const config = require('../config');
const { logger } = require('../utils/logger');

/**
 * Generate a nonce for wallet signature
 */
exports.getNonce = async (req, res, next) => {
  try {
    const { walletAddress } = req.body;
    
    if (!walletAddress) {
      return res.status(400).json({ 
        error: { 
          message: 'Wallet address is required', 
          code: 'MISSING_WALLET_ADDRESS' 
        } 
      });
    }
    
    // Find or create user
    let user = await User.findOne({ walletAddress: walletAddress.toLowerCase() });
    
    if (!user) {
      // Create new user with wallet address
      user = new User({
        walletAddress: walletAddress.toLowerCase()
      });
    }
    
    // Generate and save nonce
    const nonce = user.generateNonce();
    await user.save();
    
    res.status(200).json({
      nonce: `Sign this message to authenticate with Phoenix Protocol: ${nonce}`
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Authenticate with wallet signature
 */
exports.authenticateWithWallet = async (req, res, next) => {
  try {
    const { walletAddress, signature } = req.body;
    
    if (!walletAddress || !signature) {
      return res.status(400).json({ 
        error: { 
          message: 'Wallet address and signature are required', 
          code: 'MISSING_CREDENTIALS' 
        } 
      });
    }
    
    // Find user
    const user = await User.findOne({ walletAddress: walletAddress.toLowerCase() }).select('+nonce');
    
    if (!user) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    // Verify signature
    const message = `Sign this message to authenticate with Phoenix Protocol: ${user.nonce}`;
    const recoveredAddress = ethers.verifyMessage(message, signature);
    
    if (recoveredAddress.toLowerCase() !== walletAddress.toLowerCase()) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid signature', 
          code: 'INVALID_SIGNATURE' 
        } 
      });
    }
    
    // Generate new nonce for next login
    user.generateNonce();
    user.lastLogin = new Date();
    await user.save();
    
    // Generate JWT token
    const token = jwt.sign(
      { userId: user._id, walletAddress: user.walletAddress },
      config.JWT_SECRET,
      { expiresIn: config.JWT_EXPIRATION }
    );
    
    res.status(200).json({
      token,
      user: {
        _id: user._id,
        walletAddress: user.walletAddress,
        username: user.username,
        email: user.email,
        role: user.role,
        twoFactorEnabled: user.twoFactorEnabled,
        profileImage: user.profileImage,
        preferences: user.preferences,
        createdAt: user.createdAt
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Login with email and password
 */
exports.login = async (req, res, next) => {
  try {
    const { email, password } = req.body;
    
    if (!email || !password) {
      return res.status(400).json({ 
        error: { 
          message: 'Email and password are required', 
          code: 'MISSING_CREDENTIALS' 
        } 
      });
    }
    
    // Find user
    const user = await User.findOne({ email: email.toLowerCase() }).select('+password');
    
    if (!user) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid credentials', 
          code: 'INVALID_CREDENTIALS' 
        } 
      });
    }
    
    // Check if password is set
    if (!user.password) {
      return res.status(401).json({ 
        error: { 
          message: 'Account has no password set', 
          code: 'NO_PASSWORD' 
        } 
      });
    }
    
    // Verify password
    const isMatch = await user.comparePassword(password);
    
    if (!isMatch) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid credentials', 
          code: 'INVALID_CREDENTIALS' 
        } 
      });
    }
    
    // Update last login
    user.lastLogin = new Date();
    await user.save();
    
    // Generate JWT token
    const token = jwt.sign(
      { userId: user._id, walletAddress: user.walletAddress },
      config.JWT_SECRET,
      { expiresIn: config.JWT_EXPIRATION }
    );
    
    res.status(200).json({
      token,
      user: {
        _id: user._id,
        walletAddress: user.walletAddress,
        username: user.username,
        email: user.email,
        role: user.role,
        twoFactorEnabled: user.twoFactorEnabled,
        profileImage: user.profileImage,
        preferences: user.preferences,
        createdAt: user.createdAt
      },
      twoFactorRequired: user.twoFactorEnabled
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Register a new user
 */
exports.register = async (req, res, next) => {
  try {
    const { email, password, username, walletAddress } = req.body;
    
    if (!email || !password) {
      return res.status(400).json({ 
        error: { 
          message: 'Email and password are required', 
          code: 'MISSING_CREDENTIALS' 
        } 
      });
    }
    
    // Check if email already exists
    const emailExists = await User.findOne({ email: email.toLowerCase() });
    
    if (emailExists) {
      return res.status(400).json({ 
        error: { 
          message: 'Email already in use', 
          code: 'EMAIL_IN_USE' 
        } 
      });
    }
    
    // Check if username exists if provided
    if (username) {
      const usernameExists = await User.findOne({ username });
      
      if (usernameExists) {
        return res.status(400).json({ 
          error: { 
            message: 'Username already in use', 
            code: 'USERNAME_IN_USE' 
          } 
        });
      }
    }
    
    // Create user
    const user = new User({
      email: email.toLowerCase(),
      password,
      username,
      walletAddress: walletAddress ? walletAddress.toLowerCase() : null
    });
    
    await user.save();
    
    // Generate JWT token
    const token = jwt.sign(
      { userId: user._id, walletAddress: user.walletAddress },
      config.JWT_SECRET,
      { expiresIn: config.JWT_EXPIRATION }
    );
    
    res.status(201).json({
      token,
      user: {
        _id: user._id,
        walletAddress: user.walletAddress,
        username: user.username,
        email: user.email,
        role: user.role,
        twoFactorEnabled: user.twoFactorEnabled,
        profileImage: user.profileImage,
        preferences: user.preferences,
        createdAt: user.createdAt
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Logout user
 */
exports.logout = async (req, res, next) => {
  try {
    // Add current token to revoked tokens list
    if (!req.user.revokedTokens) {
      req.user.revokedTokens = [];
    }
    
    req.user.revokedTokens.push(req.token);
    
    // Limit the size of revoked tokens array
    if (req.user.revokedTokens.length > 10) {
      req.user.revokedTokens = req.user.revokedTokens.slice(-10);
    }
    
    await req.user.save();
    
    res.status(200).json({ message: 'Logged out successfully' });
  } catch (error) {
    next(error);
  }
};

/**
 * Refresh access token
 */
exports.refreshToken = async (req, res, next) => {
  try {
    const { token } = req.body;
    
    if (!token) {
      return res.status(400).json({ 
        error: { 
          message: 'Token is required', 
          code: 'MISSING_TOKEN' 
        } 
      });
    }
    
    // Verify token
    let decoded;
    try {
      decoded = jwt.verify(token, config.JWT_SECRET, { ignoreExpiration: true });
    } catch (error) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid token', 
          code: 'INVALID_TOKEN' 
        } 
      });
    }
    
    // Check if token is expired
    const currentTime = Math.floor(Date.now() / 1000);
    if (decoded.exp && decoded.exp > currentTime) {
      return res.status(400).json({ 
        error: { 
          message: 'Token is not expired yet', 
          code: 'TOKEN_NOT_EXPIRED' 
        } 
      });
    }
    
    // Find user
    const user = await User.findById(decoded.userId);
    
    if (!user) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    // Check if token is revoked
    if (user.revokedTokens && user.revokedTokens.includes(token)) {
      return res.status(401).json({ 
        error: { 
          message: 'Token has been revoked', 
          code: 'TOKEN_REVOKED' 
        } 
      });
    }
    
    // Generate new token
    const newToken = jwt.sign(
      { userId: user._id, walletAddress: user.walletAddress },
      config.JWT_SECRET,
      { expiresIn: config.JWT_EXPIRATION }
    );
    
    // Add old token to revoked list
    if (!user.revokedTokens) {
      user.revokedTokens = [];
    }
    
    user.revokedTokens.push(token);
    
    // Limit the size of revoked tokens array
    if (user.revokedTokens.length > 10) {
      user.revokedTokens = user.revokedTokens.slice(-10);
    }
    
    await user.save();
    
    res.status(200).json({ token: newToken });
  } catch (error) {
    next(error);
  }
};

/**
 * Setup two-factor authentication
 */
exports.setupTwoFactor = async (req, res, next) => {
  try {
    // Generate secret
    const secret = speakeasy.generateSecret({
      name: `Phoenix Protocol (${req.user.walletAddress})`
    });
    
    // Save secret to user
    req.user.twoFactorSecret = secret.base32;
    await req.user.save();
    
    res.status(200).json({
      secret: secret.base32,
      otpauth_url: secret.otpauth_url
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Verify two-factor authentication code
 */
exports.verifyTwoFactor = async (req, res, next) => {
  try {
    const { token } = req.body;
    
    if (!token) {
      return res.status(400).json({ 
        error: { 
          message: 'Token is required', 
          code: 'MISSING_TOKEN' 
        } 
      });
    }
    
    // Get user with 2FA secret
    const user = await User.findById(req.user._id).select('+twoFactorSecret');
    
    if (!user.twoFactorSecret) {
      return res.status(400).json({ 
        error: { 
          message: 'Two-factor authentication not set up', 
          code: 'TWO_FACTOR_NOT_SETUP' 
        } 
      });
    }
    
    // Verify token
    const verified = speakeasy.totp.verify({
      secret: user.twoFactorSecret,
      encoding: 'base32',
      token
    });
    
    if (!verified) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid verification code', 
          code: 'INVALID_CODE' 
        } 
      });
    }
    
    // If this is the first verification, enable 2FA
    if (!user.twoFactorEnabled) {
      user.twoFactorEnabled = true;
    }
    
    // Mark as verified for this session
    user.twoFactorVerified = true;
    await user.save();
    
    res.status(200).json({
      verified: true,
      twoFactorEnabled: user.twoFactorEnabled
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Disable two-factor authentication
 */
exports.disableTwoFactor = async (req, res, next) => {
  try {
    const { token } = req.body;
    
    if (!token) {
      return res.status(400).json({ 
        error: { 
          message: 'Token is required', 
          code: 'MISSING_TOKEN' 
        } 
      });
    }
    
    // Get user with 2FA secret
    const user = await User.findById(req.user._id).select('+twoFactorSecret');
    
    if (!user.twoFactorEnabled) {
      return res.status(400).json({ 
        error: { 
          message: 'Two-factor authentication not enabled', 
          code: 'TWO_FACTOR_NOT_ENABLED' 
        } 
      });
    }
    
    // Verify token
    const verified = speakeasy.totp.verify({
      secret: user.twoFactorSecret,
      encoding: 'base32',
      token
    });
    
    if (!verified) {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid verification code', 
          code: 'INVALID_CODE' 
        } 
      });
    }
    
    // Disable 2FA
    user.twoFactorEnabled = false;
    user.twoFactorSecret = undefined;
    user.twoFactorVerified = false;
    await user.save();
    
    res.status(200).json({
      message: 'Two-factor authentication disabled successfully'
    });
  } catch (error) {
    next(error);
  }
};