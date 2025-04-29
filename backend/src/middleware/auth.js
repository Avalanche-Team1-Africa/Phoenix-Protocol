const jwt = require('jsonwebtoken');
const config = require('../config');
const User = require('../models/User');

/**
 * Authentication middleware
 * Verifies JWT token and attaches user to request object
 */
const auth = async (req, res, next) => {
  try {
    // Get token from header
    const authHeader = req.headers.authorization;
    
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      return res.status(401).json({ 
        error: { 
          message: 'Authentication required', 
          code: 'AUTH_REQUIRED' 
        } 
      });
    }
    
    const token = authHeader.split(' ')[1];
    
    // Verify token
    const decoded = jwt.verify(token, config.JWT_SECRET);
    
    // Find user by id
    const user = await User.findById(decoded.userId);
    
    if (!user) {
      return res.status(401).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    // Check if token is in the blacklist (for logout)
    if (user.revokedTokens && user.revokedTokens.includes(token)) {
      return res.status(401).json({ 
        error: { 
          message: 'Token has been revoked', 
          code: 'TOKEN_REVOKED' 
        } 
      });
    }
    
    // Attach user to request object
    req.user = user;
    req.token = token;
    
    next();
  } catch (error) {
    if (error.name === 'JsonWebTokenError') {
      return res.status(401).json({ 
        error: { 
          message: 'Invalid token', 
          code: 'INVALID_TOKEN' 
        } 
      });
    }
    
    if (error.name === 'TokenExpiredError') {
      return res.status(401).json({ 
        error: { 
          message: 'Token expired', 
          code: 'TOKEN_EXPIRED' 
        } 
      });
    }
    
    next(error);
  }
};

/**
 * Admin authorization middleware
 * Checks if authenticated user has admin role
 */
const adminAuth = (req, res, next) => {
  if (!req.user || req.user.role !== 'admin') {
    return res.status(403).json({ 
      error: { 
        message: 'Admin access required', 
        code: 'ADMIN_REQUIRED' 
      } 
    });
  }
  
  next();
};

/**
 * Two-factor authentication middleware
 * Checks if user has completed 2FA verification for this session
 */
const twoFactorAuth = (req, res, next) => {
  if (!req.user) {
    return res.status(401).json({ 
      error: { 
        message: 'Authentication required', 
        code: 'AUTH_REQUIRED' 
      } 
    });
  }
  
  // If 2FA is not enabled for this user, proceed
  if (!req.user.twoFactorEnabled) {
    return next();
  }
  
  // Check if user has completed 2FA verification for this session
  if (!req.user.twoFactorVerified) {
    return res.status(403).json({ 
      error: { 
        message: 'Two-factor authentication required', 
        code: 'TWO_FACTOR_REQUIRED' 
      } 
    });
  }
  
  next();
};

module.exports = {
  auth,
  adminAuth,
  twoFactorAuth
};