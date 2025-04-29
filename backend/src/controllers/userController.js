const User = require('../models/User');
const { logger } = require('../utils/logger');

/**
 * Get current user profile
 */
exports.getCurrentUser = async (req, res, next) => {
  try {
    const user = await User.findById(req.user._id);
    
    if (!user) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    res.status(200).json({
      user: {
        _id: user._id,
        walletAddress: user.walletAddress,
        username: user.username,
        email: user.email,
        role: user.role,
        twoFactorEnabled: user.twoFactorEnabled,
        profileImage: user.profileImage,
        preferences: user.preferences,
        createdAt: user.createdAt,
        lastLogin: user.lastLogin
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Update current user profile
 */
exports.updateCurrentUser = async (req, res, next) => {
  try {
    const { username, profileImage } = req.body;
    
    // Check if username exists if provided
    if (username) {
      const usernameExists = await User.findOne({ 
        username, 
        _id: { $ne: req.user._id } 
      });
      
      if (usernameExists) {
        return res.status(400).json({ 
          error: { 
            message: 'Username already in use', 
            code: 'USERNAME_IN_USE' 
          } 
        });
      }
    }
    
    // Update user
    const updatedUser = await User.findByIdAndUpdate(
      req.user._id,
      { 
        $set: { 
          username: username || req.user.username,
          profileImage: profileImage || req.user.profileImage
        } 
      },
      { new: true }
    );
    
    res.status(200).json({
      user: {
        _id: updatedUser._id,
        walletAddress: updatedUser.walletAddress,
        username: updatedUser.username,
        email: updatedUser.email,
        role: updatedUser.role,
        twoFactorEnabled: updatedUser.twoFactorEnabled,
        profileImage: updatedUser.profileImage,
        preferences: updatedUser.preferences,
        createdAt: updatedUser.createdAt,
        lastLogin: updatedUser.lastLogin
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Update user preferences
 */
exports.updatePreferences = async (req, res, next) => {
  try {
    const { theme, language, notifications } = req.body;
    
    // Validate preferences
    const preferences = {};
    
    if (theme && ['light', 'dark', 'system'].includes(theme)) {
      preferences['preferences.theme'] = theme;
    }
    
    if (language) {
      preferences['preferences.language'] = language;
    }
    
    if (notifications) {
      if (typeof notifications.email === 'boolean') {
        preferences['preferences.notifications.email'] = notifications.email;
      }
      
      if (typeof notifications.push === 'boolean') {
        preferences['preferences.notifications.push'] = notifications.push;
      }
    }
    
    // Update user
    const updatedUser = await User.findByIdAndUpdate(
      req.user._id,
      { $set: preferences },
      { new: true }
    );
    
    res.status(200).json({
      preferences: updatedUser.preferences
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Update user password
 */
exports.updatePassword = async (req, res, next) => {
  try {
    const { currentPassword, newPassword } = req.body;
    
    if (!currentPassword || !newPassword) {
      return res.status(400).json({ 
        error: { 
          message: 'Current password and new password are required', 
          code: 'MISSING_PASSWORDS' 
        } 
      });
    }
    
    // Get user with password
    const user = await User.findById(req.user._id).select('+password');
    
    // Check if password is set
    if (!user.password) {
      return res.status(400).json({ 
        error: { 
          message: 'No password is currently set', 
          code: 'NO_PASSWORD' 
        } 
      });
    }
    
    // Verify current password
    const isMatch = await user.comparePassword(currentPassword);
    
    if (!isMatch) {
      return res.status(401).json({ 
        error: { 
          message: 'Current password is incorrect', 
          code: 'INVALID_PASSWORD' 
        } 
      });
    }
    
    // Update password
    user.password = newPassword;
    await user.save();
    
    res.status(200).json({
      message: 'Password updated successfully'
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Update user email
 */
exports.updateEmail = async (req, res, next) => {
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
    
    // Check if email already exists
    const emailExists = await User.findOne({ 
      email: email.toLowerCase(), 
      _id: { $ne: req.user._id } 
    });
    
    if (emailExists) {
      return res.status(400).json({ 
        error: { 
          message: 'Email already in use', 
          code: 'EMAIL_IN_USE' 
        } 
      });
    }
    
    // Get user with password
    const user = await User.findById(req.user._id).select('+password');
    
    // Check if password is set
    if (!user.password) {
      return res.status(400).json({ 
        error: { 
          message: 'No password is currently set', 
          code: 'NO_PASSWORD' 
        } 
      });
    }
    
    // Verify password
    const isMatch = await user.comparePassword(password);
    
    if (!isMatch) {
      return res.status(401).json({ 
        error: { 
          message: 'Password is incorrect', 
          code: 'INVALID_PASSWORD' 
        } 
      });
    }
    
    // Update email
    user.email = email.toLowerCase();
    await user.save();
    
    res.status(200).json({
      message: 'Email updated successfully',
      email: user.email
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get all users (admin only)
 */
exports.getAllUsers = async (req, res, next) => {
  try {
    const page = parseInt(req.query.page) || 1;
    const limit = parseInt(req.query.limit) || 10;
    const skip = (page - 1) * limit;
    
    const users = await User.find()
      .select('-revokedTokens')
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit);
    
    const total = await User.countDocuments();
    
    res.status(200).json({
      users,
      pagination: {
        total,
        page,
        limit,
        pages: Math.ceil(total / limit)
      }
    });
  } catch (error) {
    next(error);
  }
};

/**
 * Get user by ID (admin only)
 */
exports.getUserById = async (req, res, next) => {
  try {
    const user = await User.findById(req.params.id).select('-revokedTokens');
    
    if (!user) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    res.status(200).json({ user });
  } catch (error) {
    next(error);
  }
};

/**
 * Update user by ID (admin only)
 */
exports.updateUserById = async (req, res, next) => {
  try {
    const { username, email, role, isActive } = req.body;
    
    // Check if username exists if provided
    if (username) {
      const usernameExists = await User.findOne({ 
        username, 
        _id: { $ne: req.params.id } 
      });
      
      if (usernameExists) {
        return res.status(400).json({ 
          error: { 
            message: 'Username already in use', 
            code: 'USERNAME_IN_USE' 
          } 
        });
      }
    }
    
    // Check if email exists if provided
    if (email) {
      const emailExists = await User.findOne({ 
        email: email.toLowerCase(), 
        _id: { $ne: req.params.id } 
      });
      
      if (emailExists) {
        return res.status(400).json({ 
          error: { 
            message: 'Email already in use', 
            code: 'EMAIL_IN_USE' 
          } 
        });
      }
    }
    
    // Prepare update object
    const updateData = {};
    
    if (username) updateData.username = username;
    if (email) updateData.email = email.toLowerCase();
    if (role && ['user', 'admin'].includes(role)) updateData.role = role;
    if (typeof isActive === 'boolean') updateData.isActive = isActive;
    
    // Update user
    const updatedUser = await User.findByIdAndUpdate(
      req.params.id,
      { $set: updateData },
      { new: true }
    ).select('-revokedTokens');
    
    if (!updatedUser) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    res.status(200).json({ user: updatedUser });
  } catch (error) {
    next(error);
  }
};

/**
 * Delete user by ID (admin only)
 */
exports.deleteUserById = async (req, res, next) => {
  try {
    const user = await User.findByIdAndDelete(req.params.id);
    
    if (!user) {
      return res.status(404).json({ 
        error: { 
          message: 'User not found', 
          code: 'USER_NOT_FOUND' 
        } 
      });
    }
    
    // In a real app, you might want to handle related data deletion
    // or implement soft delete instead
    
    res.status(200).json({
      message: 'User deleted successfully'
    });
  } catch (error) {
    next(error);
  }
};