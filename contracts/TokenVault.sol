// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "./interfaces/ITokenVault.sol";

/**
 * @title TokenVault
 * @dev Manages the storage and transfer of tokens for the Phoenix Protocol
 * @notice This contract securely holds tokens and provides controlled access for transfers
 * @author Phoenix Protocol Team
 */
contract TokenVault is ITokenVault, AccessControl, Pausable, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // Role definitions
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant PROTOCOL_ROLE = keccak256("PROTOCOL_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");

    // Mappings
    mapping(address => mapping(address => uint256)) private userTokenBalances;
    mapping(address => uint256) private tokenTotalBalances;
    mapping(address => bool) private supportedTokens;

    // Events are defined in the interface

    /**
     * @dev Constructor to initialize the TokenVault contract
     * @param _admin Address of the admin who will have ADMIN_ROLE
     * @param _protocol Address of the protocol contract that will have PROTOCOL_ROLE
     */
    constructor(address _admin, address _protocol) {
        require(_admin != address(0), "Admin address cannot be zero");
        require(_protocol != address(0), "Protocol address cannot be zero");

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ADMIN_ROLE, _admin);
        _grantRole(PROTOCOL_ROLE, _protocol);
        _grantRole(OPERATOR_ROLE, _admin);
    }

    /**
     * @dev Receive function to accept native token transfers
     */
    receive() external payable {
        emit NativeTokenReceived(msg.sender, msg.value);
    }

    /**
     * @dev Modifier to check if a token is supported
     * @param token Address of the token
     */
    modifier tokenSupported(address token) {
        if (token != address(0)) { // Skip check for native token
            require(supportedTokens[token], "Token not supported");
        }
        _;
    }

    /**
     * @dev Deposit tokens into the vault
     * @param token Address of the token to deposit
     * @param amount Amount of tokens to deposit
     */
    function depositToken(
        address token,
        uint256 amount
    ) external override whenNotPaused nonReentrant tokenSupported(token) {
        require(amount > 0, "Amount must be greater than zero");
        
        // Update balances
        userTokenBalances[msg.sender][token] += amount;
        tokenTotalBalances[token] += amount;
        
        // Transfer tokens from user to vault
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        emit TokenDeposited(token, msg.sender, amount);
    }

    /**
     * @dev Deposit native tokens (ETH/AVAX) into the vault
     */
    function depositNativeToken() external payable override whenNotPaused nonReentrant {
        require(msg.value > 0, "Amount must be greater than zero");
        
        // Update balances
        userTokenBalances[msg.sender][address(0)] += msg.value;
        tokenTotalBalances[address(0)] += msg.value;
        
        emit NativeTokenDeposited(msg.sender, msg.value);
    }

    /**
     * @dev Transfer tokens from the vault to a recipient
     * @param token Address of the token to transfer
     * @param to Address of the recipient
     * @param amount Amount of tokens to transfer
     */
    function transferToken(
        address token,
        address to,
        uint256 amount
    ) external override whenNotPaused nonReentrant tokenSupported(token) {
        require(
            hasRole(PROTOCOL_ROLE, msg.sender) || hasRole(OPERATOR_ROLE, msg.sender),
            "Caller must have PROTOCOL_ROLE or OPERATOR_ROLE"
        );
        require(to != address(0), "Recipient cannot be zero address");
        require(amount > 0, "Amount must be greater than zero");
        require(tokenTotalBalances[token] >= amount, "Insufficient token balance in vault");
        
        // Update total balance
        tokenTotalBalances[token] -= amount;
        
        // Transfer tokens
        if (token == address(0)) {
            // Transfer native tokens
            (bool success, ) = to.call{value: amount}("");
            require(success, "Native token transfer failed");
        } else {
            // Transfer ERC20 tokens
            IERC20(token).safeTransfer(to, amount);
        }
        
        emit TokenTransferred(token, to, amount);
    }

    /**
     * @dev Withdraw tokens from the vault
     * @param token Address of the token to withdraw
     * @param to Address of the recipient
     * @param amount Amount of tokens to withdraw
     */
    function withdrawToken(
        address token,
        address to,
        uint256 amount
    ) external override whenNotPaused nonReentrant tokenSupported(token) {
        require(to != address(0), "Recipient cannot be zero address");
        require(amount > 0, "Amount must be greater than zero");
        require(userTokenBalances[msg.sender][token] >= amount, "Insufficient user token balance");
        
        // Update balances
        userTokenBalances[msg.sender][token] -= amount;
        tokenTotalBalances[token] -= amount;
        
        // Transfer tokens
        if (token == address(0)) {
            // Transfer native tokens
            (bool success, ) = to.call{value: amount}("");
            require(success, "Native token transfer failed");
        } else {
            // Transfer ERC20 tokens
            IERC20(token).safeTransfer(to, amount);
        }
        
        emit TokenWithdrawn(token, to, amount);
    }

    /**
     * @dev Withdraw native tokens from the vault
     * @param to Address of the recipient
     * @param amount Amount of native tokens to withdraw
     */
    function withdrawNativeToken(
        address to,
        uint256 amount
    ) external override whenNotPaused nonReentrant {
        require(to != address(0), "Recipient cannot be zero address");
        require(amount > 0, "Amount must be greater than zero");
        require(userTokenBalances[msg.sender][address(0)] >= amount, "Insufficient user native token balance");
        
        // Update balances
        userTokenBalances[msg.sender][address(0)] -= amount;
        tokenTotalBalances[address(0)] -= amount;
        
        // Transfer native tokens
        (bool success, ) = to.call{value: amount}("");
        require(success, "Native token transfer failed");
        
        emit NativeTokenWithdrawn(to, amount);
    }

    /**
     * @dev Emergency withdraw all tokens of a specific type
     * @param token Address of the token to withdraw
     * @param to Address of the recipient
     */
    function emergencyWithdraw(
        address token,
        address to
    ) external override onlyRole(ADMIN_ROLE) {
        require(to != address(0), "Recipient cannot be zero address");
        
        uint256 amount;
        if (token == address(0)) {
            // Withdraw all native tokens
            amount = address(this).balance;
            require(amount > 0, "No native tokens to withdraw");
            
            (bool success, ) = to.call{value: amount}("");
            require(success, "Native token transfer failed");
        } else {
            // Withdraw all ERC20 tokens
            amount = IERC20(token).balanceOf(address(this));
            require(amount > 0, "No tokens to withdraw");
            
            IERC20(token).safeTransfer(to, amount);
        }
        
        // Reset balances for this token
        tokenTotalBalances[token] = 0;
        
        emit EmergencyWithdrawal(token, to, amount);
    }

    /**
     * @dev Add a supported token
     * @param token Address of the token to add
     */
    function addSupportedToken(address token) external onlyRole(ADMIN_ROLE) {
        require(token != address(0), "Cannot add native token");
        require(!supportedTokens[token], "Token already supported");
        
        supportedTokens[token] = true;
        
        emit TokenSupported(token, true);
    }

    /**
     * @dev Remove a supported token
     * @param token Address of the token to remove
     */
    function removeSupportedToken(address token) external onlyRole(ADMIN_ROLE) {
        require(token != address(0), "Cannot remove native token");
        require(supportedTokens[token], "Token not supported");
        require(tokenTotalBalances[token] == 0, "Token balance must be zero");
        
        supportedTokens[token] = false;
        
        emit TokenSupported(token, false);
    }

    /**
     * @dev Check if a token is supported
     * @param token Address of the token
     * @return bool True if the token is supported
     */
    function isTokenSupported(address token) external view returns (bool) {
        if (token == address(0)) return true; // Native token is always supported
        return supportedTokens[token];
    }

    /**
     * @dev Get the balance of a token in the vault
     * @param token Address of the token
     * @return uint256 Balance of the token
     */
    function getTokenBalance(address token) external view override returns (uint256) {
        return tokenTotalBalances[token];
    }

    /**
     * @dev Get the balance of a user's token in the vault
     * @param user Address of the user
     * @param token Address of the token
     * @return uint256 Balance of the user's token
     */
    function getUserTokenBalance(address user, address token) external view returns (uint256) {
        return userTokenBalances[user][token];
    }

    /**
     * @dev Get the native token balance in the vault
     * @return uint256 Balance of native tokens
     */
    function getNativeTokenBalance() external view override returns (uint256) {
        return address(this).balance;
    }

    /**
     * @dev Pause the contract
     */
    function pause() external onlyRole(ADMIN_ROLE) {
        _pause();
    }

    /**
     * @dev Unpause the contract
     */
    function unpause() external onlyRole(ADMIN_ROLE) {
        _unpause();
    }
}