// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "./interfaces/ITokenVault.sol";

/**
 * @title TokenVault
 * @dev Contract for securely storing and managing tokens
 * 
 * This contract serves as a secure vault for storing tokens used in the protocol.
 * It handles deposits, withdrawals, and transfers of both native and ERC20 tokens.
 */
contract TokenVault is ITokenVault, AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // Role definitions
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant PROTOCOL_ROLE = keccak256("PROTOCOL_ROLE");

    // Events
    event TokenDeposited(address indexed token, address indexed from, uint256 amount);
    event TokenWithdrawn(address indexed token, address indexed to, uint256 amount);
    event TokenTransferred(address indexed token, address indexed to, uint256 amount);
    event NativeTokenReceived(address indexed sender, uint256 amount);
    event EmergencyWithdrawal(address indexed token, address indexed to, uint256 amount);

    /**
     * @dev Constructor
     * @param _admin Admin address that will have full control
     * @param _protocol Protocol contract address that can transfer tokens
     */
    constructor(address _admin, address _protocol) {
        require(_admin != address(0), "Admin cannot be zero address");
        require(_protocol != address(0), "Protocol cannot be zero address");

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ADMIN_ROLE, _admin);
        _grantRole(PROTOCOL_ROLE, _protocol);
    }

    /**
     * @dev Fallback function to receive ETH
     */
    receive() external payable {
        emit NativeTokenReceived(msg.sender, msg.value);
    }

    /**
     * @dev Deposit ERC20 tokens into the vault
     * @param token The token address
     * @param amount The amount to deposit
     */
    function depositToken(
        address token,
        uint256 amount
    ) external nonReentrant whenNotPaused {
        require(token != address(0), "Invalid token address");
        require(amount > 0, "Amount must be greater than zero");

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        emit TokenDeposited(token, msg.sender, amount);
    }

    /**
     * @dev Withdraw ERC20 tokens from the vault (admin only)
     * @param token The token address
     * @param to The recipient address
     * @param amount The amount to withdraw
     */
    function withdrawToken(
        address token,
        address to,
        uint256 amount
    ) external onlyRole(ADMIN_ROLE) nonReentrant {
        require(token != address(0), "Invalid token address");
        require(to != address(0), "Invalid recipient address");
        require(amount > 0, "Amount must be greater than zero");

        IERC20(token).safeTransfer(to, amount);
        
        emit TokenWithdrawn(token, to, amount);
    }

    /**
     * @dev Transfer ERC20 tokens from the vault (protocol only)
     * @param token The token address
     * @param to The recipient address
     * @param amount The amount to transfer
     */
    function transferToken(
        address token,
        address to,
        uint256 amount
    ) external override onlyRole(PROTOCOL_ROLE) nonReentrant whenNotPaused {
        require(token != address(0), "Invalid token address");
        require(to != address(0), "Invalid recipient address");
        require(amount > 0, "Amount must be greater than zero");

        IERC20(token).safeTransfer(to, amount);
        
        emit TokenTransferred(token, to, amount);
    }

    /**
     * @dev Withdraw native tokens from the vault (admin only)
     * @param to The recipient address
     * @param amount The amount to withdraw
     */
    function withdrawNativeToken(
        address to,
        uint256 amount
    ) external onlyRole(ADMIN_ROLE) nonReentrant {
        require(to != address(0), "Invalid recipient address");
        require(amount > 0, "Amount must be greater than zero");
        require(address(this).balance >= amount, "Insufficient balance");

        (bool success, ) = to.call{value: amount}("");
        require(success, "Native token transfer failed");
        
        emit TokenWithdrawn(address(0), to, amount);
    }

    /**
     * @dev Get the balance of an ERC20 token in the vault
     * @param token The token address
     * @return The token balance
     */
    function getTokenBalance(address token) external view returns (uint256) {
        return IERC20(token).balanceOf(address(this));
    }

    /**
     * @dev Get the native token balance in the vault
     * @return The native token balance
     */
    function getNativeTokenBalance() external view returns (uint256) {
        return address(this).balance;
    }

    /**
     * @dev Emergency withdrawal of all tokens (admin only)
     * @param token The token address (address(0) for native token)
     * @param to The recipient address
     */
    function emergencyWithdraw(
        address token,
        address to
    ) external onlyRole(ADMIN_ROLE) nonReentrant {
        require(to != address(0), "Invalid recipient address");

        if (token == address(0)) {
            // Withdraw all native tokens
            uint256 balance = address(this).balance;
            require(balance > 0, "No balance to withdraw");

            (bool success, ) = to.call{value: balance}("");
            require(success, "Native token transfer failed");
            
            emit EmergencyWithdrawal(address(0), to, balance);
        } else {
            // Withdraw all ERC20 tokens
            uint256 balance = IERC20(token).balanceOf(address(this));
            require(balance > 0, "No balance to withdraw");

            IERC20(token).safeTransfer(to, balance);
            
            emit EmergencyWithdrawal(token, to, balance);
        }
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