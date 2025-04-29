// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "./interfaces/IIntentRegistry.sol";
import "./interfaces/IRecoveryModule.sol";
import "./interfaces/ITokenVault.sol";

/**
 * @title PhoenixProtocol
 * @dev Main contract for the Phoenix Protocol system
 * 
 * This contract serves as the entry point for the Phoenix Protocol system.
 * It handles user intents, transaction execution, and recovery processes.
 */
contract PhoenixProtocol is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;
    using ECDSA for bytes32;

    // Role definitions
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    bytes32 public constant RECOVERY_ROLE = keccak256("RECOVERY_ROLE");

    // Protocol fee configuration
    uint256 public protocolFeePercentage; // Fee in basis points (1/100 of a percent)
    address public feeCollector;

    // Module addresses
    IIntentRegistry public intentRegistry;
    IRecoveryModule public recoveryModule;
    ITokenVault public tokenVault;

    // Events
    event ProtocolFeeUpdated(uint256 newFeePercentage);
    event FeeCollectorUpdated(address newFeeCollector);
    event ModuleUpdated(string moduleName, address moduleAddress);
    event TransactionExecuted(
        bytes32 indexed intentId,
        address indexed user,
        address indexed recipient,
        uint256 amount,
        address tokenAddress,
        uint256 protocolFee
    );
    event NativeTokenReceived(address indexed sender, uint256 amount);

    /**
     * @dev Constructor
     * @param _admin Admin address that will have full control
     * @param _feeCollector Address that will receive protocol fees
     * @param _protocolFeePercentage Initial protocol fee percentage (in basis points)
     */
    constructor(
        address _admin,
        address _feeCollector,
        uint256 _protocolFeePercentage
    ) {
        require(_admin != address(0), "Admin cannot be zero address");
        require(_feeCollector != address(0), "Fee collector cannot be zero address");
        require(_protocolFeePercentage <= 1000, "Fee percentage too high"); // Max 10%

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ADMIN_ROLE, _admin);
        
        feeCollector = _feeCollector;
        protocolFeePercentage = _protocolFeePercentage;
    }

    /**
     * @dev Fallback function to receive ETH
     */
    receive() external payable {
        emit NativeTokenReceived(msg.sender, msg.value);
    }

    /**
     * @dev Set the intent registry module
     * @param _intentRegistry Address of the intent registry contract
     */
    function setIntentRegistry(address _intentRegistry) external onlyRole(ADMIN_ROLE) {
        require(_intentRegistry != address(0), "Invalid intent registry address");
        intentRegistry = IIntentRegistry(_intentRegistry);
        emit ModuleUpdated("IntentRegistry", _intentRegistry);
    }

    /**
     * @dev Set the recovery module
     * @param _recoveryModule Address of the recovery module contract
     */
    function setRecoveryModule(address _recoveryModule) external onlyRole(ADMIN_ROLE) {
        require(_recoveryModule != address(0), "Invalid recovery module address");
        recoveryModule = IRecoveryModule(_recoveryModule);
        emit ModuleUpdated("RecoveryModule", _recoveryModule);
    }

    /**
     * @dev Set the token vault module
     * @param _tokenVault Address of the token vault contract
     */
    function setTokenVault(address _tokenVault) external onlyRole(ADMIN_ROLE) {
        require(_tokenVault != address(0), "Invalid token vault address");
        tokenVault = ITokenVault(_tokenVault);
        emit ModuleUpdated("TokenVault", _tokenVault);
    }

    /**
     * @dev Update the protocol fee percentage
     * @param _protocolFeePercentage New fee percentage (in basis points)
     */
    function updateProtocolFee(uint256 _protocolFeePercentage) external onlyRole(ADMIN_ROLE) {
        require(_protocolFeePercentage <= 1000, "Fee percentage too high"); // Max 10%
        protocolFeePercentage = _protocolFeePercentage;
        emit ProtocolFeeUpdated(_protocolFeePercentage);
    }

    /**
     * @dev Update the fee collector address
     * @param _feeCollector New fee collector address
     */
    function updateFeeCollector(address _feeCollector) external onlyRole(ADMIN_ROLE) {
        require(_feeCollector != address(0), "Fee collector cannot be zero address");
        feeCollector = _feeCollector;
        emit FeeCollectorUpdated(_feeCollector);
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

    /**
     * @dev Execute a transaction based on a signed intent
     * @param intentId The ID of the intent to execute
     * @param signature The signature of the intent owner
     */
    function executeIntent(
        bytes32 intentId,
        bytes calldata signature
    ) external nonReentrant whenNotPaused {
        // Verify intent exists and is valid
        (
            address user,
            address tokenAddress,
            uint256 amount,
            address recipient,
            uint256 expiresAt,
            bool isExecuted
        ) = intentRegistry.getIntent(intentId);
        
        require(user != address(0), "Intent does not exist");
        require(!isExecuted, "Intent already executed");
        require(block.timestamp <= expiresAt, "Intent expired");
        
        // Verify signature
        bytes32 messageHash = keccak256(abi.encodePacked(intentId, user, tokenAddress, amount, recipient, expiresAt));
        bytes32 ethSignedMessageHash = messageHash.toEthSignedMessageHash();
        address signer = ethSignedMessageHash.recover(signature);
        
        require(signer == user, "Invalid signature");
        
        // Calculate protocol fee
        uint256 protocolFee = (amount * protocolFeePercentage) / 10000;
        uint256 amountAfterFee = amount - protocolFee;
        
        // Execute the transfer
        if (tokenAddress == address(0)) {
            // Native token transfer
            require(address(this).balance >= amount, "Insufficient balance");
            
            // Send to recipient
            (bool success, ) = recipient.call{value: amountAfterFee}("");
            require(success, "Native token transfer failed");
            
            // Send fee to collector
            if (protocolFee > 0) {
                (bool feeSuccess, ) = feeCollector.call{value: protocolFee}("");
                require(feeSuccess, "Fee transfer failed");
            }
        } else {
            // ERC20 token transfer
            IERC20 token = IERC20(tokenAddress);
            
            // Transfer from vault to recipient
            tokenVault.transferToken(tokenAddress, recipient, amountAfterFee);
            
            // Transfer fee to collector
            if (protocolFee > 0) {
                tokenVault.transferToken(tokenAddress, feeCollector, protocolFee);
            }
        }
        
        // Mark intent as executed
        intentRegistry.markIntentExecuted(intentId);
        
        emit TransactionExecuted(
            intentId,
            user,
            recipient,
            amount,
            tokenAddress,
            protocolFee
        );
    }

    /**
     * @dev Request recovery for a transaction
     * @param transactionId The ID of the transaction to recover
     * @param reason The reason for recovery
     */
    function requestRecovery(
        bytes32 transactionId,
        string calldata reason
    ) external nonReentrant {
        recoveryModule.requestRecovery(msg.sender, transactionId, reason);
    }

    /**
     * @dev Approve a recovery request (admin only)
     * @param recoveryId The ID of the recovery request
     */
    function approveRecovery(bytes32 recoveryId) external onlyRole(RECOVERY_ROLE) {
        recoveryModule.updateRecoveryStatus(recoveryId, IRecoveryModule.RecoveryStatus.Approved);
    }

    /**
     * @dev Reject a recovery request (admin only)
     * @param recoveryId The ID of the recovery request
     */
    function rejectRecovery(bytes32 recoveryId) external onlyRole(RECOVERY_ROLE) {
        recoveryModule.updateRecoveryStatus(recoveryId, IRecoveryModule.RecoveryStatus.Rejected);
    }

    /**
     * @dev Execute a recovery (after approval)
     * @param recoveryId The ID of the approved recovery request
     */
    function executeRecovery(bytes32 recoveryId) external nonReentrant {
        recoveryModule.executeRecovery(recoveryId);
    }

    /**
     * @dev Grant a role to an account
     * @param role The role to grant
     * @param account The account to grant the role to
     */
    function grantProtocolRole(bytes32 role, address account) external onlyRole(ADMIN_ROLE) {
        _grantRole(role, account);
    }

    /**
     * @dev Revoke a role from an account
     * @param role The role to revoke
     * @param account The account to revoke the role from
     */
    function revokeProtocolRole(bytes32 role, address account) external onlyRole(ADMIN_ROLE) {
        _revokeRole(role, account);
    }
}