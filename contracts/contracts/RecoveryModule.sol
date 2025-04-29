// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "./interfaces/IRecoveryModule.sol";
import "./interfaces/ITokenVault.sol";

/**
 * @title RecoveryModule
 * @dev Contract for handling transaction recovery requests
 * 
 * This contract manages the recovery process for transactions,
 * allowing users to request recovery of funds and administrators
 * to approve or reject these requests.
 */
contract RecoveryModule is IRecoveryModule, AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // Role definitions
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant RECOVERY_ROLE = keccak256("RECOVERY_ROLE");
    bytes32 public constant PROTOCOL_ROLE = keccak256("PROTOCOL_ROLE");

    // Recovery request storage
    mapping(bytes32 => RecoveryRequest) public recoveryRequests;
    mapping(bytes32 => bool) public processedTransactions;
    
    // Token vault for executing recoveries
    ITokenVault public tokenVault;
    
    // Recovery fee configuration
    uint256 public recoveryFeePercentage; // Fee in basis points (1/100 of a percent)
    address public feeCollector;
    
    // Cooldown period for recovery requests (in seconds)
    uint256 public recoveryCooldown;
    
    // Events
    event RecoveryRequested(
        bytes32 indexed recoveryId,
        address indexed requester,
        bytes32 indexed transactionId,
        string reason
    );
    event RecoveryStatusUpdated(
        bytes32 indexed recoveryId,
        RecoveryStatus status,
        address updatedBy
    );
    event RecoveryExecuted(
        bytes32 indexed recoveryId,
        address indexed requester,
        address indexed recipient,
        address tokenAddress,
        uint256 amount,
        uint256 fee
    );
    event RecoveryFeeUpdated(uint256 newFeePercentage);
    event FeeCollectorUpdated(address newFeeCollector);
    event RecoveryCooldownUpdated(uint256 newCooldown);

    /**
     * @dev Constructor
     * @param _admin Admin address that will have full control
     * @param _feeCollector Address that will receive recovery fees
     * @param _recoveryFeePercentage Initial recovery fee percentage (in basis points)
     * @param _recoveryCooldown Initial cooldown period for recovery requests (in seconds)
     */
    constructor(
        address _admin,
        address _feeCollector,
        uint256 _recoveryFeePercentage,
        uint256 _recoveryCooldown
    ) {
        require(_admin != address(0), "Admin cannot be zero address");
        require(_feeCollector != address(0), "Fee collector cannot be zero address");
        require(_recoveryFeePercentage <= 2000, "Fee percentage too high"); // Max 20%

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ADMIN_ROLE, _admin);
        _grantRole(RECOVERY_ROLE, _admin);
        
        feeCollector = _feeCollector;
        recoveryFeePercentage = _recoveryFeePercentage;
        recoveryCooldown = _recoveryCooldown;
    }

    /**
     * @dev Set the token vault
     * @param _tokenVault Address of the token vault contract
     */
    function setTokenVault(address _tokenVault) external onlyRole(ADMIN_ROLE) {
        require(_tokenVault != address(0), "Invalid token vault address");
        tokenVault = ITokenVault(_tokenVault);
    }

    /**
     * @dev Update the recovery fee percentage
     * @param _recoveryFeePercentage New fee percentage (in basis points)
     */
    function updateRecoveryFee(uint256 _recoveryFeePercentage) external onlyRole(ADMIN_ROLE) {
        require(_recoveryFeePercentage <= 2000, "Fee percentage too high"); // Max 20%
        recoveryFeePercentage = _recoveryFeePercentage;
        emit RecoveryFeeUpdated(_recoveryFeePercentage);
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
     * @dev Update the recovery cooldown period
     * @param _recoveryCooldown New cooldown period (in seconds)
     */
    function updateRecoveryCooldown(uint256 _recoveryCooldown) external onlyRole(ADMIN_ROLE) {
        recoveryCooldown = _recoveryCooldown;
        emit RecoveryCooldownUpdated(_recoveryCooldown);
    }

    /**
     * @dev Request recovery for a transaction
     * @param requester The address requesting recovery
     * @param transactionId The ID of the transaction to recover
     * @param reason The reason for recovery
     * @return recoveryId The ID of the created recovery request
     */
    function requestRecovery(
        address requester,
        bytes32 transactionId,
        string calldata reason
    ) external override whenNotPaused nonReentrant returns (bytes32 recoveryId) {
        require(msg.sender == requester || hasRole(PROTOCOL_ROLE, msg.sender), "Not authorized");
        require(!processedTransactions[transactionId], "Transaction already processed for recovery");
        
        // Generate recovery ID
        recoveryId = keccak256(abi.encodePacked(
            requester,
            transactionId,
            block.timestamp,
            reason
        ));
        
        // Store recovery request
        recoveryRequests[recoveryId] = RecoveryRequest({
            requester: requester,
            transactionId: transactionId,
            status: RecoveryStatus.Requested,
            reason: reason,
            tokenAddress: address(0), // Will be filled during approval
            amount: 0, // Will be filled during approval
            recipient: address(0), // Will be filled during approval
            requestedAt: block.timestamp,
            updatedAt: block.timestamp,
            executedAt: 0
        });
        
        // Mark transaction as processed
        processedTransactions[transactionId] = true;
        
        emit RecoveryRequested(recoveryId, requester, transactionId, reason);
        
        return recoveryId;
    }

    /**
     * @dev Update recovery request status
     * @param recoveryId The ID of the recovery request
     * @param status The new status
     */
    function updateRecoveryStatus(
        bytes32 recoveryId,
        RecoveryStatus status
    ) external override onlyRole(RECOVERY_ROLE) {
        RecoveryRequest storage request = recoveryRequests[recoveryId];
        
        require(request.requester != address(0), "Recovery request does not exist");
        require(request.status == RecoveryStatus.Requested, "Invalid current status");
        require(
            status == RecoveryStatus.Approved || status == RecoveryStatus.Rejected,
            "Invalid new status"
        );
        
        request.status = status;
        request.updatedAt = block.timestamp;
        
        emit RecoveryStatusUpdated(recoveryId, status, msg.sender);
    }

    /**
     * @dev Set recovery details (after approval)
     * @param recoveryId The ID of the recovery request
     * @param tokenAddress The token address for recovery
     * @param amount The amount to recover
     * @param recipient The recipient address
     */
    function setRecoveryDetails(
        bytes32 recoveryId,
        address tokenAddress,
        uint256 amount,
        address recipient
    ) external onlyRole(RECOVERY_ROLE) {
        RecoveryRequest storage request = recoveryRequests[recoveryId];
        
        require(request.requester != address(0), "Recovery request does not exist");
        require(request.status == RecoveryStatus.Approved, "Recovery not approved");
        require(recipient != address(0), "Recipient cannot be zero address");
        require(amount > 0, "Amount must be greater than zero");
        
        request.tokenAddress = tokenAddress;
        request.amount = amount;
        request.recipient = recipient;
        request.updatedAt = block.timestamp;
    }

    /**
     * @dev Execute an approved recovery
     * @param recoveryId The ID of the recovery request
     */
    function executeRecovery(bytes32 recoveryId) external override nonReentrant {
        RecoveryRequest storage request = recoveryRequests[recoveryId];
        
        require(request.requester != address(0), "Recovery request does not exist");
        require(request.status == RecoveryStatus.Approved, "Recovery not approved");
        require(request.executedAt == 0, "Recovery already executed");
        require(request.recipient != address(0), "Recovery details not set");
        require(request.amount > 0, "Recovery amount not set");
        
        // Check cooldown period
        require(
            block.timestamp >= request.updatedAt + recoveryCooldown,
            "Recovery cooldown period not elapsed"
        );
        
        // Calculate recovery fee
        uint256 recoveryFee = (request.amount * recoveryFeePercentage) / 10000;
        uint256 amountAfterFee = request.amount - recoveryFee;
        
        // Execute the recovery
        if (request.tokenAddress == address(0)) {
            // Native token recovery
            require(address(this).balance >= request.amount, "Insufficient balance");
            
            // Send to recipient
            (bool success, ) = request.recipient.call{value: amountAfterFee}("");
            require(success, "Native token transfer failed");
            
            // Send fee to collector
            if (recoveryFee > 0) {
                (bool feeSuccess, ) = feeCollector.call{value: recoveryFee}("");
                require(feeSuccess, "Fee transfer failed");
            }
        } else {
            // ERC20 token recovery
            // Transfer from vault to recipient
            tokenVault.transferToken(request.tokenAddress, request.recipient, amountAfterFee);
            
            // Transfer fee to collector
            if (recoveryFee > 0) {
                tokenVault.transferToken(request.tokenAddress, feeCollector, recoveryFee);
            }
        }
        
        // Update recovery status
        request.status = RecoveryStatus.Completed;
        request.executedAt = block.timestamp;
        
        emit RecoveryExecuted(
            recoveryId,
            request.requester,
            request.recipient,
            request.tokenAddress,
            request.amount,
            recoveryFee
        );
    }

    /**
     * @dev Get recovery request details
     * @param recoveryId The ID of the recovery request
     * @return The recovery request struct
     */
    function getRecoveryRequest(bytes32 recoveryId) external view returns (RecoveryRequest memory) {
        return recoveryRequests[recoveryId];
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
     * @dev Fallback function to receive ETH
     */
    receive() external payable {}
}