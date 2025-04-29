// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "./interfaces/IIntentRegistry.sol";

/**
 * @title IntentRegistry
 * @dev Contract for registering and managing user intents
 * 
 * This contract stores and manages user intents for transactions.
 * Intents represent a user's desire to perform a specific action,
 * which can be executed later by the protocol.
 */
contract IntentRegistry is IIntentRegistry, AccessControl, ReentrancyGuard, Pausable {
    // Role definitions
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    bytes32 public constant EXECUTOR_ROLE = keccak256("EXECUTOR_ROLE");

    // Intent storage
    mapping(bytes32 => Intent) public intents;
    mapping(address => uint256) public userNonces;

    // Events
    event IntentCreated(
        bytes32 indexed intentId,
        address indexed user,
        address tokenAddress,
        uint256 amount,
        address recipient,
        uint256 expiresAt,
        IntentType intentType
    );
    event IntentExecuted(bytes32 indexed intentId, address indexed executor);
    event IntentCancelled(bytes32 indexed intentId);

    /**
     * @dev Constructor
     * @param _admin Admin address that will have full control
     * @param _executor Address that can execute intents (typically the main protocol contract)
     */
    constructor(address _admin, address _executor) {
        require(_admin != address(0), "Admin cannot be zero address");
        require(_executor != address(0), "Executor cannot be zero address");

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ADMIN_ROLE, _admin);
        _grantRole(EXECUTOR_ROLE, _executor);
    }

    /**
     * @dev Create a new intent
     * @param tokenAddress The address of the token (address(0) for native token)
     * @param amount The amount to transfer
     * @param recipient The recipient address
     * @param expiresAt The expiration timestamp
     * @param intentType The type of intent
     * @param additionalParams Additional parameters for the intent (if any)
     * @return intentId The ID of the created intent
     */
    function createIntent(
        address tokenAddress,
        uint256 amount,
        address recipient,
        uint256 expiresAt,
        IntentType intentType,
        bytes calldata additionalParams
    ) external whenNotPaused nonReentrant returns (bytes32 intentId) {
        require(recipient != address(0), "Recipient cannot be zero address");
        require(amount > 0, "Amount must be greater than zero");
        require(expiresAt > block.timestamp, "Expiration must be in the future");

        // Increment user nonce
        uint256 nonce = userNonces[msg.sender]++;

        // Generate intent ID
        intentId = keccak256(abi.encodePacked(
            msg.sender,
            tokenAddress,
            amount,
            recipient,
            expiresAt,
            nonce
        ));

        // Store intent
        intents[intentId] = Intent({
            user: msg.sender,
            tokenAddress: tokenAddress,
            amount: amount,
            recipient: recipient,
            expiresAt: expiresAt,
            intentType: intentType,
            additionalParams: additionalParams,
            isExecuted: false,
            isCancelled: false,
            createdAt: block.timestamp
        });

        emit IntentCreated(
            intentId,
            msg.sender,
            tokenAddress,
            amount,
            recipient,
            expiresAt,
            intentType
        );

        return intentId;
    }

    /**
     * @dev Mark an intent as executed
     * @param intentId The ID of the intent to mark as executed
     */
    function markIntentExecuted(bytes32 intentId) external override onlyRole(EXECUTOR_ROLE) {
        Intent storage intent = intents[intentId];
        
        require(intent.user != address(0), "Intent does not exist");
        require(!intent.isExecuted, "Intent already executed");
        require(!intent.isCancelled, "Intent is cancelled");
        require(block.timestamp <= intent.expiresAt, "Intent expired");

        intent.isExecuted = true;
        
        emit IntentExecuted(intentId, msg.sender);
    }

    /**
     * @dev Cancel an intent
     * @param intentId The ID of the intent to cancel
     */
    function cancelIntent(bytes32 intentId) external nonReentrant {
        Intent storage intent = intents[intentId];
        
        require(intent.user != address(0), "Intent does not exist");
        require(intent.user == msg.sender || hasRole(ADMIN_ROLE, msg.sender), "Not authorized");
        require(!intent.isExecuted, "Intent already executed");
        require(!intent.isCancelled, "Intent already cancelled");

        intent.isCancelled = true;
        
        emit IntentCancelled(intentId);
    }

    /**
     * @dev Get intent details
     * @param intentId The ID of the intent
     * @return user The user who created the intent
     * @return tokenAddress The token address
     * @return amount The amount
     * @return recipient The recipient address
     * @return expiresAt The expiration timestamp
     * @return isExecuted Whether the intent has been executed
     */
    function getIntent(bytes32 intentId) external view override returns (
        address user,
        address tokenAddress,
        uint256 amount,
        address recipient,
        uint256 expiresAt,
        bool isExecuted
    ) {
        Intent storage intent = intents[intentId];
        
        return (
            intent.user,
            intent.tokenAddress,
            intent.amount,
            intent.recipient,
            intent.expiresAt,
            intent.isExecuted
        );
    }

    /**
     * @dev Get full intent details
     * @param intentId The ID of the intent
     * @return The full intent struct
     */
    function getIntentDetails(bytes32 intentId) external view returns (Intent memory) {
        return intents[intentId];
    }

    /**
     * @dev Check if an intent is valid for execution
     * @param intentId The ID of the intent
     * @return valid Whether the intent is valid
     * @return reason The reason if the intent is invalid
     */
    function isIntentValid(bytes32 intentId) external view returns (bool valid, string memory reason) {
        Intent storage intent = intents[intentId];
        
        if (intent.user == address(0)) {
            return (false, "Intent does not exist");
        }
        
        if (intent.isExecuted) {
            return (false, "Intent already executed");
        }
        
        if (intent.isCancelled) {
            return (false, "Intent is cancelled");
        }
        
        if (block.timestamp > intent.expiresAt) {
            return (false, "Intent expired");
        }
        
        return (true, "");
    }

    /**
     * @dev Get the current nonce for a user
     * @param user The user address
     * @return The current nonce
     */
    function getCurrentNonce(address user) external view returns (uint256) {
        return userNonces[user];
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