// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title IIntentRegistry
 * @dev Interface for the IntentRegistry contract
 */
interface IIntentRegistry {
    // Intent types
    enum IntentType {
        Send,
        Swap,
        Stake,
        Approve,
        Other
    }

    // Intent struct
    struct Intent {
        address user;
        address tokenAddress;
        uint256 amount;
        address recipient;
        uint256 expiresAt;
        IntentType intentType;
        bytes additionalParams;
        bool isExecuted;
        bool isCancelled;
        uint256 createdAt;
    }

    /**
     * @dev Mark an intent as executed
     * @param intentId The ID of the intent to mark as executed
     */
    function markIntentExecuted(bytes32 intentId) external;

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
    function getIntent(bytes32 intentId) external view returns (
        address user,
        address tokenAddress,
        uint256 amount,
        address recipient,
        uint256 expiresAt,
        bool isExecuted
    );
}