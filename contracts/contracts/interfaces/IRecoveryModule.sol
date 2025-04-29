// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title IRecoveryModule
 * @dev Interface for the RecoveryModule contract
 */
interface IRecoveryModule {
    // Recovery status enum
    enum RecoveryStatus {
        Requested,
        Approved,
        Rejected,
        Completed
    }

    // Recovery request struct
    struct RecoveryRequest {
        address requester;
        bytes32 transactionId;
        RecoveryStatus status;
        string reason;
        address tokenAddress;
        uint256 amount;
        address recipient;
        uint256 requestedAt;
        uint256 updatedAt;
        uint256 executedAt;
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
    ) external returns (bytes32 recoveryId);

    /**
     * @dev Update recovery request status
     * @param recoveryId The ID of the recovery request
     * @param status The new status
     */
    function updateRecoveryStatus(
        bytes32 recoveryId,
        RecoveryStatus status
    ) external;

    /**
     * @dev Execute an approved recovery
     * @param recoveryId The ID of the recovery request
     */
    function executeRecovery(bytes32 recoveryId) external;
}