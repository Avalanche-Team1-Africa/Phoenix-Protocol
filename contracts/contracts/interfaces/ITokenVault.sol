// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title ITokenVault
 * @dev Interface for the TokenVault contract
 */
interface ITokenVault {
    /**
     * @dev Transfer ERC20 tokens from the vault
     * @param token The token address
     * @param to The recipient address
     * @param amount The amount to transfer
     */
    function transferToken(
        address token,
        address to,
        uint256 amount
    ) external;
}