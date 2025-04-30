# Cardano Integration Guide

This document provides a comprehensive guide to the Cardano integration in Phoenix Protocol, including architecture, implementation details, and usage instructions.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Smart Contracts](#smart-contracts)
4. [Wallet Integration](#wallet-integration)
5. [Bridge Mechanism](#bridge-mechanism)
6. [Development Setup](#development-setup)
7. [Deployment](#deployment)
8. [Testing](#testing)
9. [Troubleshooting](#troubleshooting)
10. [Future Enhancements](#future-enhancements)

## Overview

Phoenix Protocol's Cardano integration extends the protocol's smart contract recovery and UX protection middleware to the Cardano blockchain. This integration leverages Cardano's UTXO model, formal verification capabilities, and native multi-asset support to provide a secure and efficient platform for transaction protection.

The integration consists of:
- Plutus smart contracts for intent registry and recovery
- Wallet integration with popular Cardano wallets (Nami, Eternl, Flint)
- Cross-chain bridge mechanism for asset and intent synchronization
- Frontend components for interacting with Cardano contracts

## Architecture

The Cardano integration follows a UTXO-based architecture, which differs from the account-based model used in Avalanche. The key components are:

### Intent Registry

The Intent Registry stores user intents as transaction metadata in the Cardano blockchain. Each intent includes:
- Owner address
- Action type
- Parameters
- Timestamp
- Expiry time

### Recovery Module

The Recovery Module manages the recovery process for transactions. It includes:
- Recovery request storage
- Guardian approval mechanism
- M-of-N threshold for approvals
- Recovery execution logic

### Bridge Service

The Bridge Service facilitates cross-chain communication between Avalanche and Cardano. It handles:
- Asset transfers between chains
- Intent synchronization
- Transaction status tracking
- Fee calculation and payment

## Smart Contracts

The Cardano integration uses Plutus scripts for smart contract functionality. The main contracts are:

### IntentRegistry.hs

The Intent Registry contract handles the creation, execution, and cancellation of intents. It validates that:
- The intent owner is signing the transaction
- The intent is not expired
- The intent has not already been executed

```haskell
mkIntentValidator :: IntentDatum -> IntentRedeemer -> ScriptContext -> Bool
mkIntentValidator datum redeemer ctx = case redeemer of
    RegisterIntent intent' -> 
        -- Check that the intent owner is signing the transaction
        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner intent')) &&
        -- Check that the intent is not expired
        traceIfFalse "Intent is expired" (intentExpiry intent' > currentTime)
    
    ExecuteIntent intentId' ->
        -- Check that the intent ID matches
        traceIfFalse "Intent ID mismatch" (intentId datum == intentId') &&
        -- Check that the intent is not executed
        traceIfFalse "Intent already executed" (not $ isExecuted datum) &&
        -- Check that the intent is not expired
        traceIfFalse "Intent is expired" (intentExpiry (intent datum) > currentTime) &&
        -- Check that the intent owner is signing the transaction
        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner (intent datum)))
    
    CancelIntent intentId' ->
        -- Check that the intent ID matches
        traceIfFalse "Intent ID mismatch" (intentId datum == intentId') &&
        -- Check that the intent is not executed
        traceIfFalse "Intent already executed" (not $ isExecuted datum) &&
        -- Check that the intent owner is signing the transaction
        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner (intent datum)))
```

### RecoveryModule.hs

The Recovery Module contract handles the recovery process for transactions. It validates that:
- The recovery request is valid
- The guardians are authorized to approve the recovery
- The required number of approvals has been reached

```haskell
mkRecoveryValidator :: RecoveryDatum -> RecoveryRedeemer -> ScriptContext -> Bool
mkRecoveryValidator datum redeemer ctx = case redeemer of
    RequestRecovery request' -> 
        -- Check that the request owner is signing the transaction
        traceIfFalse "Request owner must sign" (txSignedBy info (requestOwner request'))
    
    ApproveRecovery recoveryId' approval ->
        -- Check that the recovery ID matches
        traceIfFalse "Recovery ID mismatch" (recoveryId datum == recoveryId') &&
        -- Check that the recovery is not executed
        traceIfFalse "Recovery already executed" (not $ isExecuted datum) &&
        -- Check that the guardian is signing the transaction
        traceIfFalse "Guardian must sign" (txSignedBy info (guardian approval))
    
    ExecuteRecovery recoveryId' ->
        -- Check that the recovery ID matches
        traceIfFalse "Recovery ID mismatch" (recoveryId datum == recoveryId') &&
        -- Check that the recovery is not executed
        traceIfFalse "Recovery already executed" (not $ isExecuted datum) &&
        -- Check that there are enough approvals
        traceIfFalse "Not enough approvals" (countApprovals (approvals datum) >= requiredApprovals datum)
```

## Wallet Integration

The Cardano integration supports popular Cardano wallets, including:
- Nami
- Eternl
- Flint

The wallet integration is implemented in the `wallet-connect.ts` file, which provides functions for:
- Connecting to Cardano wallets
- Signing transactions
- Getting wallet addresses and balances
- Submitting transactions to the Cardano network

```typescript
// Connect to Cardano wallet
async function connectCardanoWallet(): Promise<WalletInfo> {
  // Check if Cardano wallets are available
  if (typeof window === "undefined") {
    throw new Error("Cannot connect to Cardano wallet in server environment");
  }
  
  // Check for Nami wallet
  if (window.cardano?.nami) {
    return connectNamiWallet();
  }
  
  // Check for Eternl wallet
  if (window.cardano?.eternl) {
    return connectEternlWallet();
  }
  
  // Check for Flint wallet
  if (window.cardano?.flint) {
    return connectFlintWallet();
  }
  
  throw new Error("No Cardano wallet found. Please install Nami, Eternl, or Flint wallet.");
}
```

## Bridge Mechanism

The bridge mechanism facilitates cross-chain communication between Avalanche and Cardano. It uses Milkomeda, a sidechain that enables EVM compatibility on Cardano, to bridge assets and synchronize intents.

The bridge service provides the following functions:
- `bridgeFromAvalancheToCardano`: Transfer assets from Avalanche to Cardano
- `bridgeFromCardanoToAvalanche`: Transfer assets from Cardano to Avalanche
- `syncIntent`: Synchronize an intent across chains

```typescript
/**
 * Bridge assets from Avalanche to Cardano
 */
public async bridgeFromAvalancheToCardano(
  sourceAddress: string,
  targetAddress: string,
  amount: string,
  token: string,
  provider: ethers.providers.Web3Provider
): Promise<string> {
  try {
    // In a real implementation, this would interact with a bridge contract
    // For now, we'll just simulate the process
    
    // Create a transaction record
    const txHash = ethers.utils.id(Date.now().toString());
    const transaction: BridgeTransaction = {
      sourceChainId: CHAIN_IDS.AVALANCHE_FUJI,
      targetChainId: CHAIN_IDS.CARDANO_TESTNET,
      sourceAddress,
      targetAddress,
      amount,
      token,
      status: 'pending',
      txHash,
      timestamp: Date.now(),
    };
    
    // Add to transactions and save
    this.transactions.push(transaction);
    this.saveToLocalStorage();
    
    // Simulate processing time
    setTimeout(() => {
      this.updateTransactionStatus(txHash, 'completed');
    }, 5000);
    
    return txHash;
  } catch (error) {
    console.error('Error bridging from Avalanche to Cardano:', error);
    throw new Error('Failed to bridge assets');
  }
}
```

## Development Setup

To set up the Cardano development environment:

1. Install the Cardano development tools:
   ```bash
   # Install Haskell and Cabal
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   
   # Install Cardano Node and CLI
   ghcup install ghc 8.10.7
   ghcup set ghc 8.10.7
   cabal update
   ```

2. Clone the Phoenix Protocol repository:
   ```bash
   git clone https://github.com/yourusername/Phoenix-Protocol.git
   cd Phoenix-Protocol
   ```

3. Set up the Cardano development environment:
   ```bash
   cd cardano
   cabal build
   ```

4. Compile the Plutus scripts:
   ```bash
   cabal run compile-scripts -- scripts
   ```

## Deployment

To deploy the Cardano contracts:

1. Make sure you have a funded Cardano wallet:
   ```bash
   # Generate a new key pair if needed
   cardano-cli address key-gen \
     --verification-key-file payment.vkey \
     --signing-key-file payment.skey
   
   # Build the address
   cardano-cli address build \
     --payment-verification-key-file payment.vkey \
     --testnet-magic 1097911063 \
     --out-file payment.addr
   ```

2. Fund the address from the Cardano testnet faucet.

3. Deploy the contracts:
   ```bash
   cd cardano/scripts
   ./deploy-contracts.sh testnet
   ```

4. Update the contract addresses in the frontend:
   ```bash
   # The script will output the contract addresses
   # Update these in the frontend .env file
   ```

## Testing

To test the Cardano integration:

1. Run the Plutus script tests:
   ```bash
   cd cardano
   cabal test
   ```

2. Test the contracts on the testnet:
   ```bash
   cd cardano/scripts
   
   # Register an intent
   ./interact-with-contracts.sh testnet register-intent "swap" "token1,token2,100" "1717171717"
   
   # Execute an intent
   ./interact-with-contracts.sh testnet execute-intent "intent-id"
   
   # Request recovery
   ./interact-with-contracts.sh testnet request-recovery "tx-id" "wrong-amount" "100" "ADA"
   ```

3. Test the bridge functionality in the frontend:
   - Connect your Cardano wallet
   - Navigate to the Bridge page
   - Try bridging assets between Avalanche and Cardano

## Troubleshooting

Common issues and solutions:

1. **Wallet Connection Issues**:
   - Make sure you have a compatible Cardano wallet installed (Nami, Eternl, Flint)
   - Check that your wallet is connected to the correct network (testnet/mainnet)
   - Try refreshing the page or reconnecting the wallet

2. **Transaction Failures**:
   - Ensure you have enough ADA for transaction fees
   - Check that the transaction parameters are valid
   - Verify that you're signing with the correct wallet

3. **Bridge Issues**:
   - Make sure both source and target chains are properly configured
   - Check that you have sufficient balance on the source chain
   - Verify that the bridge service is running and accessible

## Future Enhancements

Planned enhancements for the Cardano integration:

1. **Performance Optimization**:
   - Optimize Plutus scripts for lower execution costs
   - Implement batching for multiple intents
   - Use reference inputs for shared data

2. **Security Enhancements**:
   - Formal verification of all Plutus scripts
   - Multi-signature approval for high-value transactions
   - Time-locked recovery mechanisms

3. **Feature Additions**:
   - Support for Cardano native tokens
   - Integration with Cardano DeFi protocols
   - Governance mechanisms for protocol updates

4. **User Experience**:
   - Improved wallet integration with better error handling
   - Real-time transaction monitoring
   - Mobile-friendly interfaces for Cardano interactions