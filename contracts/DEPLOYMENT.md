# Phoenix Protocol Deployment Guide

This document provides a step-by-step guide for deploying the Phoenix Protocol smart contracts.

## Prerequisites

Before deploying the contracts, ensure you have the following:

1. Node.js (v14 or later) and npm installed
2. A wallet with sufficient funds for deployment (ETH for Ethereum networks, AVAX for Avalanche networks)
3. API keys for Infura (for Ethereum networks) and/or other RPC providers
4. API keys for Etherscan/Snowtrace for contract verification

## Environment Setup

1. Create a `.env` file in the root directory with the following variables:

```
# Blockchain Configuration
ADMIN_WALLET_ADDRESS=0x9086994E481e60ceeb6C32F1af28C8B2ef363FFE
INFURA_API_KEY=your-infura-api-key
PRIVATE_KEY=your-private-key-here

# Contract Addresses (to be filled after deployment)
PHOENIX_PROTOCOL_ADDRESS=
INTENT_REGISTRY_ADDRESS=
RECOVERY_MODULE_ADDRESS=
TOKEN_VAULT_ADDRESS=

# Network Configuration
DEFAULT_CHAIN_ID=43113  # Avalanche Fuji Testnet

# API Keys for Contract Verification
ETHERSCAN_API_KEY=your-etherscan-api-key
SNOWTRACE_API_KEY=your-snowtrace-api-key

# Protocol Configuration
PROTOCOL_FEE_PERCENTAGE=100  # 1% (in basis points)
RECOVERY_FEE_PERCENTAGE=500  # 5% (in basis points)
RECOVERY_COOLDOWN=86400  # 24 hours (in seconds)
```

2. Install dependencies:

```bash
cd contracts
npm install
```

## Compilation

Compile the smart contracts:

```bash
npx hardhat compile
```

## Deployment

### Testnet Deployment

Deploy to a testnet (e.g., Avalanche Fuji):

```bash
npm run deploy:testnet
# or
npx hardhat run scripts/deploy.js --network fuji
```

### Mainnet Deployment

Deploy to mainnet (e.g., Avalanche C-Chain):

```bash
npm run deploy:mainnet
# or
npx hardhat run scripts/deploy.js --network avalanche
```

## Post-Deployment Steps

After deployment, the script will output the addresses of the deployed contracts. Update your `.env` file with these addresses:

```
PHOENIX_PROTOCOL_ADDRESS=0x...
INTENT_REGISTRY_ADDRESS=0x...
RECOVERY_MODULE_ADDRESS=0x...
TOKEN_VAULT_ADDRESS=0x...
```

## Contract Verification

Verify the contracts on the blockchain explorer:

```bash
# For testnet
npm run verify:testnet
# or
npx hardhat run scripts/verify.js --network fuji

# For mainnet
npm run verify:mainnet
# or
npx hardhat run scripts/verify.js --network avalanche
```

## Contract Interaction

After deployment, you can interact with the contracts using the provided scripts:

```bash
# Check contract information
npx hardhat run scripts/interact.js --network fuji

# Create an intent
npx hardhat run scripts/create-intent.js --network fuji 0x0000000000000000000000000000000000000000 0.1 0xRecipientAddress 24

# Execute an intent
npx hardhat run scripts/execute-intent.js --network fuji 0xIntentId

# Request recovery
npx hardhat run scripts/request-recovery.js --network fuji 0xTransactionId "Transaction sent to wrong address"

# Approve recovery (admin only)
npx hardhat run scripts/approve-recovery.js --network fuji 0xRecoveryId 0x0000000000000000000000000000000000000000 0.1 0xRecipientAddress

# Execute recovery
npx hardhat run scripts/execute-recovery.js --network fuji 0xRecoveryId
```

## Security Considerations

Before deploying to mainnet, consider the following:

1. **Contract Audit**: Have the contracts audited by a reputable security firm.
2. **Test Coverage**: Ensure comprehensive test coverage for all contract functions.
3. **Gradual Rollout**: Consider a phased deployment approach, starting with limited functionality.
4. **Emergency Procedures**: Have a plan for handling security incidents, including contract pausing and emergency withdrawals.

## Troubleshooting

If you encounter issues during deployment:

1. **Gas Issues**: Ensure your wallet has sufficient funds for deployment.
2. **Network Congestion**: Increase gas price during periods of high network activity.
3. **Verification Failures**: Double-check that the constructor arguments match those used during deployment.
4. **Contract Size**: If contracts are too large, consider optimizing or splitting functionality.

## Support

For additional support, refer to the documentation or contact the Phoenix Protocol team.