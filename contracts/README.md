# Phoenix Protocol Smart Contracts

This directory contains the smart contracts for the Phoenix Protocol project, a decentralized protocol for secure transaction management with intent-based execution and recovery mechanisms.

## Overview

The Phoenix Protocol consists of four main smart contracts:

1. **PhoenixProtocol**: The main contract that serves as the entry point for the protocol. It handles user intents, transaction execution, and recovery processes.

2. **IntentRegistry**: Stores and manages user intents for transactions. Intents represent a user's desire to perform a specific action, which can be executed later by the protocol.

3. **RecoveryModule**: Manages the recovery process for transactions, allowing users to request recovery of funds and administrators to approve or reject these requests.

4. **TokenVault**: Serves as a secure vault for storing tokens used in the protocol. It handles deposits, withdrawals, and transfers of both native and ERC20 tokens.

## Getting Started

### Prerequisites

- Node.js (v14 or later)
- npm or yarn
- Hardhat

### Installation

1. Install dependencies:
   ```bash
   npm install
   # or
   yarn install
   ```

2. Create a `.env` file in the root directory with the following variables:
   ```
   ADMIN_WALLET_ADDRESS=0x9086994E481e60ceeb6C32F1af28C8B2ef363FFE
   INFURA_API_KEY=your-infura-api-key
   PRIVATE_KEY=your-private-key-here
   ETHERSCAN_API_KEY=your-etherscan-api-key
   SNOWTRACE_API_KEY=your-snowtrace-api-key
   ```

### Compilation

Compile the smart contracts:

```bash
npx hardhat compile
```

### Testing

Run the test suite:

```bash
npx hardhat test
```

### Deployment

Deploy to a testnet (e.g., Avalanche Fuji):

```bash
npm run deploy:testnet
# or
yarn deploy:testnet
```

Deploy to mainnet (e.g., Avalanche C-Chain):

```bash
npm run deploy:mainnet
# or
yarn deploy:mainnet
```

### Verification

Verify contracts on the blockchain explorer:

```bash
npm run verify:testnet
# or
yarn verify:testnet
```

## Contract Details

### PhoenixProtocol

The main contract that coordinates all protocol operations. It handles:
- Intent execution
- Fee collection
- Recovery process initiation
- Module management

### IntentRegistry

Manages user intents with the following features:
- Intent creation
- Intent execution tracking
- Intent cancellation
- Intent validation

### RecoveryModule

Handles the recovery process with:
- Recovery request management
- Admin approval/rejection
- Recovery execution
- Fee collection for recovery services

### TokenVault

Securely manages tokens with:
- Token deposits
- Token withdrawals
- Token transfers
- Emergency withdrawal functionality

## Security Considerations

- All contracts use OpenZeppelin's security libraries
- Access control is implemented using role-based permissions
- Reentrancy protection is applied to all external functions
- Pausable functionality is available for emergency situations

## License

These contracts are licensed under the MIT License.