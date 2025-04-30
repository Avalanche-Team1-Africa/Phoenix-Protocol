# Phoenix Protocol Integration Guide

This guide explains how to integrate the frontend, backend, and smart contracts of the Phoenix Protocol project.

## Prerequisites

- Node.js (v14 or later)
- npm or yarn
- MongoDB (for backend)
- MetaMask or another Ethereum wallet

## Step 1: Deploy Smart Contracts

First, deploy the smart contracts to the blockchain:

```bash
# Navigate to the contracts directory
cd contracts

# Install dependencies
npm install

# Compile contracts
npx hardhat compile

# Deploy to testnet (e.g., Avalanche Fuji)
npx hardhat run scripts/deploy.js --network fuji
```

After deployment, note the contract addresses that are output by the script.

## Step 2: Update Contract Addresses

Update all `.env` files with the deployed contract addresses:

```bash
# Navigate to the root directory
cd ..

# Run the update script with the contract addresses
node scripts/update-addresses.js <phoenix-protocol-address> <intent-registry-address> <recovery-module-address> <token-vault-address>
```

This script will update the contract addresses in all `.env` files.

## Step 3: Start the Backend

Start the backend server:

```bash
# Navigate to the backend directory
cd backend

# Install dependencies
npm install

# Start the server
npm start
```

The backend server will start on port 3001 (or the port specified in the `.env` file).

## Step 4: Start the Frontend

Start the frontend development server:

```bash
# Navigate to the frontend directory
cd frontend

# Install dependencies
npm install

# Start the development server
npm start
```

The frontend development server will start on port 3000 and open in your default browser.

## Step 5: Connect MetaMask

1. Install MetaMask in your browser if you haven't already.
2. Connect MetaMask to the Avalanche Fuji Testnet.
3. Add some test AVAX to your wallet from the [Avalanche Faucet](https://faucet.avax.network/).
4. Connect your wallet to the Phoenix Protocol application.

## Step 6: Test the Integration

Test the integration by performing the following actions:

1. **Create an Intent**: Create a new intent to send tokens to another address.
2. **Execute an Intent**: Execute an intent that you've created.
3. **Request Recovery**: Request recovery for a transaction.
4. **Approve Recovery**: Approve a recovery request (admin only).
5. **Execute Recovery**: Execute an approved recovery.

## Troubleshooting

If you encounter any issues during integration:

1. **Contract Deployment Issues**:
   - Make sure your wallet has enough funds for deployment.
   - Check that you're connected to the correct network.

2. **Backend Issues**:
   - Make sure MongoDB is running.
   - Check the backend logs for any errors.

3. **Frontend Issues**:
   - Make sure the contract addresses are correctly set in the `.env` file.
   - Check the browser console for any errors.

4. **MetaMask Issues**:
   - Make sure you're connected to the correct network.
   - Try resetting your MetaMask account if transactions are failing.

## Next Steps

After successful integration, you can:

1. **Add More Features**: Add additional features to the application.
2. **Improve UI/UX**: Enhance the user interface and experience.
3. **Add Tests**: Add more tests to ensure the application works correctly.
4. **Deploy to Mainnet**: Deploy the application to the mainnet when ready.