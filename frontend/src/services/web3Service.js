import { ethers } from 'ethers';
import PhoenixProtocolABI from '../contracts/PhoenixProtocol.json';
import IntentRegistryABI from '../contracts/IntentRegistry.json';
import RecoveryModuleABI from '../contracts/RecoveryModule.json';
import TokenVaultABI from '../contracts/TokenVault.json';

class Web3Service {
  constructor() {
    this.provider = null;
    this.signer = null;
    this.phoenixProtocol = null;
    this.intentRegistry = null;
    this.recoveryModule = null;
    this.tokenVault = null;
    this.isInitialized = false;
    this.chainId = null;
    this.account = null;
  }

  async initialize() {
    try {
      // Check if MetaMask is installed
      if (window.ethereum) {
        // Create a new Web3Provider from MetaMask
        this.provider = new ethers.BrowserProvider(window.ethereum);
        
        // Get the network
        const network = await this.provider.getNetwork();
        this.chainId = network.chainId;
        
        // Get the signer
        this.signer = await this.provider.getSigner();
        
        // Get the connected account
        this.account = await this.signer.getAddress();
        
        // Initialize contract instances
        await this.initializeContracts();
        
        // Set up event listeners for account and network changes
        window.ethereum.on('accountsChanged', this.handleAccountsChanged.bind(this));
        window.ethereum.on('chainChanged', this.handleChainChanged.bind(this));
        
        this.isInitialized = true;
        return true;
      } else {
        console.error('MetaMask is not installed');
        return false;
      }
    } catch (error) {
      console.error('Error initializing Web3Service:', error);
      return false;
    }
  }

  async initializeContracts() {
    try {
      // Get contract addresses from environment variables
      const phoenixProtocolAddress = process.env.REACT_APP_PHOENIX_PROTOCOL_ADDRESS;
      const intentRegistryAddress = process.env.REACT_APP_INTENT_REGISTRY_ADDRESS;
      const recoveryModuleAddress = process.env.REACT_APP_RECOVERY_MODULE_ADDRESS;
      const tokenVaultAddress = process.env.REACT_APP_TOKEN_VAULT_ADDRESS;
      
      // Check if addresses are available
      if (!phoenixProtocolAddress || !intentRegistryAddress || !recoveryModuleAddress || !tokenVaultAddress) {
        console.error('Contract addresses not found in environment variables');
        return false;
      }
      
      // Initialize contract instances
      this.phoenixProtocol = new ethers.Contract(
        phoenixProtocolAddress,
        PhoenixProtocolABI.abi,
        this.signer
      );
      
      this.intentRegistry = new ethers.Contract(
        intentRegistryAddress,
        IntentRegistryABI.abi,
        this.signer
      );
      
      this.recoveryModule = new ethers.Contract(
        recoveryModuleAddress,
        RecoveryModuleABI.abi,
        this.signer
      );
      
      this.tokenVault = new ethers.Contract(
        tokenVaultAddress,
        TokenVaultABI.abi,
        this.signer
      );
      
      return true;
    } catch (error) {
      console.error('Error initializing contracts:', error);
      return false;
    }
  }

  handleAccountsChanged(accounts) {
    if (accounts.length === 0) {
      // MetaMask is locked or the user has not connected any accounts
      console.log('Please connect to MetaMask.');
      this.account = null;
    } else if (accounts[0] !== this.account) {
      // Account changed, update the account and reinitialize
      this.account = accounts[0];
      this.initialize();
    }
  }

  handleChainChanged() {
    // When the chain changes, reload the page
    window.location.reload();
  }

  async connectWallet() {
    try {
      if (!window.ethereum) {
        console.error('MetaMask is not installed');
        return { success: false, error: 'MetaMask is not installed' };
      }
      
      // Request account access
      const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
      
      if (accounts.length === 0) {
        return { success: false, error: 'No accounts found' };
      }
      
      this.account = accounts[0];
      
      // Initialize if not already initialized
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      return { success: true, account: this.account };
    } catch (error) {
      console.error('Error connecting wallet:', error);
      return { success: false, error: error.message };
    }
  }

  async createIntent(tokenAddress, amount, recipient, expiresInHours, intentType = 0) {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      // Convert amount to wei
      const amountInWei = ethers.parseEther(amount.toString());
      
      // Calculate expiration time (current time + hours in seconds)
      const expiresAt = Math.floor(Date.now() / 1000) + (expiresInHours * 3600);
      
      // Additional parameters (empty for now)
      const additionalParams = "0x";
      
      // Create intent
      const tx = await this.intentRegistry.createIntent(
        tokenAddress,
        amountInWei,
        recipient,
        expiresAt,
        intentType,
        additionalParams
      );
      
      // Wait for transaction to be mined
      const receipt = await tx.wait();
      
      // Get the intent ID from the event logs
      const intentCreatedEvent = receipt.logs
        .filter(log => {
          return log.topics[0] === ethers.id("IntentCreated(bytes32,address,address,uint256,address,uint256,uint8)");
        })
        .map(log => {
          const decodedLog = this.intentRegistry.interface.parseLog({
            topics: log.topics,
            data: log.data
          });
          return decodedLog.args;
        })[0];
      
      if (intentCreatedEvent) {
        const intentId = intentCreatedEvent[0];
        return { success: true, intentId, txHash: receipt.hash };
      } else {
        return { success: false, error: 'Could not find IntentCreated event in the logs' };
      }
    } catch (error) {
      console.error('Error creating intent:', error);
      return { success: false, error: error.message };
    }
  }

  async executeIntent(intentId) {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      // Get intent details
      const intentDetails = await this.intentRegistry.getIntentDetails(intentId);
      
      // Create message to sign
      const messageHash = ethers.keccak256(
        ethers.solidityPacked(
          ["bytes32", "address", "address", "uint256", "address", "uint256"],
          [
            intentId,
            intentDetails.user,
            intentDetails.tokenAddress,
            intentDetails.amount,
            intentDetails.recipient,
            intentDetails.expiresAt
          ]
        )
      );
      
      // Sign the message
      const signature = await this.signer.signMessage(ethers.getBytes(messageHash));
      
      // Execute the intent
      const tx = await this.phoenixProtocol.executeIntent(intentId, signature);
      
      // Wait for transaction to be mined
      const receipt = await tx.wait();
      
      return { success: true, txHash: receipt.hash };
    } catch (error) {
      console.error('Error executing intent:', error);
      return { success: false, error: error.message };
    }
  }

  async requestRecovery(transactionId, reason) {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      // Request recovery
      const tx = await this.phoenixProtocol.requestRecovery(transactionId, reason);
      
      // Wait for transaction to be mined
      const receipt = await tx.wait();
      
      // Get the recovery ID from the event logs
      const recoveryRequestedEvent = receipt.logs
        .filter(log => {
          // Check if this is a RecoveryRequested event
          return log.topics[0] === ethers.id("RecoveryRequested(bytes32,address,bytes32,string)");
        })
        .map(log => {
          const decodedLog = this.recoveryModule.interface.parseLog({
            topics: log.topics,
            data: log.data
          });
          return decodedLog.args;
        })[0];
      
      if (recoveryRequestedEvent) {
        const recoveryId = recoveryRequestedEvent[0];
        return { success: true, recoveryId, txHash: receipt.hash };
      } else {
        return { success: false, error: 'Could not find RecoveryRequested event in the logs' };
      }
    } catch (error) {
      console.error('Error requesting recovery:', error);
      return { success: false, error: error.message };
    }
  }

  async getUserIntents() {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      // Get current nonce (number of intents created by user)
      const nonce = await this.intentRegistry.getCurrentNonce(this.account);
      
      // Array to store intents
      const intents = [];
      
      // Loop through nonces to get all intents
      for (let i = 0; i < nonce; i++) {
        // Generate intent ID (this is a simplified version, actual implementation may vary)
        const intentId = await this.getIntentIdByNonce(i);
        
        if (intentId) {
          // Get intent details
          const intentDetails = await this.intentRegistry.getIntentDetails(intentId);
          
          // Add to array if the intent belongs to the current user
          if (intentDetails.user.toLowerCase() === this.account.toLowerCase()) {
            intents.push({
              id: intentId,
              tokenAddress: intentDetails.tokenAddress,
              amount: ethers.formatEther(intentDetails.amount),
              recipient: intentDetails.recipient,
              expiresAt: new Date(Number(intentDetails.expiresAt) * 1000),
              type: ["Send", "Swap", "Stake", "Approve", "Other"][intentDetails.intentType],
              status: intentDetails.isExecuted ? "Executed" : intentDetails.isCancelled ? "Cancelled" : "Pending",
              createdAt: new Date(Number(intentDetails.createdAt) * 1000)
            });
          }
        }
      }
      
      return { success: true, intents };
    } catch (error) {
      console.error('Error getting user intents:', error);
      return { success: false, error: error.message };
    }
  }

  // Helper method to get intent ID by nonce (simplified)
  async getIntentIdByNonce(nonce) {
    try {
      // This is a placeholder - in a real implementation, you would need to
      // either query events or have a mapping in the contract to get intent IDs by nonce
      return null;
    } catch (error) {
      console.error('Error getting intent ID by nonce:', error);
      return null;
    }
  }

  // Get native token balance
  async getNativeBalance() {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      const balance = await this.provider.getBalance(this.account);
      return { success: true, balance: ethers.formatEther(balance) };
    } catch (error) {
      console.error('Error getting native balance:', error);
      return { success: false, error: error.message };
    }
  }

  // Get token balance
  async getTokenBalance(tokenAddress) {
    try {
      if (!this.isInitialized) {
        await this.initialize();
      }
      
      // ERC20 token ABI (minimal for balanceOf)
      const abi = [
        'function balanceOf(address owner) view returns (uint256)',
        'function decimals() view returns (uint8)',
        'function symbol() view returns (string)'
      ];
      
      const tokenContract = new ethers.Contract(tokenAddress, abi, this.provider);
      
      const balance = await tokenContract.balanceOf(this.account);
      const decimals = await tokenContract.decimals();
      const symbol = await tokenContract.symbol();
      
      return {
        success: true,
        balance: ethers.formatUnits(balance, decimals),
        symbol,
        decimals
      };
    } catch (error) {
      console.error('Error getting token balance:', error);
      return { success: false, error: error.message };
    }
  }
}

// Create a singleton instance
const web3Service = new Web3Service();

export default web3Service;