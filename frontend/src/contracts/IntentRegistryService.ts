import { ethers } from 'ethers';
import IntentRegistryABI from './abis/IntentRegistry.json';
import { getContractAddresses } from '@/lib/blockchain/providers';

export interface Intent {
  user: string;
  intentType: string;
  intentData: string;
  timestamp: number;
  transactionHash: string;
  executed: boolean;
  verified: boolean;
  matches: boolean;
}

export interface VerificationResult {
  matches: boolean;
  discrepancies: string[];
}

export class IntentRegistryService {
  private contract: ethers.Contract | null = null;
  private signer: ethers.Signer | null = null;
  private chainId: number;

  constructor(signer: ethers.Signer | null, chainId: number) {
    this.signer = signer;
    this.chainId = chainId;
    this.initContract();
  }

  private initContract() {
    if (!this.signer) return;

    const contractAddresses = getContractAddresses(this.chainId);
    if (!contractAddresses) {
      throw new Error(`No contract addresses found for chain ID ${this.chainId}`);
    }

    const intentRegistryAddress = contractAddresses.intentRegistry;
    if (!intentRegistryAddress || intentRegistryAddress === '0x0000000000000000000000000000000000000000') {
      throw new Error(`Intent registry not deployed on chain ID ${this.chainId}`);
    }

    this.contract = new ethers.Contract(
      intentRegistryAddress,
      IntentRegistryABI,
      this.signer
    );
  }

  async createIntent(intentType: string, intentData: any): Promise<{ intentId: string; tx: ethers.TransactionResponse }> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    // Convert intentData to JSON string
    const intentDataStr = JSON.stringify(intentData);
    
    const tx = await this.contract.createIntent(intentType, intentDataStr);
    const receipt = await tx.wait();
    
    // Find the IntentCreated event to get the intentId
    const event = receipt.logs
      .map((log: any) => {
        try {
          return this.contract?.interface.parseLog(log);
        } catch (e) {
          return null;
        }
      })
      .find((event: any) => event && event.name === 'IntentCreated');

    if (!event) {
      throw new Error('Intent created but could not find intentId');
    }

    return {
      intentId: event.args.intentId,
      tx
    };
  }

  async executeTransaction(intentId: string, transactionHash: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    // Convert transaction hash to bytes32 if it's a hex string
    const txHashBytes32 = ethers.keccak256(ethers.toUtf8Bytes(transactionHash));
    
    return this.contract.executeTransaction(intentId, txHashBytes32);
  }

  async verifyTransaction(intentId: string, transactionHash: string): Promise<VerificationResult> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    // Convert transaction hash to bytes32 if it's a hex string
    const txHashBytes32 = ethers.keccak256(ethers.toUtf8Bytes(transactionHash));
    
    const [matches, discrepancies] = await this.contract.verifyTransaction(intentId, txHashBytes32);
    
    return {
      matches,
      discrepancies
    };
  }

  async getIntentsByUser(userAddress: string): Promise<string[]> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.getIntentsByUser(userAddress);
  }

  async getIntentDetails(intentId: string): Promise<Intent> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    const intent = await this.contract.getIntentDetails(intentId);
    
    // Parse intentData from JSON string
    let parsedIntentData = {};
    try {
      parsedIntentData = JSON.parse(intent.intentData);
    } catch (e) {
      console.error('Failed to parse intent data:', e);
    }
    
    return {
      user: intent.user,
      intentType: intent.intentType,
      intentData: intent.intentData,
      timestamp: Number(intent.timestamp) * 1000, // Convert to milliseconds
      transactionHash: intent.transactionHash,
      executed: intent.executed,
      verified: intent.verified,
      matches: intent.matches
    };
  }
}