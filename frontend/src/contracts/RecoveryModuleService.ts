import { ethers } from 'ethers';
import RecoveryModuleABI from './abis/RecoveryModule.json';
import { getContractAddresses } from '@/lib/blockchain/providers';

export interface RecoveryRequest {
  oldWallet: string;
  newWallet: string;
  approvalCount: number;
  timestamp: number;
  cooldownEnds: number;
  executed: boolean;
  cancelled: boolean;
}

export class RecoveryModuleService {
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

    const recoveryModuleAddress = contractAddresses.recoveryModule;
    if (!recoveryModuleAddress || recoveryModuleAddress === '0x0000000000000000000000000000000000000000') {
      throw new Error(`Recovery module not deployed on chain ID ${this.chainId}`);
    }

    this.contract = new ethers.Contract(
      recoveryModuleAddress,
      RecoveryModuleABI,
      this.signer
    );
  }

  async addGuardian(guardian: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.addGuardian(guardian);
  }

  async removeGuardian(guardian: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.removeGuardian(guardian);
  }

  async setGuardianThreshold(threshold: number): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.setGuardianThreshold(threshold);
  }

  async initiateRecovery(newWalletAddress: string): Promise<{ recoveryId: string; tx: ethers.TransactionResponse }> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    const tx = await this.contract.initiateRecovery(newWalletAddress);
    const receipt = await tx.wait();
    
    // Find the RecoveryInitiated event to get the recoveryId
    const event = receipt.logs
      .map((log: any) => {
        try {
          return this.contract?.interface.parseLog(log);
        } catch (e) {
          return null;
        }
      })
      .find((event: any) => event && event.name === 'RecoveryInitiated');

    if (!event) {
      throw new Error('Recovery initiated but could not find recoveryId');
    }

    return {
      recoveryId: event.args.recoveryId,
      tx
    };
  }

  async approveRecovery(recoveryId: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.approveRecovery(recoveryId);
  }

  async cancelRecovery(recoveryId: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.cancelRecovery(recoveryId);
  }

  async executeRecovery(recoveryId: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.executeRecovery(recoveryId);
  }

  async getGuardians(walletAddress: string): Promise<string[]> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.getGuardians(walletAddress);
  }

  async getGuardiansThreshold(walletAddress: string): Promise<number> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    const threshold = await this.contract.getGuardiansThreshold(walletAddress);
    return Number(threshold);
  }

  async getRecoveryRequest(recoveryId: string): Promise<RecoveryRequest> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    const request = await this.contract.getRecoveryRequest(recoveryId);
    
    return {
      oldWallet: request.oldWallet,
      newWallet: request.newWallet,
      approvalCount: Number(request.approvalCount),
      timestamp: Number(request.timestamp) * 1000, // Convert to milliseconds
      cooldownEnds: Number(request.cooldownEnds) * 1000, // Convert to milliseconds
      executed: request.executed,
      cancelled: request.cancelled
    };
  }

  async getRecoveryRequests(walletAddress: string): Promise<string[]> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.getRecoveryRequests(walletAddress);
  }

  async isGuardian(walletAddress: string, guardianAddress: string): Promise<boolean> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.isGuardian(walletAddress, guardianAddress);
  }

  async hasVoted(recoveryId: string, guardianAddress: string): Promise<boolean> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.hasVoted(recoveryId, guardianAddress);
  }
}