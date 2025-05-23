import { ethers } from 'ethers';
import DisputeResolutionDAOABI from './abis/DisputeResolutionDAO.json';
import { getContractAddresses } from '@/lib/blockchain/providers';

export interface Dispute {
  submitter: string;
  disputeType: string;
  description: string;
  transactionHash: string;
  intentId: string;
  timestamp: number;
  votesInFavor: number;
  votesAgainst: number;
  resolved: boolean;
  approved: boolean;
  evidenceURIs: string[];
}

export class DisputeResolutionService {
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

    const disputeResolutionAddress = contractAddresses.disputeResolutionDAO;
    if (!disputeResolutionAddress || disputeResolutionAddress === '0x0000000000000000000000000000000000000000') {
      throw new Error(`Dispute resolution DAO not deployed on chain ID ${this.chainId}`);
    }

    this.contract = new ethers.Contract(
      disputeResolutionAddress,
      DisputeResolutionDAOABI,
      this.signer
    );
  }

  async submitDispute(
    disputeType: string,
    description: string,
    transactionHash: string,
    intentId: string
  ): Promise<{ disputeId: string; tx: ethers.TransactionResponse }> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    // Convert transaction hash to bytes32 if it's a hex string
    const txHashBytes32 = ethers.keccak256(ethers.toUtf8Bytes(transactionHash));
    
    const tx = await this.contract.submitDispute(disputeType, description, txHashBytes32, intentId);
    const receipt = await tx.wait();
    
    // Find the DisputeCreated event to get the disputeId
    const event = receipt.logs
      .map((log: any) => {
        try {
          return this.contract?.interface.parseLog(log);
        } catch (e) {
          return null;
        }
      })
      .find((event: any) => event && event.name === 'DisputeCreated');

    if (!event) {
      throw new Error('Dispute created but could not find disputeId');
    }

    return {
      disputeId: event.args.disputeId,
      tx
    };
  }

  async submitEvidence(disputeId: string, evidenceURI: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.submitEvidence(disputeId, evidenceURI);
  }

  async castVote(disputeId: string, inFavor: boolean): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.castVote(disputeId, inFavor);
  }

  async executeResolution(disputeId: string): Promise<ethers.TransactionResponse> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.executeResolution(disputeId);
  }

  async getDisputeDetails(disputeId: string): Promise<Dispute> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    
    const dispute = await this.contract.getDisputeDetails(disputeId);
    
    return {
      submitter: dispute.submitter,
      disputeType: dispute.disputeType,
      description: dispute.description,
      transactionHash: dispute.transactionHash,
      intentId: dispute.intentId,
      timestamp: Number(dispute.timestamp) * 1000, // Convert to milliseconds
      votesInFavor: Number(dispute.votesInFavor),
      votesAgainst: Number(dispute.votesAgainst),
      resolved: dispute.resolved,
      approved: dispute.approved,
      evidenceURIs: dispute.evidenceURIs
    };
  }

  async getDisputes(): Promise<string[]> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.getDisputes();
  }

  async getDisputesByUser(userAddress: string): Promise<string[]> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.getDisputesByUser(userAddress);
  }

  async hasVoted(disputeId: string, voterAddress: string): Promise<boolean> {
    if (!this.contract) {
      throw new Error('Contract not initialized');
    }
    return this.contract.hasVoted(disputeId, voterAddress);
  }
}