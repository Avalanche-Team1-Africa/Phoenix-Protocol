"use client";

import React, { createContext, useContext, useState, useEffect } from "react";
import { useWallet } from "@/context/wallet-context";
import { RecoveryModuleService, RecoveryRequest as ContractRecoveryRequest } from "@/contracts/RecoveryModuleService";
import { IntentRegistryService } from "@/contracts/IntentRegistryService";
import { ethers } from "ethers";

// Define types
export type Guardian = {
  address: string;
  name?: string;
  email?: string;
  dateAdded: number;
};

export type RecoveryRequest = {
  id: string;
  oldWalletAddress: string;
  newWalletAddress: string;
  reason?: string;
  details?: string;
  timestamp: number;
  status: "pending" | "approved" | "rejected" | "executed" | "disputed" | "cancelled";
  approvals: string[];
  approvalCount: number;
  cooldownEnds: number;
  executedAt?: number;
};

interface GuardianContextType {
  // Guardian management
  guardians: Guardian[];
  guardianThreshold: number;
  addGuardian: (address: string, name?: string, email?: string) => Promise<boolean>;
  removeGuardian: (address: string) => Promise<boolean>;
  setGuardianThreshold: (threshold: number) => Promise<boolean>;
  isGuardian: (address: string) => Promise<boolean>;
  
  // Recovery requests
  recoveryRequests: RecoveryRequest[];
  pendingGuardianRequests: RecoveryRequest[];
  initiateRecovery: (newWalletAddress: string, reason?: string, details?: string) => Promise<string>;
  cancelRecovery: (recoveryId: string) => Promise<boolean>;
  approveRecovery: (recoveryId: string) => Promise<boolean>;
  executeRecovery: (recoveryId: string) => Promise<boolean>;
  
  // Intent verification
  verifyTransactionIntent: (transactionHash: string, intentId: string) => Promise<{
    matches: boolean;
    discrepancies?: string[];
  }>;
  
  // Loading states
  isLoading: boolean;
  error: string | null;
}

const GuardianContext = createContext<GuardianContextType | undefined>(undefined);

export function useGuardian() {
  const context = useContext(GuardianContext);
  if (context === undefined) {
    throw new Error("useGuardian must be used within a GuardianProvider");
  }
  return context;
}

export function GuardianProvider({ children }: { children: React.ReactNode }) {
  const { wallet, getSigner } = useWallet();
  
  // State for guardians
  const [guardians, setGuardians] = useState<Guardian[]>([]);
  const [guardianThreshold, setGuardianThreshold] = useState(0);
  
  // State for recovery requests
  const [recoveryRequests, setRecoveryRequests] = useState<RecoveryRequest[]>([]);
  const [pendingGuardianRequests, setPendingGuardianRequests] = useState<RecoveryRequest[]>([]);
  
  // Loading states
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  // Services
  const [recoveryService, setRecoveryService] = useState<RecoveryModuleService | null>(null);
  const [intentService, setIntentService] = useState<IntentRegistryService | null>(null);
  
  // Initialize services when wallet connects
  useEffect(() => {
    if (wallet.connected && wallet.chainId) {
      const signer = getSigner();
      if (signer) {
        setRecoveryService(new RecoveryModuleService(signer, wallet.chainId));
        setIntentService(new IntentRegistryService(signer, wallet.chainId));
      }
    } else {
      setRecoveryService(null);
      setIntentService(null);
    }
  }, [wallet.connected, wallet.chainId, getSigner]);
  
  // Load guardian data when wallet connects and services are initialized
  useEffect(() => {
    if (wallet.connected && recoveryService) {
      loadGuardianData();
      loadRecoveryRequests();
    }
  }, [wallet.connected, wallet.address, recoveryService]);
  
  // Load guardian data
  const loadGuardianData = async () => {
    if (!recoveryService || !wallet.address) return;
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Get guardians from contract
      const guardianAddresses = await recoveryService.getGuardians(wallet.address);
      
      // Get guardian threshold
      const threshold = await recoveryService.getGuardiansThreshold(wallet.address);
      setGuardianThreshold(threshold);
      
      // Convert addresses to Guardian objects
      // In a real app, you might want to fetch additional metadata from a database
      const guardianObjects: Guardian[] = guardianAddresses.map(address => ({
        address,
        name: `Guardian ${address.substring(0, 6)}...${address.substring(38)}`,
        dateAdded: Date.now() // We don't have this info from the contract
      }));
      
      setGuardians(guardianObjects);
      setIsLoading(false);
    } catch (error: any) {
      console.error("Error loading guardian data:", error);
      setError(error.message || "Failed to load guardian data");
      setIsLoading(false);
    }
  };
  
  // Load recovery requests
  const loadRecoveryRequests = async () => {
    if (!recoveryService || !wallet.address) return;
    
    setIsLoading(true);
    
    try {
      // Get recovery request IDs
      const recoveryIds = await recoveryService.getRecoveryRequests(wallet.address);
      
      // Get details for each recovery request
      const requests: RecoveryRequest[] = [];
      const pendingRequests: RecoveryRequest[] = [];
      
      for (const id of recoveryIds) {
        const request = await recoveryService.getRecoveryRequest(id);
        
        // Convert contract request to our format
        const formattedRequest: RecoveryRequest = {
          id,
          oldWalletAddress: request.oldWallet,
          newWalletAddress: request.newWallet,
          timestamp: request.timestamp,
          approvalCount: request.approvalCount,
          approvals: [], // We don't have this info from the contract directly
          cooldownEnds: request.cooldownEnds,
          status: getStatusFromContractRequest(request)
        };
        
        requests.push(formattedRequest);
        
        // Check if this is a pending request where the current user is a guardian
        const isGuardianForRequest = await recoveryService.isGuardian(request.oldWallet, wallet.address);
        const hasVoted = await recoveryService.hasVoted(id, wallet.address);
        
        if (isGuardianForRequest && !hasVoted && !request.executed && !request.cancelled) {
          pendingRequests.push(formattedRequest);
        }
      }
      
      setRecoveryRequests(requests);
      setPendingGuardianRequests(pendingRequests);
      setIsLoading(false);
    } catch (error: any) {
      console.error("Error loading recovery requests:", error);
      setError(error.message || "Failed to load recovery requests");
      setIsLoading(false);
    }
  };
  
  // Helper function to determine status from contract request
  const getStatusFromContractRequest = (request: ContractRecoveryRequest): RecoveryRequest["status"] => {
    if (request.executed) return "executed";
    if (request.cancelled) return "cancelled";
    
    // Check if cooldown has ended and approvals meet threshold
    const now = Date.now();
    if (request.cooldownEnds < now && request.approvalCount >= guardianThreshold) {
      return "approved";
    }
    
    return "pending";
  };
  
  // Add guardian
  const addGuardian = async (address: string, name?: string, email?: string): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to add guardian
      const tx = await recoveryService.addGuardian(address);
      await tx.wait();
      
      // Store additional metadata in local storage
      const guardianMetadata = JSON.parse(localStorage.getItem("phoenixGuardianMetadata") || "{}");
      guardianMetadata[address] = { name, email };
      localStorage.setItem("phoenixGuardianMetadata", JSON.stringify(guardianMetadata));
      
      // Reload guardian data
      await loadGuardianData();
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error adding guardian:", error);
      setError(error.message || "Failed to add guardian");
      setIsLoading(false);
      return false;
    }
  };
  
  // Remove guardian
  const removeGuardian = async (address: string): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to remove guardian
      const tx = await recoveryService.removeGuardian(address);
      await tx.wait();
      
      // Remove metadata from local storage
      const guardianMetadata = JSON.parse(localStorage.getItem("phoenixGuardianMetadata") || "{}");
      delete guardianMetadata[address];
      localStorage.setItem("phoenixGuardianMetadata", JSON.stringify(guardianMetadata));
      
      // Reload guardian data
      await loadGuardianData();
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error removing guardian:", error);
      setError(error.message || "Failed to remove guardian");
      setIsLoading(false);
      return false;
    }
  };
  
  // Set guardian threshold
  const setGuardianThresholdValue = async (threshold: number): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Validate threshold
      if (threshold < 1 || threshold > guardians.length) {
        setError("Invalid threshold value");
        setIsLoading(false);
        return false;
      }
      
      // Call contract to set threshold
      const tx = await recoveryService.setGuardianThreshold(threshold);
      await tx.wait();
      
      // Update local state
      setGuardianThreshold(threshold);
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error setting guardian threshold:", error);
      setError(error.message || "Failed to set guardian threshold");
      setIsLoading(false);
      return false;
    }
  };
  
  // Check if address is a guardian
  const isGuardian = async (address: string): Promise<boolean> => {
    if (!recoveryService || !wallet.address) return false;
    
    try {
      return await recoveryService.isGuardian(wallet.address, address);
    } catch (error) {
      console.error("Error checking if address is guardian:", error);
      return false;
    }
  };
  
  // Initiate recovery
  const initiateRecovery = async (newWalletAddress: string, reason?: string, details?: string): Promise<string> => {
    if (!recoveryService) {
      throw new Error("Wallet not connected");
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to initiate recovery
      const { recoveryId, tx } = await recoveryService.initiateRecovery(newWalletAddress);
      await tx.wait();
      
      // Store additional metadata in local storage
      const recoveryMetadata = JSON.parse(localStorage.getItem("phoenixRecoveryMetadata") || "{}");
      recoveryMetadata[recoveryId] = { reason, details };
      localStorage.setItem("phoenixRecoveryMetadata", JSON.stringify(recoveryMetadata));
      
      // Reload recovery requests
      await loadRecoveryRequests();
      
      setIsLoading(false);
      return recoveryId;
    } catch (error: any) {
      console.error("Error initiating recovery:", error);
      setError(error.message || "Failed to initiate recovery");
      setIsLoading(false);
      throw error;
    }
  };
  
  // Cancel recovery
  const cancelRecovery = async (recoveryId: string): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to cancel recovery
      const tx = await recoveryService.cancelRecovery(recoveryId);
      await tx.wait();
      
      // Reload recovery requests
      await loadRecoveryRequests();
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error cancelling recovery:", error);
      setError(error.message || "Failed to cancel recovery");
      setIsLoading(false);
      return false;
    }
  };
  
  // Approve recovery as guardian
  const approveRecovery = async (recoveryId: string): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to approve recovery
      const tx = await recoveryService.approveRecovery(recoveryId);
      await tx.wait();
      
      // Reload recovery requests
      await loadRecoveryRequests();
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error approving recovery:", error);
      setError(error.message || "Failed to approve recovery");
      setIsLoading(false);
      return false;
    }
  };
  
  // Execute recovery
  const executeRecovery = async (recoveryId: string): Promise<boolean> => {
    if (!recoveryService) {
      setError("Wallet not connected");
      return false;
    }
    
    setIsLoading(true);
    setError(null);
    
    try {
      // Call contract to execute recovery
      const tx = await recoveryService.executeRecovery(recoveryId);
      await tx.wait();
      
      // Reload recovery requests
      await loadRecoveryRequests();
      
      setIsLoading(false);
      return true;
    } catch (error: any) {
      console.error("Error executing recovery:", error);
      setError(error.message || "Failed to execute recovery");
      setIsLoading(false);
      return false;
    }
  };
  
  // Verify transaction intent
  const verifyTransactionIntent = async (transactionHash: string, intentId: string): Promise<{
    matches: boolean;
    discrepancies?: string[];
  }> => {
    if (!intentService) {
      throw new Error("Wallet not connected");
    }
    
    try {
      const result = await intentService.verifyTransaction(intentId, transactionHash);
      return {
        matches: result.matches,
        discrepancies: result.discrepancies
      };
    } catch (error: any) {
      console.error("Error verifying transaction intent:", error);
      throw new Error(error.message || "Failed to verify transaction intent");
    }
  };
  
  return (
    <GuardianContext.Provider
      value={{
        // Guardian management
        guardians,
        guardianThreshold,
        addGuardian,
        removeGuardian,
        setGuardianThreshold: setGuardianThresholdValue,
        isGuardian,
        
        // Recovery requests
        recoveryRequests,
        pendingGuardianRequests,
        initiateRecovery,
        cancelRecovery,
        approveRecovery,
        executeRecovery,
        
        // Intent verification
        verifyTransactionIntent,
        
        // Loading states
        isLoading,
        error
      }}
    >
      {children}
    </GuardianContext.Provider>
  );
}