import { useState, useEffect } from "react";
import { useWallet } from "@/context/wallet-context";
import { IntentRegistryService, Intent } from "@/contracts/IntentRegistryService";
import { ethers } from "ethers";

interface TransactionIntent {
  id: string;
  txType: string;
  payload: Record<string, any>;
  timestamp: number;
  status: "pending" | "confirmed" | "executed" | "verified" | "mismatch";
  txHash?: string;
  verification?: {
    success: boolean;
    discrepancies?: string[];
  };
}

export function useTransactionIntent() {
  const { wallet, getSigner } = useWallet();
  const [intents, setIntents] = useState<TransactionIntent[]>([]);
  const [currentIntent, setCurrentIntent] = useState<TransactionIntent | null>(null);
  const [isIntentModalOpen, setIsIntentModalOpen] = useState(false);
  const [intentService, setIntentService] = useState<IntentRegistryService | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Initialize intent service when wallet connects
  useEffect(() => {
    if (wallet.connected && wallet.chainId) {
      const signer = getSigner();
      if (signer) {
        setIntentService(new IntentRegistryService(signer, wallet.chainId));
      }
    } else {
      setIntentService(null);
    }
  }, [wallet.connected, wallet.chainId, getSigner]);

  // Load intents when service is initialized
  useEffect(() => {
    if (intentService && wallet.address) {
      loadIntents();
    }
  }, [intentService, wallet.address]);

  // Load intents from blockchain
  const loadIntents = async () => {
    if (!intentService || !wallet.address) return;

    setIsLoading(true);
    setError(null);

    try {
      // Get intent IDs for the user
      const intentIds = await intentService.getIntentsByUser(wallet.address);
      
      // Get details for each intent
      const intentDetails: TransactionIntent[] = [];
      
      for (const id of intentIds) {
        const intent = await intentService.getIntentDetails(id);
        
        // Parse intent data
        let payload = {};
        try {
          payload = JSON.parse(intent.intentData);
        } catch (e) {
          console.error("Failed to parse intent data:", e);
        }
        
        // Convert to our format
        intentDetails.push({
          id,
          txType: intent.intentType,
          payload,
          timestamp: intent.timestamp,
          status: getStatusFromContractIntent(intent),
          txHash: intent.transactionHash !== ethers.ZeroHash ? intent.transactionHash : undefined,
          verification: intent.verified ? {
            success: intent.matches,
            discrepancies: intent.matches ? [] : ["Transaction does not match intent"]
          } : undefined
        });
      }
      
      setIntents(intentDetails);
      setIsLoading(false);
    } catch (error: any) {
      console.error("Error loading intents:", error);
      setError(error.message || "Failed to load intents");
      setIsLoading(false);
    }
  };

  // Helper to determine status from contract intent
  const getStatusFromContractIntent = (intent: Intent): TransactionIntent["status"] => {
    if (intent.verified) {
      return intent.matches ? "verified" : "mismatch";
    }
    if (intent.executed) {
      return "executed";
    }
    return "confirmed";
  };

  // Create a new intent
  const createIntent = async (txType: string, payload: Record<string, any>) => {
    if (!intentService) {
      throw new Error("Wallet not connected");
    }

    setIsLoading(true);
    setError(null);

    try {
      // Create a pending intent in local state first
      const tempId = `intent-${Date.now()}-${Math.floor(Math.random() * 1000)}`;
      const pendingIntent: TransactionIntent = {
        id: tempId,
        txType,
        payload,
        timestamp: Date.now(),
        status: "pending",
      };

      setCurrentIntent(pendingIntent);
      setIsIntentModalOpen(true);
      
      setIsLoading(false);
      return tempId;
    } catch (error: any) {
      console.error("Error creating intent:", error);
      setError(error.message || "Failed to create intent");
      setIsLoading(false);
      throw error;
    }
  };

  // Confirm the intent
  const confirmIntent = async () => {
    if (!currentIntent || !intentService) {
      throw new Error("No current intent or wallet not connected");
    }

    setIsLoading(true);
    setError(null);

    try {
      // Create intent on blockchain
      const { intentId, tx } = await intentService.createIntent(
        currentIntent.txType,
        currentIntent.payload
      );
      
      // Wait for transaction to be mined
      await tx.wait();
      
      // Get intent details from blockchain
      const intentDetails = await intentService.getIntentDetails(intentId);
      
      // Create confirmed intent
      const confirmedIntent: TransactionIntent = {
        id: intentId,
        txType: currentIntent.txType,
        payload: currentIntent.payload,
        timestamp: intentDetails.timestamp,
        status: "confirmed",
      };
      
      // Update state
      setIntents((prev) => [...prev, confirmedIntent]);
      setCurrentIntent(null);
      setIsIntentModalOpen(false);
      setIsLoading(false);
      
      return confirmedIntent;
    } catch (error: any) {
      console.error("Error confirming intent:", error);
      setError(error.message || "Failed to confirm intent");
      setIsLoading(false);
      throw error;
    }
  };

  // Reject the intent
  const rejectIntent = () => {
    // This just removes the intent from local state
    // since it hasn't been submitted to the blockchain yet
    setCurrentIntent(null);
    setIsIntentModalOpen(false);
    return true;
  };

  // Execute a transaction and verify against intent
  const executeTransaction = async (intentId: string, txReceipt: any) => {
    if (!intentService) {
      throw new Error("Wallet not connected");
    }

    setIsLoading(true);
    setError(null);

    try {
      // Find the intent
      const intent = intents.find((i) => i.id === intentId);
      
      if (!intent) {
        throw new Error("Intent not found");
      }
      
      // Execute transaction on blockchain
      const tx = await intentService.executeTransaction(intentId, txReceipt.transactionHash);
      await tx.wait();
      
      // Verify transaction against intent
      const verification = await intentService.verifyTransaction(intentId, txReceipt.transactionHash);
      
      // Update intent in state
      const updatedIntent: TransactionIntent = {
        ...intent,
        status: verification.matches ? "verified" : "mismatch",
        txHash: txReceipt.transactionHash,
        verification: {
          success: verification.matches,
          discrepancies: verification.discrepancies
        }
      };
      
      setIntents((prev) =>
        prev.map((i) => (i.id === intentId ? updatedIntent : i))
      );
      
      setIsLoading(false);
      
      return {
        success: verification.matches,
        intent: updatedIntent,
        discrepancies: verification.discrepancies
      };
    } catch (error: any) {
      console.error("Error executing transaction:", error);
      setError(error.message || "Failed to execute transaction");
      setIsLoading(false);
      throw error;
    }
  };

  // Get all intents
  const getAllIntents = () => {
    return intents;
  };

  // Get intent by ID
  const getIntentById = (intentId: string) => {
    return intents.find((i) => i.id === intentId) || null;
  };

  // Verify a transaction against an intent
  const verifyTransaction = async (intentId: string, txHash: string) => {
    if (!intentService) {
      throw new Error("Wallet not connected");
    }

    try {
      return await intentService.verifyTransaction(intentId, txHash);
    } catch (error: any) {
      console.error("Error verifying transaction:", error);
      throw new Error(error.message || "Failed to verify transaction");
    }
  };

  return {
    createIntent,
    confirmIntent,
    rejectIntent,
    executeTransaction,
    getAllIntents,
    getIntentById,
    verifyTransaction,
    currentIntent,
    isIntentModalOpen,
    setIsIntentModalOpen,
    isLoading,
    error
  };
}