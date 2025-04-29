import { useState } from "react";
import { useWallet } from "@/context/wallet-context";
import { ethers } from "ethers";
import { verifyTransactionMatchesIntent } from "@/lib/utils/blockchain";

interface TransactionIntent {
  id: string;
  txType: string;
  payload: Record<string, any>;
  timestamp: number;
  status: "pending" | "confirmed" | "rejected" | "executed" | "disputed" | "matched";
  signature?: string;
}

export function useTransactionIntent() {
  const [intents, setIntents] = useState<TransactionIntent[]>([]);
  const [currentIntent, setCurrentIntent] = useState<TransactionIntent | null>(null);
  const [isIntentModalOpen, setIsIntentModalOpen] = useState(false);
  const { wallet } = useWallet();

  // Create a new intent
  const createIntent = (txType: string, payload: Record<string, any>) => {
    if (!wallet.connected) {
      throw new Error("Wallet not connected");
    }

    const intentId = `intent-${Date.now()}-${Math.floor(Math.random() * 1000)}`;
    const newIntent: TransactionIntent = {
      id: intentId,
      txType,
      payload,
      timestamp: Date.now(),
      status: "pending",
    };

    setCurrentIntent(newIntent);
    setIsIntentModalOpen(true);
    
    return intentId;
  };

  // Confirm the intent
  const confirmIntent = async () => {
    if (!currentIntent || !wallet.connected) {
      return false;
    }

    try {
      // In a real implementation, we would:
      // 1. Create a hash of the intent
      // 2. Sign it with the user's wallet
      // 3. Store it on-chain or in a secure database

      // Mock implementation for demo
      const intentHash = ethers.keccak256(
        ethers.toUtf8Bytes(JSON.stringify(currentIntent))
      );
      
      // In a real app, we would use the wallet provider to sign
      // const signature = await wallet.provider.request({
      //   method: "personal_sign",
      //   params: [intentHash, wallet.address],
      // });

      // Mock signature for demo
      const signature = `0x${Array(130).fill(0).map(() => Math.floor(Math.random() * 16).toString(16)).join('')}`;
      
      const confirmedIntent = {
        ...currentIntent,
        status: "confirmed" as const,
        signature,
      };
      
      setIntents((prev) => [...prev, confirmedIntent]);
      setCurrentIntent(null);
      setIsIntentModalOpen(false);
      
      return confirmedIntent;
    } catch (error) {
      console.error("Error confirming intent:", error);
      return false;
    }
  };

  // Reject the intent
  const rejectIntent = () => {
    if (!currentIntent) {
      return false;
    }

    const rejectedIntent = {
      ...currentIntent,
      status: "rejected" as const,
    };
    
    setIntents((prev) => [...prev, rejectedIntent]);
    setCurrentIntent(null);
    setIsIntentModalOpen(false);
    
    return true;
  };

  // Execute a transaction and verify against intent
  const executeTransaction = async (intentId: string, txReceipt: any) => {
    const intent = intents.find((i) => i.id === intentId);
    
    if (!intent || intent.status !== "confirmed") {
      throw new Error("No confirmed intent found with this ID");
    }
    
    // Verify the transaction against the intent
    const verification = verifyTransactionMatchesIntent(intentId, txReceipt);
    
    if (verification.matches) {
      // Update intent status to matched
      setIntents((prev) =>
        prev.map((i) =>
          i.id === intentId ? { ...i, status: "matched" as const } : i
        )
      );
      
      return { success: true, intent };
    } else {
      // Update intent status to disputed
      setIntents((prev) =>
        prev.map((i) =>
          i.id === intentId ? { ...i, status: "disputed" as const } : i
        )
      );
      
      return { 
        success: false, 
        intent,
        discrepancies: verification.discrepancies 
      };
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

  return {
    createIntent,
    confirmIntent,
    rejectIntent,
    executeTransaction,
    getAllIntents,
    getIntentById,
    currentIntent,
    isIntentModalOpen,
    setIsIntentModalOpen,
  };
}