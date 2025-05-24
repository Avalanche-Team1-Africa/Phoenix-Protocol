"use client";

import React, { useState, useEffect } from "react";
import { X, AlertTriangle, ArrowUpRight } from "lucide-react";
import { TransactionStepper, TransactionStep } from "./transaction-stepper";
import { Button } from "@/components/ui/button";
import { getExplorerUrl } from "@/lib/utils/blockchain";

interface TransactionDetails {
  type: string;
  from: string;
  to: string;
  value: string;
  gasEstimate: string;
  data?: string;
  chainId: number;
}

interface TransactionConfirmationModalProps {
  isOpen: boolean;
  onClose: () => void;
  transaction: TransactionDetails;
  onConfirm: () => Promise<{ success: boolean; hash?: string; error?: string }>;
  onComplete?: (success: boolean, hash?: string) => void;
}

export function TransactionConfirmationModal({
  isOpen,
  onClose,
  transaction,
  onConfirm,
  onComplete,
}: TransactionConfirmationModalProps) {
  // Define transaction steps
  const [steps, setSteps] = useState<TransactionStep[]>([
    {
      id: "review",
      title: "Review Transaction",
      description: "Carefully review the transaction details before proceeding.",
      status: "active",
    },
    {
      id: "confirm",
      title: "Confirm in Wallet",
      description: "Approve the transaction in your wallet.",
      status: "pending",
    },
    {
      id: "processing",
      title: "Processing",
      description: "Your transaction is being processed on the blockchain.",
      status: "pending",
    },
    {
      id: "complete",
      title: "Complete",
      description: "Transaction has been successfully processed.",
      status: "pending",
    },
  ]);

  const [currentStepIndex, setCurrentStepIndex] = useState(0);
  const [isProcessing, setIsProcessing] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [txHash, setTxHash] = useState<string | undefined>(undefined);
  const [isVisible, setIsVisible] = useState(false);

  // Handle modal visibility
  useEffect(() => {
    if (isOpen) {
      setIsVisible(true);
    }
  }, [isOpen]);

  // Reset state when modal is closed
  const handleClose = () => {
    onClose();
    setTimeout(() => {
      setCurrentStepIndex(0);
      setIsProcessing(false);
      setError(null);
      setTxHash(undefined);
      setSteps([
        {
          id: "review",
          title: "Review Transaction",
          description: "Carefully review the transaction details before proceeding.",
          status: "active",
        },
        {
          id: "confirm",
          title: "Confirm in Wallet",
          description: "Approve the transaction in your wallet.",
          status: "pending",
        },
        {
          id: "processing",
          title: "Processing",
          description: "Your transaction is being processed on the blockchain.",
          status: "pending",
        },
        {
          id: "complete",
          title: "Complete",
          description: "Transaction has been successfully processed.",
          status: "pending",
        },
      ]);
      setIsVisible(false);
    }, 300);
  };

  // Handle next step
  const handleNext = async () => {
    if (currentStepIndex === 0) {
      // Move to wallet confirmation step
      setSteps((prev) =>
        prev.map((step, idx) =>
          idx === 0
            ? { ...step, status: "completed" }
            : idx === 1
            ? { ...step, status: "active" }
            : step
        )
      );
      setCurrentStepIndex(1);
      setIsProcessing(true);
      setError(null);

      try {
        // Execute the transaction
        const result = await onConfirm();

        if (result.success) {
          // Transaction confirmed in wallet and submitted
          setTxHash(result.hash);
          setSteps((prev) =>
            prev.map((step, idx) =>
              idx === 1
                ? { ...step, status: "completed" }
                : idx === 2
                ? { ...step, status: "active" }
                : step
            )
          );
          setCurrentStepIndex(2);

          // Wait for transaction to be mined (simulated here)
          setTimeout(() => {
            setSteps((prev) =>
              prev.map((step, idx) =>
                idx === 2
                  ? { ...step, status: "completed" }
                  : idx === 3
                  ? { ...step, status: "active" }
                  : step
              )
            );
            setCurrentStepIndex(3);
            setIsProcessing(false);
            if (onComplete) onComplete(true, result.hash);
          }, 2000);
        } else {
          // Error during wallet confirmation
          setSteps((prev) =>
            prev.map((step, idx) =>
              idx === 1 ? { ...step, status: "error" } : step
            )
          );
          setError(result.error || "Transaction failed");
          setIsProcessing(false);
        }
      } catch (err: any) {
        setSteps((prev) =>
          prev.map((step, idx) =>
            idx === 1 ? { ...step, status: "error" } : step
          )
        );
        setError(err.message || "Transaction failed");
        setIsProcessing(false);
      }
    } else if (currentStepIndex === 3) {
      // Complete flow
      handleClose();
    }
  };

  // Handle retry
  const handleRetry = () => {
    if (currentStepIndex === 1) {
      // Retry wallet confirmation
      handleNext();
    }
  };

  // Handle previous step
  const handlePrevious = () => {
    if (currentStepIndex === 1 && !isProcessing) {
      setSteps((prev) =>
        prev.map((step, idx) =>
          idx === 0
            ? { ...step, status: "active" }
            : idx === 1
            ? { ...step, status: "pending" }
            : step
        )
      );
      setCurrentStepIndex(0);
      setError(null);
    }
  };

  if (!isOpen) return null;

  return (
    <div
      className={`fixed inset-0 z-50 flex items-center justify-center bg-black/50 transition-opacity duration-300 ${
        isVisible ? "opacity-100" : "opacity-0"
      }`}
    >
      <div
        className={`w-full max-w-lg rounded-lg bg-background p-6 shadow-lg transition-all duration-300 ${
          isVisible ? "scale-100 opacity-100" : "scale-95 opacity-0"
        }`}
      >
        <div className="mb-6 flex items-center justify-between">
          <h2 className="text-xl font-bold">Confirm Transaction</h2>
          <button
            onClick={handleClose}
            className="text-muted-foreground hover:text-foreground"
            disabled={isProcessing}
          >
            <X className="h-5 w-5" />
          </button>
        </div>

        {/* Transaction Details */}
        {currentStepIndex === 0 && (
          <div className="mb-6 space-y-4 rounded-lg border bg-muted/30 p-4">
            <div className="flex justify-between">
              <span className="text-sm text-muted-foreground">Transaction Type</span>
              <span className="font-medium">{transaction.type}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-sm text-muted-foreground">From</span>
              <span className="font-medium">{transaction.from}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-sm text-muted-foreground">To</span>
              <span className="font-medium">{transaction.to}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-sm text-muted-foreground">Value</span>
              <span className="font-medium">{transaction.value}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-sm text-muted-foreground">Estimated Gas</span>
              <span className="font-medium">{transaction.gasEstimate}</span>
            </div>

            <div className="rounded-md bg-orange-500/10 p-3">
              <div className="flex">
                <AlertTriangle className="h-5 w-5 text-orange-500" />
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-orange-500">
                    Important Notice
                  </h3>
                  <div className="mt-2 text-sm text-orange-500/90">
                    <p>
                      Please verify all transaction details carefully. Once submitted,
                      blockchain transactions cannot be reversed without using Phoenix
                      Protocol's recovery features.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}

        {/* Transaction Hash (when available) */}
        {txHash && currentStepIndex >= 2 && (
          <div className="mb-6 rounded-lg border bg-muted/30 p-4">
            <div className="flex flex-col space-y-2">
              <span className="text-sm text-muted-foreground">Transaction Hash</span>
              <div className="flex items-center justify-between">
                <span className="font-mono text-sm">{txHash}</span>
                <a
                  href={getExplorerUrl(txHash, transaction.chainId)}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="ml-2 inline-flex items-center text-sm text-orange-500 hover:text-orange-600"
                >
                  View
                  <ArrowUpRight className="ml-1 h-3 w-3" />
                </a>
              </div>
            </div>
          </div>
        )}

        {/* Transaction Stepper */}
        <TransactionStepper
          steps={steps}
          currentStepIndex={currentStepIndex}
          onNext={handleNext}
          onPrevious={handlePrevious}
          onRetry={handleRetry}
          isProcessing={isProcessing}
          error={error}
        />
      </div>
    </div>
  );
}