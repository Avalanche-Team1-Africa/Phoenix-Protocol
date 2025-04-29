import React from "react";
import { Button } from "@/components/ui/button";
import { createIntentDescription } from "@/lib/utils/blockchain";

interface IntentModalProps {
  isOpen: boolean;
  onClose: () => void;
  onConfirm: () => void;
  onReject: () => void;
  txType: string;
  payload: Record<string, any>;
}

export function IntentModal({
  isOpen,
  onClose,
  onConfirm,
  onReject,
  txType,
  payload,
}: IntentModalProps) {
  if (!isOpen) return null;

  const intentDescription = createIntentDescription(txType, payload);
  
  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div className="bg-background rounded-lg shadow-lg max-w-md w-full p-6 m-4">
        <div className="mb-4">
          <h3 className="text-lg font-bold">Confirm Your Intent</h3>
          <p className="text-sm text-muted-foreground">
            Please review and confirm the following transaction intent:
          </p>
        </div>
        
        <div className="my-6 p-4 bg-muted rounded-lg">
          <p className="text-lg font-medium text-center">
            {intentDescription}
          </p>
        </div>
        
        <div className="space-y-2 mb-6">
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Transaction Type:</span>
            <span className="font-medium capitalize">{txType}</span>
          </div>
          
          {txType === "swap" && (
            <>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">From Token:</span>
                <span className="font-medium">{payload.tokenInSymbol}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">To Token:</span>
                <span className="font-medium">{payload.tokenOutSymbol}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Amount:</span>
                <span className="font-medium">{payload.amountIn} {payload.tokenInSymbol}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Max Slippage:</span>
                <span className="font-medium">{payload.slippage}%</span>
              </div>
            </>
          )}
          
          {txType === "transfer" && (
            <>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Token:</span>
                <span className="font-medium">{payload.tokenSymbol}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Amount:</span>
                <span className="font-medium">{payload.amount} {payload.tokenSymbol}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Recipient:</span>
                <span className="font-medium font-mono">{payload.recipient}</span>
              </div>
            </>
          )}
          
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Valid Until:</span>
            <span className="font-medium">5 minutes from now</span>
          </div>
        </div>
        
        <div className="flex flex-col gap-2">
          <Button variant="gradient" onClick={onConfirm}>
            Confirm Intent
          </Button>
          <Button variant="outline" onClick={onReject}>
            Reject Intent
          </Button>
        </div>
        
        <div className="mt-4 text-xs text-center text-muted-foreground">
          By confirming, you're creating a signed intent that will be stored on-chain.
          This allows Phoenix Protocol to verify the actual transaction against your intent.
        </div>
      </div>
    </div>
  );
}