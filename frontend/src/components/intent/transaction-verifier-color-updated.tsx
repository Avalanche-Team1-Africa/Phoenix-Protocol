"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Alert } from "@/components/ui/alert";
import { CheckCircle, XCircle, AlertTriangle, Search, ArrowRight } from "lucide-react";
import { useGuardian } from "@/context/guardian-provider";
import { useTransactionIntent } from "@/hooks/use-transaction-intent";
import Link from "next/link";

export function TransactionVerifier() {
  const { verifyTransactionIntent } = useGuardian();
  const { getIntentById, verifyTransaction, isLoading } = useTransactionIntent();
  
  const [transactionHash, setTransactionHash] = useState("");
  const [intentId, setIntentId] = useState("");
  const [verificationResult, setVerificationResult] = useState<{
    matches: boolean;
    discrepancies?: string[];
  } | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [showResult, setShowResult] = useState(false);
  const [intentDetails, setIntentDetails] = useState<any>(null);
  const [transactionDetails, setTransactionDetails] = useState<any>(null);

  const handleVerify = async () => {
    if (!transactionHash || !intentId) {
      setError("Please enter both transaction hash and intent ID");
      return;
    }

    setError(null);
    setShowResult(false);
    setIntentDetails(null);
    setTransactionDetails(null);

    try {
      // Get intent details
      const intent = getIntentById(intentId);
      if (intent) {
        setIntentDetails(intent);
      }
      
      // Set mock transaction details (in a real app, we'd fetch this from the blockchain)
      setTransactionDetails({
        hash: transactionHash,
        timestamp: Date.now() - (60 * 60 * 1000) // 1 hour ago
      });
      
      // Verify transaction against intent
      const result = await verifyTransaction(intentId, transactionHash);
      setVerificationResult(result);
      setShowResult(true);
    } catch (error: any) {
      console.error("Verification error:", error);
      setError(error.message || "Verification failed");
    }
  };

  // Format intent description
  const getIntentDescription = (intent: any) => {
    if (!intent) return "";
    
    switch (intent.txType) {
      case "swap":
        return `Swap ${intent.payload.amountIn} ${intent.payload.tokenInSymbol} for ${intent.payload.tokenOutSymbol}`;
      case "transfer":
        return `Send ${intent.payload.amount} ${intent.payload.tokenSymbol} to ${intent.payload.recipient.substring(0, 6)}...${intent.payload.recipient.substring(38)}`;
      case "stake":
        return `Stake ${intent.payload.amount} ${intent.payload.tokenSymbol} for ${intent.payload.duration} days`;
      case "mint":
        return `Mint ${intent.payload.quantity} ${intent.payload.nftName} for ${intent.payload.price} ${intent.payload.currencySymbol}`;
      default:
        return `${intent.txType} Transaction`;
    }
  };

  // Format time
  const formatTime = (timestamp: number) => {
    const now = Date.now();
    const diff = now - timestamp;
    
    if (diff < 60 * 1000) return "Just now";
    if (diff < 60 * 60 * 1000) return `${Math.floor(diff / (60 * 1000))} minutes ago`;
    if (diff < 24 * 60 * 60 * 1000) return `${Math.floor(diff / (60 * 60 * 1000))} hours ago`;
    return `${Math.floor(diff / (24 * 60 * 60 * 1000))} days ago`;
  };

  return (
    <Card className="border-phoenix-light-border dark:border-phoenix-dark-border">
      <CardHeader>
        <CardTitle className="text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Transaction Intent Verifier</CardTitle>
        <CardDescription className="text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
          Verify that a transaction matches your original intent
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        <Alert className="alert-info">
          <p className="text-sm text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
            This tool compares an executed transaction against your original intent to detect any discrepancies.
            If a mismatch is found, you can initiate a dispute process.
          </p>
        </Alert>

        {error && (
          <Alert className="alert-error">
            {error}
          </Alert>
        )}

        <div className="space-y-4">
          <div className="space-y-2">
            <label htmlFor="transactionHash" className="text-sm font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
              Transaction Hash
            </label>
            <Input
              id="transactionHash"
              type="text"
              placeholder="0x..."
              value={transactionHash}
              onChange={(e) => setTransactionHash(e.target.value)}
              className="border-phoenix-light-border dark:border-phoenix-dark-border focus:border-phoenix-light-border-focus dark:focus:border-phoenix-dark-border-focus"
            />
          </div>

          <div className="space-y-2">
            <label htmlFor="intentId" className="text-sm font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
              Intent ID
            </label>
            <Input
              id="intentId"
              type="text"
              placeholder="0x..."
              value={intentId}
              onChange={(e) => setIntentId(e.target.value)}
              className="border-phoenix-light-border dark:border-phoenix-dark-border focus:border-phoenix-light-border-focus dark:focus:border-phoenix-dark-border-focus"
            />
          </div>

          <Button
            className="w-full bg-phoenix-light-primary hover:bg-phoenix-light-primary-hover dark:bg-phoenix-dark-primary dark:hover:bg-phoenix-dark-primary-hover text-white"
            onClick={handleVerify}
            disabled={isLoading || !transactionHash || !intentId}
          >
            {isLoading ? (
              <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
            ) : (
              <Search className="h-4 w-4 mr-2" />
            )}
            Verify Transaction
          </Button>
        </div>

        {showResult && verificationResult && (
          <div className="mt-6">
            <h3 className="text-lg font-medium mb-2 text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Verification Result</h3>
            <div
              className={`p-4 rounded-md ${
                verificationResult.matches
                  ? "bg-phoenix-light-success-light dark:bg-phoenix-dark-success-light"
                  : "bg-phoenix-light-error-light dark:bg-phoenix-dark-error-light"
              }`}
            >
              <div className="flex items-center gap-2 mb-2">
                {verificationResult.matches ? (
                  <>
                    <CheckCircle className="h-5 w-5 text-phoenix-light-success dark:text-phoenix-dark-success" />
                    <p className="text-base font-medium text-phoenix-light-success dark:text-phoenix-dark-success">
                      Transaction matches intent
                    </p>
                  </>
                ) : (
                  <>
                    <AlertTriangle className="h-5 w-5 text-phoenix-light-error dark:text-phoenix-dark-error" />
                    <p className="text-base font-medium text-phoenix-light-error dark:text-phoenix-dark-error">
                      Transaction does not match intent
                    </p>
                  </>
                )}
              </div>

              {!verificationResult.matches && verificationResult.discrepancies && verificationResult.discrepancies.length > 0 && (
                <div className="space-y-2 mt-4">
                  <p className="text-sm font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Discrepancies Found:</p>
                  <ul className="list-disc pl-5 text-sm space-y-1">
                    {verificationResult.discrepancies.map((discrepancy, index) => (
                      <li key={index} className="text-phoenix-light-error dark:text-phoenix-dark-error">
                        {discrepancy}
                      </li>
                    ))}
                  </ul>
                </div>
              )}

              {intentDetails && (
                <div className="mt-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div className="space-y-1">
                      <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Intent</p>
                      <div className="p-2 bg-phoenix-light-background dark:bg-phoenix-dark-background rounded-md">
                        <p className="text-xs text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{getIntentDescription(intentDetails)}</p>
                        <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
                          Created: {formatTime(intentDetails.timestamp)}
                        </p>
                      </div>
                    </div>
                    <div className="space-y-1">
                      <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Transaction</p>
                      <div className="p-2 bg-phoenix-light-background dark:bg-phoenix-dark-background rounded-md">
                        <p className="text-xs text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Transaction {transactionHash.substring(0, 6)}...{transactionHash.substring(transactionHash.length - 4)}</p>
                        <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
                          Executed: {transactionDetails ? formatTime(transactionDetails.timestamp) : "Unknown"}
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {!verificationResult.matches && (
                <div className="mt-6">
                  <Link href="/disputes">
                    <Button
                      className="w-full bg-phoenix-gradient dark:bg-phoenix-gradient-dark text-white"
                    >
                      <ArrowRight className="h-4 w-4 mr-2" />
                      Submit Dispute
                    </Button>
                  </Link>
                </div>
              )}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}