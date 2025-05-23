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
    <Card>
      <CardHeader>
        <CardTitle>Transaction Intent Verifier</CardTitle>
        <CardDescription>
          Verify that a transaction matches your original intent
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        <Alert className="bg-blue-500/10 border-blue-500/20">
          <p className="text-sm">
            This tool compares an executed transaction against your original intent to detect any discrepancies.
            If a mismatch is found, you can initiate a dispute process.
          </p>
        </Alert>

        {error && (
          <Alert className="bg-red-500/10 border-red-500/20 text-red-500">
            {error}
          </Alert>
        )}

        <div className="space-y-4">
          <div className="space-y-2">
            <label htmlFor="transactionHash" className="text-sm font-medium">
              Transaction Hash
            </label>
            <Input
              id="transactionHash"
              type="text"
              placeholder="0x..."
              value={transactionHash}
              onChange={(e) => setTransactionHash(e.target.value)}
            />
          </div>

          <div className="space-y-2">
            <label htmlFor="intentId" className="text-sm font-medium">
              Intent ID
            </label>
            <Input
              id="intentId"
              type="text"
              placeholder="0x..."
              value={intentId}
              onChange={(e) => setIntentId(e.target.value)}
            />
          </div>

          <Button
            variant="default"
            className="w-full"
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
            <h3 className="text-lg font-medium mb-2">Verification Result</h3>
            <div
              className={`p-4 rounded-md ${
                verificationResult.matches
                  ? "bg-green-500/10"
                  : "bg-red-500/10"
              }`}
            >
              <div className="flex items-center gap-2 mb-2">
                {verificationResult.matches ? (
                  <>
                    <CheckCircle className="h-5 w-5 text-green-500" />
                    <p className="text-base font-medium text-green-500">
                      Transaction matches intent
                    </p>
                  </>
                ) : (
                  <>
                    <AlertTriangle className="h-5 w-5 text-red-500" />
                    <p className="text-base font-medium text-red-500">
                      Transaction does not match intent
                    </p>
                  </>
                )}
              </div>

              {!verificationResult.matches && verificationResult.discrepancies && verificationResult.discrepancies.length > 0 && (
                <div className="space-y-2 mt-4">
                  <p className="text-sm font-medium">Discrepancies Found:</p>
                  <ul className="list-disc pl-5 text-sm space-y-1">
                    {verificationResult.discrepancies.map((discrepancy, index) => (
                      <li key={index} className="text-red-500">
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
                      <p className="text-xs text-muted-foreground">Intent</p>
                      <div className="p-2 bg-background rounded-md">
                        <p className="text-xs">{getIntentDescription(intentDetails)}</p>
                        <p className="text-xs text-muted-foreground">
                          Created: {formatTime(intentDetails.timestamp)}
                        </p>
                      </div>
                    </div>
                    <div className="space-y-1">
                      <p className="text-xs text-muted-foreground">Transaction</p>
                      <div className="p-2 bg-background rounded-md">
                        <p className="text-xs">Transaction {transactionHash.substring(0, 6)}...{transactionHash.substring(transactionHash.length - 4)}</p>
                        <p className="text-xs text-muted-foreground">
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
                      variant="gradient"
                      className="w-full"
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