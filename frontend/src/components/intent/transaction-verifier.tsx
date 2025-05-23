"use client";

import React, { useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Alert } from "@/components/ui/alert";
import { CheckCircle, XCircle, AlertTriangle, Search, ArrowRight } from "lucide-react";
import { useGuardian } from "@/context/guardian-provider";

export function TransactionVerifier() {
  const { verifyTransactionIntent, isLoading } = useGuardian();
  const [transactionHash, setTransactionHash] = useState("");
  const [intentId, setIntentId] = useState("");
  const [verificationResult, setVerificationResult] = useState<{
    matches: boolean;
    discrepancies?: string[];
  } | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [showResult, setShowResult] = useState(false);

  const handleVerify = async () => {
    if (!transactionHash || !intentId) {
      setError("Please enter both transaction hash and intent ID");
      return;
    }

    setError(null);
    setShowResult(false);

    try {
      const result = await verifyTransactionIntent(transactionHash, intentId);
      setVerificationResult(result);
      setShowResult(true);
    } catch (error: any) {
      setError(error.message || "Verification failed");
    }
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
            If a mismatch is found, you can initiate a recovery process.
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

              {!verificationResult.matches && verificationResult.discrepancies && (
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

              <div className="mt-4">
                <div className="grid grid-cols-2 gap-4">
                  <div className="space-y-1">
                    <p className="text-xs text-muted-foreground">Intent</p>
                    <div className="p-2 bg-background rounded-md">
                      <p className="text-xs">Send 0.25 ETH to 0x1234...5678</p>
                      <p className="text-xs text-muted-foreground">
                        Created: 2 hours ago
                      </p>
                    </div>
                  </div>
                  <div className="space-y-1">
                    <p className="text-xs text-muted-foreground">Transaction</p>
                    <div className="p-2 bg-background rounded-md">
                      <p className="text-xs">Sent 2.5 ETH to 0x1234...5678</p>
                      <p className="text-xs text-muted-foreground">
                        Executed: 1 hour ago
                      </p>
                    </div>
                  </div>
                </div>
              </div>

              {!verificationResult.matches && (
                <div className="mt-6">
                  <Button
                    variant="gradient"
                    className="w-full"
                  >
                    <ArrowRight className="h-4 w-4 mr-2" />
                    Initiate Recovery Process
                  </Button>
                </div>
              )}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}