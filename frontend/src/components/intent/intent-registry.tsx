"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Alert } from "@/components/ui/alert";
import { CheckCircle, XCircle, AlertTriangle, Clock, Info } from "lucide-react";
import { useWallet } from "@/context/wallet-context";

// Mock intent data type
interface Intent {
  id: string;
  type: "Send" | "Swap" | "Stake" | "Approve" | "Other";
  tokenAddress: string;
  tokenSymbol: string;
  amount: string;
  recipient: string;
  createdAt: number;
  expiresAt: number;
  executed: boolean;
  cancelled: boolean;
  transactionHash?: string;
}

export function IntentRegistry() {
  const { wallet } = useWallet();
  const [intents, setIntents] = useState<Intent[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedIntent, setSelectedIntent] = useState<Intent | null>(null);
  const [showVerification, setShowVerification] = useState(false);
  const [verificationResult, setVerificationResult] = useState<{
    matches: boolean;
    discrepancies?: string[];
  } | null>(null);

  // Load intents when wallet connects
  useEffect(() => {
    if (wallet.connected) {
      loadIntents();
    }
  }, [wallet.connected, wallet.address]);

  // Load intents from blockchain/API
  const loadIntents = async () => {
    setLoading(true);
    setError(null);

    try {
      // In a real implementation, this would fetch data from the blockchain
      // Mock implementation for demo
      await new Promise(resolve => setTimeout(resolve, 1000));

      // Mock data
      const mockIntents: Intent[] = [
        {
          id: "0x1234...5678",
          type: "Send",
          tokenAddress: "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2",
          tokenSymbol: "ETH",
          amount: "1.5",
          recipient: "0xabcd...efgh",
          createdAt: Date.now() - 2 * 60 * 60 * 1000, // 2 hours ago
          expiresAt: Date.now() + 22 * 60 * 60 * 1000, // 22 hours from now
          executed: true,
          cancelled: false,
          transactionHash: "0x9876...5432"
        },
        {
          id: "0xabcd...efgh",
          type: "Swap",
          tokenAddress: "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48",
          tokenSymbol: "USDC",
          amount: "500",
          recipient: "0x1234...5678",
          createdAt: Date.now() - 1 * 60 * 60 * 1000, // 1 hour ago
          expiresAt: Date.now() + 23 * 60 * 60 * 1000, // 23 hours from now
          executed: false,
          cancelled: false
        },
        {
          id: "0xfedc...ba98",
          type: "Approve",
          tokenAddress: "0x6B175474E89094C44Da98b954EedeAC495271d0F",
          tokenSymbol: "DAI",
          amount: "1000",
          recipient: "0x7890...1234",
          createdAt: Date.now() - 3 * 60 * 60 * 1000, // 3 hours ago
          expiresAt: Date.now() + 21 * 60 * 60 * 1000, // 21 hours from now
          executed: false,
          cancelled: true
        }
      ];

      setIntents(mockIntents);
      setLoading(false);
    } catch (error) {
      console.error("Error loading intents:", error);
      setError("Failed to load intents");
      setLoading(false);
    }
  };

  // Verify transaction against intent
  const verifyTransaction = async (intent: Intent) => {
    if (!intent.transactionHash) {
      setError("No transaction hash available for verification");
      return;
    }

    setLoading(true);
    setError(null);

    try {
      // In a real implementation, this would verify the transaction against the intent on-chain
      // Mock implementation for demo
      await new Promise(resolve => setTimeout(resolve, 1500));

      // Mock verification result
      const mockResult = {
        matches: Math.random() > 0.3, // 70% chance of matching
        discrepancies: !intent.executed ? [] : ["Amount in transaction (2.5 ETH) doesn't match intent (0.25 ETH)"]
      };

      setVerificationResult(mockResult);
      setShowVerification(true);
      setLoading(false);
    } catch (error) {
      console.error("Error verifying transaction:", error);
      setError("Failed to verify transaction");
      setLoading(false);
    }
  };

  // Cancel intent
  const cancelIntent = async (intentId: string) => {
    setLoading(true);
    setError(null);

    try {
      // In a real implementation, this would interact with the blockchain
      // Mock implementation for demo
      await new Promise(resolve => setTimeout(resolve, 1000));

      // Update intent status
      setIntents(
        intents.map(intent =>
          intent.id === intentId
            ? { ...intent, cancelled: true }
            : intent
        )
      );

      setLoading(false);
    } catch (error) {
      console.error("Error cancelling intent:", error);
      setError("Failed to cancel intent");
      setLoading(false);
    }
  };

  // Format time remaining
  const formatTimeRemaining = (expiresAt: number) => {
    const now = Date.now();
    const remaining = expiresAt - now;

    if (remaining <= 0) {
      return "Expired";
    }

    const hours = Math.floor(remaining / (1000 * 60 * 60));
    const minutes = Math.floor((remaining % (1000 * 60 * 60)) / (1000 * 60));

    return `${hours}h ${minutes}m`;
  };

  // Get status badge
  const getStatusBadge = (intent: Intent) => {
    if (intent.cancelled) {
      return (
        <span className="px-2 py-1 bg-red-500/20 text-red-500 rounded-full text-xs font-medium flex items-center gap-1">
          <XCircle className="h-3 w-3" /> Cancelled
        </span>
      );
    }

    if (intent.executed) {
      return (
        <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs font-medium flex items-center gap-1">
          <CheckCircle className="h-3 w-3" /> Executed
        </span>
      );
    }

    if (intent.expiresAt < Date.now()) {
      return (
        <span className="px-2 py-1 bg-gray-500/20 text-gray-500 rounded-full text-xs font-medium flex items-center gap-1">
          <Clock className="h-3 w-3" /> Expired
        </span>
      );
    }

    return (
      <span className="px-2 py-1 bg-blue-500/20 text-blue-500 rounded-full text-xs font-medium flex items-center gap-1">
        <Clock className="h-3 w-3" /> Pending
      </span>
    );
  };

  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold">Transaction Intent Registry</h2>
        <Button variant="default" onClick={loadIntents} disabled={loading}>
          Refresh
        </Button>
      </div>

      {error && (
        <Alert className="bg-red-500/10 border-red-500/20 text-red-500">
          {error}
        </Alert>
      )}

      <Alert className="bg-blue-500/10 border-blue-500/20">
        <div className="flex items-start gap-2">
          <Info className="h-4 w-4 text-blue-500 mt-0.5" />
          <div>
            <p className="text-sm font-medium text-blue-500">How Intent Verification Works</p>
            <p className="text-xs text-muted-foreground">
              Phoenix Protocol logs your transaction intents before execution. This allows the system to verify that
              the executed transaction matches your original intent, protecting you from malicious frontends or
              accidental transactions. If a mismatch is detected, you can initiate a rollback through the recovery module.
            </p>
          </div>
        </div>
      </Alert>

      {loading ? (
        <div className="flex items-center justify-center p-8">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-orange-500"></div>
        </div>
      ) : intents.length === 0 ? (
        <Card>
          <CardContent className="py-8 text-center">
            <p className="text-muted-foreground">No transaction intents found</p>
          </CardContent>
        </Card>
      ) : (
        <div className="space-y-4">
          {intents.map(intent => (
            <Card key={intent.id} className={intent.cancelled ? "border-red-500/20" : intent.executed ? "border-green-500/20" : ""}>
              <CardHeader className="pb-2">
                <div className="flex justify-between items-start">
                  <div>
                    <CardTitle className="text-base">{intent.type} Intent</CardTitle>
                    <CardDescription>
                      Created {new Date(intent.createdAt).toLocaleString()}
                    </CardDescription>
                  </div>
                  {getStatusBadge(intent)}
                </div>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Intent ID</p>
                    <p className="text-sm font-mono">{intent.id}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Expires In</p>
                    <p className="text-sm">{formatTimeRemaining(intent.expiresAt)}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Token</p>
                    <p className="text-sm">{intent.tokenSymbol}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Amount</p>
                    <p className="text-sm">{intent.amount} {intent.tokenSymbol}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Recipient</p>
                    <p className="text-sm font-mono">{intent.recipient}</p>
                  </div>
                  {intent.transactionHash && (
                    <div className="space-y-2">
                      <p className="text-sm text-muted-foreground">Transaction Hash</p>
                      <p className="text-sm font-mono">{intent.transactionHash}</p>
                    </div>
                  )}
                </div>

                {intent.executed && selectedIntent?.id === intent.id && showVerification && (
                  <div className="mt-4">
                    <h4 className="text-sm font-medium mb-2">Intent Verification Result</h4>
                    {verificationResult ? (
                      <div className={`p-4 rounded-md ${verificationResult.matches ? "bg-green-500/10" : "bg-red-500/10"}`}>
                        <div className="flex items-center gap-2 mb-2">
                          {verificationResult.matches ? (
                            <>
                              <CheckCircle className="h-4 w-4 text-green-500" />
                              <p className="text-sm font-medium text-green-500">Transaction matches intent</p>
                            </>
                          ) : (
                            <>
                              <AlertTriangle className="h-4 w-4 text-red-500" />
                              <p className="text-sm font-medium text-red-500">Transaction does not match intent</p>
                            </>
                          )}
                        </div>
                        {!verificationResult.matches && verificationResult.discrepancies && (
                          <div className="space-y-1 mt-2">
                            <p className="text-sm font-medium">Discrepancies:</p>
                            <ul className="list-disc pl-5 text-xs space-y-1">
                              {verificationResult.discrepancies.map((discrepancy, index) => (
                                <li key={index} className="text-red-500">{discrepancy}</li>
                              ))}
                            </ul>
                            <div className="mt-4">
                              <Button 
                                variant="outline" 
                                className="border-red-500 text-red-500 hover:bg-red-500/10 text-xs"
                                size="sm"
                              >
                                Initiate Recovery
                              </Button>
                            </div>
                          </div>
                        )}
                      </div>
                    ) : (
                      <div className="flex items-center justify-center p-4">
                        <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-orange-500"></div>
                      </div>
                    )}
                  </div>
                )}
              </CardContent>
              <CardFooter className="flex justify-end gap-2">
                {!intent.cancelled && !intent.executed && (
                  <Button 
                    variant="outline" 
                    size="sm"
                    onClick={() => cancelIntent(intent.id)}
                    disabled={loading}
                  >
                    Cancel Intent
                  </Button>
                )}
                {intent.executed && (
                  <Button 
                    variant={selectedIntent?.id === intent.id && showVerification ? "outline" : "default"}
                    size="sm"
                    onClick={() => {
                      setSelectedIntent(intent);
                      setShowVerification(!showVerification);
                      if (!showVerification || selectedIntent?.id !== intent.id) {
                        setVerificationResult(null);
                        verifyTransaction(intent);
                      }
                    }}
                    disabled={loading}
                  >
                    {selectedIntent?.id === intent.id && showVerification ? "Hide Verification" : "Verify Transaction"}
                  </Button>
                )}
              </CardFooter>
            </Card>
          ))}
        </div>
      )}
    </div>
  );
}