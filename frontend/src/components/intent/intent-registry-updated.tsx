"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Alert } from "@/components/ui/alert";
import { CheckCircle, XCircle, AlertTriangle, Clock, Info } from "lucide-react";
import { useWallet } from "@/context/wallet-context";
import { useTransactionIntent } from "@/hooks/use-transaction-intent";
import Link from "next/link";

export function IntentRegistry() {
  const { wallet } = useWallet();
  const { 
    getAllIntents, 
    verifyTransaction, 
    isLoading, 
    error: intentError 
  } = useTransactionIntent();
  
  const [selectedIntentId, setSelectedIntentId] = useState<string | null>(null);
  const [showVerification, setShowVerification] = useState(false);
  const [verificationResult, setVerificationResult] = useState<{
    matches: boolean;
    discrepancies?: string[];
  } | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  // Get all intents
  const intents = getAllIntents();

  // Verify transaction against intent
  const handleVerifyTransaction = async (intentId: string, txHash: string) => {
    if (!txHash) {
      setError("No transaction hash available for verification");
      return;
    }

    setLoading(true);
    setError(null);
    setSelectedIntentId(intentId);
    setShowVerification(true);
    setVerificationResult(null);

    try {
      const result = await verifyTransaction(intentId, txHash);
      setVerificationResult(result);
      setLoading(false);
    } catch (error: any) {
      console.error("Error verifying transaction:", error);
      setError(error.message || "Failed to verify transaction");
      setLoading(false);
    }
  };

  // Format time remaining (assuming intents expire after 24 hours)
  const formatTimeRemaining = (timestamp: number) => {
    const expiresAt = timestamp + (24 * 60 * 60 * 1000); // 24 hours after creation
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
  const getStatusBadge = (intent: any) => {
    if (intent.status === "mismatch") {
      return (
        <span className="px-2 py-1 bg-red-500/20 text-red-500 rounded-full text-xs font-medium flex items-center gap-1">
          <XCircle className="h-3 w-3" /> Mismatch
        </span>
      );
    }

    if (intent.status === "verified") {
      return (
        <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs font-medium flex items-center gap-1">
          <CheckCircle className="h-3 w-3" /> Verified
        </span>
      );
    }

    if (intent.status === "executed") {
      return (
        <span className="px-2 py-1 bg-yellow-500/20 text-yellow-500 rounded-full text-xs font-medium flex items-center gap-1">
          <Clock className="h-3 w-3" /> Executed
        </span>
      );
    }

    if (intent.status === "confirmed") {
      return (
        <span className="px-2 py-1 bg-blue-500/20 text-blue-500 rounded-full text-xs font-medium flex items-center gap-1">
          <Clock className="h-3 w-3" /> Confirmed
        </span>
      );
    }

    return (
      <span className="px-2 py-1 bg-gray-500/20 text-gray-500 rounded-full text-xs font-medium flex items-center gap-1">
        <Clock className="h-3 w-3" /> Pending
      </span>
    );
  };

  // Get intent description
  const getIntentDescription = (intent: any) => {
    switch (intent.txType) {
      case "swap":
        return `Swap ${intent.payload.amountIn} ${intent.payload.tokenInSymbol} for ${intent.payload.tokenOutSymbol}`;
      case "transfer":
        return `Transfer ${intent.payload.amount} ${intent.payload.tokenSymbol} to ${intent.payload.recipient.substring(0, 6)}...${intent.payload.recipient.substring(38)}`;
      case "stake":
        return `Stake ${intent.payload.amount} ${intent.payload.tokenSymbol} for ${intent.payload.duration} days`;
      case "mint":
        return `Mint ${intent.payload.quantity} ${intent.payload.nftName} for ${intent.payload.price} ${intent.payload.currencySymbol}`;
      default:
        return `${intent.txType} Transaction`;
    }
  };

  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold">Transaction Intent Registry</h2>
        <Button variant="default" onClick={() => window.location.reload()} disabled={isLoading || loading}>
          Refresh
        </Button>
      </div>

      {(error || intentError) && (
        <Alert className="bg-red-500/10 border-red-500/20 text-red-500">
          {error || intentError}
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

      {isLoading || loading ? (
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
            <Card 
              key={intent.id} 
              className={
                intent.status === "mismatch" ? "border-red-500/20" : 
                intent.status === "verified" ? "border-green-500/20" : ""
              }
            >
              <CardHeader className="pb-2">
                <div className="flex justify-between items-start">
                  <div>
                    <CardTitle className="text-base">{getIntentDescription(intent)}</CardTitle>
                    <CardDescription>
                      Created {new Date(intent.timestamp).toLocaleString()}
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
                    <p className="text-sm">{formatTimeRemaining(intent.timestamp)}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-muted-foreground">Transaction Type</p>
                    <p className="text-sm">{intent.txType}</p>
                  </div>
                  {intent.txHash && (
                    <div className="space-y-2">
                      <p className="text-sm text-muted-foreground">Transaction Hash</p>
                      <p className="text-sm font-mono">{intent.txHash}</p>
                    </div>
                  )}
                </div>

                {intent.status === "executed" || intent.status === "verified" || intent.status === "mismatch" ? (
                  <div className="mt-4">
                    <h4 className="text-sm font-medium mb-2">Intent Verification</h4>
                    {selectedIntentId === intent.id && showVerification ? (
                      verificationResult ? (
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
                          {!verificationResult.matches && verificationResult.discrepancies && verificationResult.discrepancies.length > 0 && (
                            <div className="space-y-1 mt-2">
                              <p className="text-sm font-medium">Discrepancies:</p>
                              <ul className="list-disc pl-5 text-xs space-y-1">
                                {verificationResult.discrepancies.map((discrepancy, index) => (
                                  <li key={index} className="text-red-500">{discrepancy}</li>
                                ))}
                              </ul>
                              <div className="mt-4">
                                <Link href="/disputes">
                                  <Button 
                                    variant="outline" 
                                    className="border-red-500 text-red-500 hover:bg-red-500/10 text-xs"
                                    size="sm"
                                  >
                                    Submit Dispute
                                  </Button>
                                </Link>
                              </div>
                            </div>
                          )}
                        </div>
                      ) : (
                        <div className="flex items-center justify-center p-4">
                          <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-orange-500"></div>
                        </div>
                      )
                    ) : (
                      intent.verification ? (
                        <div className={`p-4 rounded-md ${intent.verification.success ? "bg-green-500/10" : "bg-red-500/10"}`}>
                          <div className="flex items-center gap-2 mb-2">
                            {intent.verification.success ? (
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
                          {!intent.verification.success && intent.verification.discrepancies && intent.verification.discrepancies.length > 0 && (
                            <div className="space-y-1 mt-2">
                              <p className="text-sm font-medium">Discrepancies:</p>
                              <ul className="list-disc pl-5 text-xs space-y-1">
                                {intent.verification.discrepancies.map((discrepancy: string, index: number) => (
                                  <li key={index} className="text-red-500">{discrepancy}</li>
                                ))}
                              </ul>
                              <div className="mt-4">
                                <Link href="/disputes">
                                  <Button 
                                    variant="outline" 
                                    className="border-red-500 text-red-500 hover:bg-red-500/10 text-xs"
                                    size="sm"
                                  >
                                    Submit Dispute
                                  </Button>
                                </Link>
                              </div>
                            </div>
                          )}
                        </div>
                      ) : (
                        <Button 
                          variant="outline" 
                          size="sm"
                          onClick={() => handleVerifyTransaction(intent.id, intent.txHash || "")}
                          disabled={!intent.txHash || loading}
                        >
                          Verify Transaction
                        </Button>
                      )
                    )}
                  </div>
                ) : null}
              </CardContent>
              <CardFooter className="flex justify-end gap-2">
                {intent.status === "mismatch" && (
                  <Link href="/disputes">
                    <Button 
                      variant="outline" 
                      size="sm"
                      className="border-red-500 text-red-500 hover:bg-red-500/10"
                    >
                      Submit Dispute
                    </Button>
                  </Link>
                )}
                <Button 
                  variant="ghost" 
                  size="sm"
                  onClick={() => {
                    setSelectedIntentId(intent.id);
                    setShowVerification(false);
                  }}
                >
                  View Details
                </Button>
              </CardFooter>
            </Card>
          ))}
        </div>
      )}
    </div>
  );
}