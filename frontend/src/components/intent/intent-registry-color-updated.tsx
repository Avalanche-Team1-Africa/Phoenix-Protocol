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
        <span className="px-2 py-1 bg-phoenix-light-error-light text-phoenix-light-error dark:bg-phoenix-dark-error-light dark:text-phoenix-dark-error rounded-full text-xs font-medium flex items-center gap-1">
          <XCircle className="h-3 w-3" /> Mismatch
        </span>
      );
    }

    if (intent.status === "verified") {
      return (
        <span className="px-2 py-1 bg-phoenix-light-success-light text-phoenix-light-success dark:bg-phoenix-dark-success-light dark:text-phoenix-dark-success rounded-full text-xs font-medium flex items-center gap-1">
          <CheckCircle className="h-3 w-3" /> Verified
        </span>
      );
    }

    if (intent.status === "executed") {
      return (
        <span className="px-2 py-1 bg-phoenix-light-warning-light text-phoenix-light-warning dark:bg-phoenix-dark-warning-light dark:text-phoenix-dark-warning rounded-full text-xs font-medium flex items-center gap-1">
          <Clock className="h-3 w-3" /> Executed
        </span>
      );
    }

    if (intent.status === "confirmed") {
      return (
        <span className="px-2 py-1 bg-phoenix-light-info-light text-phoenix-light-info dark:bg-phoenix-dark-info-light dark:text-phoenix-dark-info rounded-full text-xs font-medium flex items-center gap-1">
          <Clock className="h-3 w-3" /> Confirmed
        </span>
      );
    }

    return (
      <span className="px-2 py-1 bg-gray-200 text-gray-600 dark:bg-gray-700 dark:text-gray-400 rounded-full text-xs font-medium flex items-center gap-1">
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
        <Button 
          className="bg-phoenix-light-primary hover:bg-phoenix-light-primary-hover dark:bg-phoenix-dark-primary dark:hover:bg-phoenix-dark-primary-hover text-white" 
          onClick={() => window.location.reload()} 
          disabled={isLoading || loading}
        >
          Refresh
        </Button>
      </div>

      {(error || intentError) && (
        <Alert className="alert-error">
          {error || intentError}
        </Alert>
      )}

      <Alert className="alert-info">
        <div className="flex items-start gap-2">
          <Info className="h-4 w-4 text-phoenix-light-info dark:text-phoenix-dark-info mt-0.5" />
          <div>
            <p className="text-sm font-medium text-phoenix-light-info dark:text-phoenix-dark-info">How Intent Verification Works</p>
            <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
              Phoenix Protocol logs your transaction intents before execution. This allows the system to verify that
              the executed transaction matches your original intent, protecting you from malicious frontends or
              accidental transactions. If a mismatch is detected, you can initiate a rollback through the recovery module.
            </p>
          </div>
        </div>
      </Alert>

      {isLoading || loading ? (
        <div className="flex items-center justify-center p-8">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-phoenix-light-primary dark:border-phoenix-dark-primary"></div>
        </div>
      ) : intents.length === 0 ? (
        <Card className="border-phoenix-light-border dark:border-phoenix-dark-border">
          <CardContent className="py-8 text-center">
            <p className="text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">No transaction intents found</p>
          </CardContent>
        </Card>
      ) : (
        <div className="space-y-4">
          {intents.map(intent => (
            <Card 
              key={intent.id} 
              className={
                intent.status === "mismatch" ? "border-phoenix-light-error dark:border-phoenix-dark-error" : 
                intent.status === "verified" ? "border-phoenix-light-success dark:border-phoenix-dark-success" : 
                "border-phoenix-light-border dark:border-phoenix-dark-border"
              }
            >
              <CardHeader className="pb-2">
                <div className="flex justify-between items-start">
                  <div>
                    <CardTitle className="text-base text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
                      {getIntentDescription(intent)}
                    </CardTitle>
                    <CardDescription className="text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
                      Created {new Date(intent.timestamp).toLocaleString()}
                    </CardDescription>
                  </div>
                  {getStatusBadge(intent)}
                </div>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <p className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Intent ID</p>
                    <p className="text-sm font-mono text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{intent.id}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Expires In</p>
                    <p className="text-sm text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{formatTimeRemaining(intent.timestamp)}</p>
                  </div>
                  <div className="space-y-2">
                    <p className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Transaction Type</p>
                    <p className="text-sm text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{intent.txType}</p>
                  </div>
                  {intent.txHash && (
                    <div className="space-y-2">
                      <p className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Transaction Hash</p>
                      <p className="text-sm font-mono text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{intent.txHash}</p>
                    </div>
                  )}
                </div>

                {intent.status === "executed" || intent.status === "verified" || intent.status === "mismatch" ? (
                  <div className="mt-4">
                    <h4 className="text-sm font-medium mb-2 text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Intent Verification</h4>
                    {selectedIntentId === intent.id && showVerification ? (
                      verificationResult ? (
                        <div className={`p-4 rounded-md ${verificationResult.matches ? "bg-phoenix-light-success-light dark:bg-phoenix-dark-success-light" : "bg-phoenix-light-error-light dark:bg-phoenix-dark-error-light"}`}>
                          <div className="flex items-center gap-2 mb-2">
                            {verificationResult.matches ? (
                              <>
                                <CheckCircle className="h-4 w-4 text-phoenix-light-success dark:text-phoenix-dark-success" />
                                <p className="text-sm font-medium text-phoenix-light-success dark:text-phoenix-dark-success">Transaction matches intent</p>
                              </>
                            ) : (
                              <>
                                <AlertTriangle className="h-4 w-4 text-phoenix-light-error dark:text-phoenix-dark-error" />
                                <p className="text-sm font-medium text-phoenix-light-error dark:text-phoenix-dark-error">Transaction does not match intent</p>
                              </>
                            )}
                          </div>
                          {!verificationResult.matches && verificationResult.discrepancies && verificationResult.discrepancies.length > 0 && (
                            <div className="space-y-1 mt-2">
                              <p className="text-sm font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Discrepancies:</p>
                              <ul className="list-disc pl-5 text-xs space-y-1">
                                {verificationResult.discrepancies.map((discrepancy, index) => (
                                  <li key={index} className="text-phoenix-light-error dark:text-phoenix-dark-error">{discrepancy}</li>
                                ))}
                              </ul>
                              <div className="mt-4">
                                <Link href="/disputes">
                                  <Button 
                                    variant="outline" 
                                    className="border-phoenix-light-error text-phoenix-light-error hover:bg-phoenix-light-error-light dark:border-phoenix-dark-error dark:text-phoenix-dark-error dark:hover:bg-phoenix-dark-error-light text-xs"
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
                          <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-phoenix-light-primary dark:border-phoenix-dark-primary"></div>
                        </div>
                      )
                    ) : (
                      intent.verification ? (
                        <div className={`p-4 rounded-md ${intent.verification.success ? "bg-phoenix-light-success-light dark:bg-phoenix-dark-success-light" : "bg-phoenix-light-error-light dark:bg-phoenix-dark-error-light"}`}>
                          <div className="flex items-center gap-2 mb-2">
                            {intent.verification.success ? (
                              <>
                                <CheckCircle className="h-4 w-4 text-phoenix-light-success dark:text-phoenix-dark-success" />
                                <p className="text-sm font-medium text-phoenix-light-success dark:text-phoenix-dark-success">Transaction matches intent</p>
                              </>
                            ) : (
                              <>
                                <AlertTriangle className="h-4 w-4 text-phoenix-light-error dark:text-phoenix-dark-error" />
                                <p className="text-sm font-medium text-phoenix-light-error dark:text-phoenix-dark-error">Transaction does not match intent</p>
                              </>
                            )}
                          </div>
                          {!intent.verification.success && intent.verification.discrepancies && intent.verification.discrepancies.length > 0 && (
                            <div className="space-y-1 mt-2">
                              <p className="text-sm font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">Discrepancies:</p>
                              <ul className="list-disc pl-5 text-xs space-y-1">
                                {intent.verification.discrepancies.map((discrepancy: string, index: number) => (
                                  <li key={index} className="text-phoenix-light-error dark:text-phoenix-dark-error">{discrepancy}</li>
                                ))}
                              </ul>
                              <div className="mt-4">
                                <Link href="/disputes">
                                  <Button 
                                    variant="outline" 
                                    className="border-phoenix-light-error text-phoenix-light-error hover:bg-phoenix-light-error-light dark:border-phoenix-dark-error dark:text-phoenix-dark-error dark:hover:bg-phoenix-dark-error-light text-xs"
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
                          className="border-phoenix-light-primary text-phoenix-light-primary hover:bg-phoenix-light-primary-light dark:border-phoenix-dark-primary dark:text-phoenix-dark-primary dark:hover:bg-phoenix-dark-primary-light"
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
                      className="border-phoenix-light-error text-phoenix-light-error hover:bg-phoenix-light-error-light dark:border-phoenix-dark-error dark:text-phoenix-dark-error dark:hover:bg-phoenix-dark-error-light"
                    >
                      Submit Dispute
                    </Button>
                  </Link>
                )}
                <Button 
                  variant="ghost" 
                  size="sm"
                  className="text-phoenix-light-text-primary hover:bg-phoenix-light-muted dark:text-phoenix-dark-text-primary dark:hover:bg-phoenix-dark-muted"
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