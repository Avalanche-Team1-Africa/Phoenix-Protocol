"use client";

import React, { useState } from "react";
import { Shield, AlertTriangle, CheckCircle, Lock, Copy, ExternalLink } from "lucide-react";
import { Button } from "@/components/ui/button";
import { formatAddress } from "@/lib/utils/blockchain";

interface TransactionData {
  from: string;
  to: string;
  value: string;
  data?: string;
  gasLimit: string;
  gasPrice: string;
  nonce: number;
  chainId: number;
}

interface TransactionSigningProps {
  transaction: TransactionData;
  onSign: () => Promise<{ success: boolean; hash?: string; error?: string }>;
  onCancel: () => void;
}

export function TransactionSigning({
  transaction,
  onSign,
  onCancel,
}: TransactionSigningProps) {
  const [isSigning, setIsSigning] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [showAdvanced, setShowAdvanced] = useState(false);
  const [signatureChecked, setSignatureChecked] = useState(false);

  const handleSign = async () => {
    if (!signatureChecked) {
      setError("Please confirm you've verified the transaction details");
      return;
    }

    setIsSigning(true);
    setError(null);

    try {
      const result = await onSign();
      if (!result.success) {
        setError(result.error || "Failed to sign transaction");
      }
    } catch (err: any) {
      setError(err.message || "An error occurred while signing");
    } finally {
      setIsSigning(false);
    }
  };

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text);
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center">
          <Lock className="mr-2 h-5 w-5 text-orange-500" />
          <h2 className="text-xl font-bold">Sign Transaction</h2>
        </div>
        <div className="rounded-full bg-orange-500/10 px-3 py-1 text-xs font-medium text-orange-500">
          Protected by Phoenix Protocol
        </div>
      </div>

      {/* Security Notice */}
      <div className="rounded-lg border border-orange-500/30 bg-orange-500/5 p-4">
        <div className="flex items-start">
          <Shield className="mr-3 mt-0.5 h-5 w-5 text-orange-500" />
          <div>
            <h3 className="font-medium text-orange-500">Security Notice</h3>
            <p className="mt-1 text-sm text-orange-500/80">
              Always verify transaction details before signing. This transaction will be
              recorded on the blockchain and cannot be reversed without using Phoenix
              Protocol's recovery features.
            </p>
          </div>
        </div>
      </div>

      {/* Transaction Details */}
      <div className="rounded-lg border bg-muted/20 p-4">
        <h3 className="mb-4 font-medium">Transaction Details</h3>

        <div className="space-y-3">
          <div className="flex flex-col space-y-1">
            <span className="text-xs text-muted-foreground">From</span>
            <div className="flex items-center justify-between">
              <span className="font-mono text-sm">{formatAddress(transaction.from, 10)}</span>
              <button
                onClick={() => copyToClipboard(transaction.from)}
                className="text-muted-foreground hover:text-foreground"
              >
                <Copy className="h-4 w-4" />
              </button>
            </div>
          </div>

          <div className="flex flex-col space-y-1">
            <span className="text-xs text-muted-foreground">To</span>
            <div className="flex items-center justify-between">
              <span className="font-mono text-sm">{formatAddress(transaction.to, 10)}</span>
              <div className="flex items-center space-x-2">
                <a
                  href={`https://etherscan.io/address/${transaction.to}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-muted-foreground hover:text-foreground"
                >
                  <ExternalLink className="h-4 w-4" />
                </a>
                <button
                  onClick={() => copyToClipboard(transaction.to)}
                  className="text-muted-foreground hover:text-foreground"
                >
                  <Copy className="h-4 w-4" />
                </button>
              </div>
            </div>
          </div>

          <div className="flex flex-col space-y-1">
            <span className="text-xs text-muted-foreground">Value</span>
            <span className="font-medium">{transaction.value}</span>
          </div>

          <div className="flex flex-col space-y-1">
            <span className="text-xs text-muted-foreground">Network</span>
            <span className="font-medium">
              {transaction.chainId === 1
                ? "Ethereum Mainnet"
                : transaction.chainId === 11155111
                ? "Sepolia Testnet"
                : transaction.chainId === 43114
                ? "Avalanche C-Chain"
                : transaction.chainId === 43113
                ? "Avalanche Fuji Testnet"
                : `Chain ID: ${transaction.chainId}`}
            </span>
          </div>

          <div className="flex flex-col space-y-1">
            <span className="text-xs text-muted-foreground">Gas Fee (estimated)</span>
            <span className="font-medium">
              {parseFloat(transaction.gasPrice) * parseFloat(transaction.gasLimit)} ETH
            </span>
          </div>

          {/* Advanced Details Toggle */}
          <button
            onClick={() => setShowAdvanced(!showAdvanced)}
            className="mt-2 text-xs text-muted-foreground hover:text-foreground"
          >
            {showAdvanced ? "Hide" : "Show"} Advanced Details
          </button>

          {/* Advanced Details */}
          {showAdvanced && (
            <div className="mt-2 space-y-3 rounded-lg border bg-muted/30 p-3">
              <div className="flex flex-col space-y-1">
                <span className="text-xs text-muted-foreground">Gas Limit</span>
                <span className="font-mono text-sm">{transaction.gasLimit}</span>
              </div>

              <div className="flex flex-col space-y-1">
                <span className="text-xs text-muted-foreground">Gas Price</span>
                <span className="font-mono text-sm">{transaction.gasPrice} Gwei</span>
              </div>

              <div className="flex flex-col space-y-1">
                <span className="text-xs text-muted-foreground">Nonce</span>
                <span className="font-mono text-sm">{transaction.nonce}</span>
              </div>

              {transaction.data && (
                <div className="flex flex-col space-y-1">
                  <span className="text-xs text-muted-foreground">Data</span>
                  <div className="max-h-20 overflow-auto rounded bg-muted p-2">
                    <pre className="text-xs">{transaction.data}</pre>
                  </div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="rounded-lg bg-red-500/10 p-3 text-sm text-red-500">
          <div className="flex items-start">
            <AlertTriangle className="mr-2 mt-0.5 h-4 w-4" />
            <span>{error}</span>
          </div>
        </div>
      )}

      {/* Confirmation Checkbox */}
      <div className="flex items-start">
        <input
          type="checkbox"
          id="confirm-signature"
          checked={signatureChecked}
          onChange={() => setSignatureChecked(!signatureChecked)}
          className="mt-1 h-4 w-4 rounded border-muted-foreground/30 text-orange-500 focus:ring-orange-500"
        />
        <label htmlFor="confirm-signature" className="ml-2 text-sm">
          I have verified the transaction details and authorize this transaction to be
          signed and submitted to the blockchain.
        </label>
      </div>

      {/* Action Buttons */}
      <div className="flex justify-end space-x-3">
        <Button variant="outline" onClick={onCancel} disabled={isSigning}>
          Cancel
        </Button>
        <Button
          variant="gradient"
          onClick={handleSign}
          disabled={isSigning || !signatureChecked}
          className="min-w-[120px]"
        >
          {isSigning ? (
            <div className="flex items-center">
              <div className="mr-2 h-4 w-4 animate-spin rounded-full border-2 border-current border-t-transparent"></div>
              Signing...
            </div>
          ) : (
            <div className="flex items-center">
              <CheckCircle className="mr-2 h-4 w-4" />
              Sign Transaction
            </div>
          )}
        </Button>
      </div>
    </div>
  );
}