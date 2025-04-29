"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Select } from "@/components/ui/select";
import { useWallet } from "@/context/wallet-context";
import { useTransactionIntent } from "@/hooks/use-transaction-intent";
import { IntentModal } from "@/components/forms/intent-modal";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { createIntentDescription } from "@/lib/utils/blockchain";
import Link from "next/link";

export default function TransactionsPage() {
  const { wallet } = useWallet();
  const { 
    createIntent, 
    confirmIntent, 
    rejectIntent, 
    getAllIntents, 
    currentIntent,
    isIntentModalOpen,
    setIsIntentModalOpen,
    executeTransaction
  } = useTransactionIntent();
  
  const [isWalletModalOpen, setIsWalletModalOpen] = useState(false);
  const [txType, setTxType] = useState("swap");
  const [tokenIn, setTokenIn] = useState("usdc");
  const [tokenOut, setTokenOut] = useState("avax");
  const [amount, setAmount] = useState("100");
  const [slippage, setSlippage] = useState("1.0");
  const [recipient, setRecipient] = useState("");
  const [intents, setIntents] = useState<any[]>([]);
  const [successMessage, setSuccessMessage] = useState("");
  const [errorMessage, setErrorMessage] = useState("");

  // Load intents when component mounts
  useEffect(() => {
    setIntents(getAllIntents());
  }, [getAllIntents]);

  // Update intents when new ones are created
  useEffect(() => {
    const allIntents = getAllIntents();
    setIntents(allIntents);
  }, [getAllIntents, isIntentModalOpen]);

  const handleCreateIntent = () => {
    if (!wallet.connected) {
      setIsWalletModalOpen(true);
      return;
    }

    try {
      let payload: Record<string, any> = {};
      
      switch (txType) {
        case "swap":
          payload = {
            tokenInSymbol: tokenIn.toUpperCase(),
            tokenOutSymbol: tokenOut.toUpperCase(),
            amountIn: amount,
            slippage,
          };
          break;
          
        case "transfer":
          payload = {
            tokenSymbol: tokenIn.toUpperCase(),
            amount,
            recipient: recipient || "0x1234567890123456789012345678901234567890",
          };
          break;
          
        case "stake":
          payload = {
            tokenSymbol: tokenIn.toUpperCase(),
            amount,
            duration: 30, // Default to 30 days
          };
          break;
          
        case "mint":
          payload = {
            nftName: "CryptoArt",
            quantity: 1,
            price: amount,
            currencySymbol: tokenIn.toUpperCase(),
          };
          break;
      }
      
      createIntent(txType, payload);
      setSuccessMessage("");
      setErrorMessage("");
    } catch (error: any) {
      setErrorMessage(error.message || "Failed to create intent");
    }
  };

  const handleConfirmIntent = async () => {
    try {
      const result = await confirmIntent();
      if (result) {
        setSuccessMessage("Intent confirmed successfully!");
        
        // Simulate transaction execution after 2 seconds
        setTimeout(() => {
          simulateTransactionExecution(result.id);
        }, 2000);
      }
    } catch (error: any) {
      setErrorMessage(error.message || "Failed to confirm intent");
    }
  };

  const handleRejectIntent = () => {
    try {
      rejectIntent();
      setSuccessMessage("Intent rejected");
    } catch (error: any) {
      setErrorMessage(error.message || "Failed to reject intent");
    }
  };

  // Simulate a transaction execution
  const simulateTransactionExecution = async (intentId: string) => {
    try {
      // Mock transaction receipt
      const txReceipt = {
        transactionHash: `0x${Array(64).fill(0).map(() => Math.floor(Math.random() * 16).toString(16)).join('')}`,
        blockNumber: 12345678,
        from: wallet.address,
        to: "0xContractAddress",
        status: 1,
      };
      
      const result = await executeTransaction(intentId, txReceipt);
      
      if (result.success) {
        setSuccessMessage("Transaction executed and verified successfully!");
      } else {
        setErrorMessage(`Transaction verification failed: ${result.discrepancies.join(", ")}`);
      }
      
      // Refresh intents list
      setIntents(getAllIntents());
    } catch (error: any) {
      setErrorMessage(error.message || "Failed to execute transaction");
    }
  };

  // Format timestamp to relative time
  const formatTimeAgo = (timestamp: number) => {
    const seconds = Math.floor((Date.now() - timestamp) / 1000);
    
    if (seconds < 60) return "Just now";
    if (seconds < 3600) return `${Math.floor(seconds / 60)} minutes ago`;
    if (seconds < 86400) return `${Math.floor(seconds / 3600)} hours ago`;
    return `${Math.floor(seconds / 86400)} days ago`;
  };

  return (
    <div className="container mx-auto py-10 max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Transaction Dashboard</h1>
      
      {successMessage && (
        <div className="mb-6 p-4 bg-green-500/10 text-green-500 rounded-md">
          {successMessage}
        </div>
      )}
      
      {errorMessage && (
        <div className="mb-6 p-4 bg-red-500/10 text-red-500 rounded-md">
          {errorMessage}
        </div>
      )}
      
      {!wallet.connected && (
        <Card className="mb-6">
          <CardContent className="py-4">
            <div className="flex items-center justify-between">
              <p className="text-muted-foreground">Connect your wallet to create and manage transactions</p>
              <Button variant="gradient" onClick={() => setIsWalletModalOpen(true)}>
                Connect Wallet
              </Button>
            </div>
          </CardContent>
        </Card>
      )}
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
        {/* Transaction Form */}
        <Card className="md:col-span-1">
          <CardHeader>
            <CardTitle>Create New Transaction</CardTitle>
            <CardDescription>
              Select transaction type and enter details
            </CardDescription>
          </CardHeader>
          <CardContent>
            <form className="space-y-4" onSubmit={(e) => e.preventDefault()}>
              <div className="space-y-2">
                <label htmlFor="txType" className="text-sm font-medium">
                  Transaction Type
                </label>
                <Select 
                  id="txType" 
                  value={txType}
                  onChange={(e) => setTxType(e.target.value)}
                >
                  <option value="swap">Token Swap</option>
                  <option value="transfer">Token Transfer</option>
                  <option value="stake">Stake Tokens</option>
                  <option value="mint">Mint NFT</option>
                </Select>
              </div>
              
              {/* Dynamic Form Fields based on transaction type */}
              {txType === "swap" && (
                <div className="space-y-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div className="space-y-2">
                      <label htmlFor="tokenIn" className="text-sm font-medium">
                        From Token
                      </label>
                      <Select 
                        id="tokenIn" 
                        value={tokenIn}
                        onChange={(e) => setTokenIn(e.target.value)}
                      >
                        <option value="usdc">USDC</option>
                        <option value="avax">AVAX</option>
                        <option value="eth">ETH</option>
                      </Select>
                    </div>
                    <div className="space-y-2">
                      <label htmlFor="tokenOut" className="text-sm font-medium">
                        To Token
                      </label>
                      <Select 
                        id="tokenOut" 
                        value={tokenOut}
                        onChange={(e) => setTokenOut(e.target.value)}
                      >
                        <option value="usdc">USDC</option>
                        <option value="avax">AVAX</option>
                        <option value="eth">ETH</option>
                      </Select>
                    </div>
                  </div>
                  
                  <div className="space-y-2">
                    <label htmlFor="amount" className="text-sm font-medium">
                      Amount
                    </label>
                    <Input 
                      id="amount" 
                      type="text" 
                      placeholder="0.0" 
                      value={amount}
                      onChange={(e) => setAmount(e.target.value)}
                    />
                  </div>
                  
                  <div className="space-y-2">
                    <label htmlFor="slippage" className="text-sm font-medium">
                      Slippage Tolerance (%)
                    </label>
                    <Input 
                      id="slippage" 
                      type="text" 
                      placeholder="1.0" 
                      value={slippage}
                      onChange={(e) => setSlippage(e.target.value)}
                    />
                  </div>
                </div>
              )}
              
              {txType === "transfer" && (
                <div className="space-y-4">
                  <div className="space-y-2">
                    <label htmlFor="tokenIn" className="text-sm font-medium">
                      Token
                    </label>
                    <Select 
                      id="tokenIn" 
                      value={tokenIn}
                      onChange={(e) => setTokenIn(e.target.value)}
                    >
                      <option value="usdc">USDC</option>
                      <option value="avax">AVAX</option>
                      <option value="eth">ETH</option>
                    </Select>
                  </div>
                  
                  <div className="space-y-2">
                    <label htmlFor="amount" className="text-sm font-medium">
                      Amount
                    </label>
                    <Input 
                      id="amount" 
                      type="text" 
                      placeholder="0.0" 
                      value={amount}
                      onChange={(e) => setAmount(e.target.value)}
                    />
                  </div>
                  
                  <div className="space-y-2">
                    <label htmlFor="recipient" className="text-sm font-medium">
                      Recipient Address
                    </label>
                    <Input 
                      id="recipient" 
                      type="text" 
                      placeholder="0x..." 
                      value={recipient}
                      onChange={(e) => setRecipient(e.target.value)}
                    />
                  </div>
                </div>
              )}
              
              <Button 
                type="button" 
                variant="gradient" 
                className="w-full"
                onClick={handleCreateIntent}
                disabled={!wallet.connected}
              >
                Create Intent
              </Button>
            </form>
          </CardContent>
        </Card>
        
        {/* Intent Summary Card */}
        <Card className="md:col-span-1">
          <CardHeader>
            <CardTitle>Intent Summary</CardTitle>
            <CardDescription>
              {currentIntent 
                ? "Review your transaction intent before confirming" 
                : "Create a new intent to see the summary here"}
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {currentIntent ? (
              <>
                <div className="p-4 bg-muted rounded-lg">
                  <p className="text-lg font-medium">
                    {createIntentDescription(currentIntent.txType, currentIntent.payload)}
                  </p>
                </div>
                
                <div className="space-y-2">
                  <div className="flex justify-between">
                    <span className="text-sm text-muted-foreground">Intent ID:</span>
                    <span className="text-sm font-mono">{currentIntent.id}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-sm text-muted-foreground">Status:</span>
                    <span className="text-sm font-medium text-yellow-500">PENDING</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-sm text-muted-foreground">Created:</span>
                    <span className="text-sm">Just now</span>
                  </div>
                </div>
              </>
            ) : (
              <div className="p-8 text-center text-muted-foreground">
                No active intent
              </div>
            )}
          </CardContent>
          <CardFooter className="flex flex-col gap-2">
            <Button 
              variant="default" 
              className="w-full"
              onClick={handleConfirmIntent}
              disabled={!currentIntent}
            >
              Confirm Intent
            </Button>
            <Button 
              variant="outline" 
              className="w-full"
              onClick={handleRejectIntent}
              disabled={!currentIntent}
            >
              Reject Intent
            </Button>
          </CardFooter>
        </Card>
      </div>
      
      {/* Recent Transactions */}
      <div className="mt-10">
        <h2 className="text-2xl font-bold mb-4">Recent Transactions</h2>
        
        {intents.length === 0 ? (
          <Card>
            <CardContent className="py-8 text-center text-muted-foreground">
              No transactions yet. Create your first intent to get started.
            </CardContent>
          </Card>
        ) : (
          <div className="space-y-4">
            {intents.map((intent) => (
              <Card key={intent.id}>
                <CardHeader className="pb-2">
                  <div className="flex justify-between">
                    <CardTitle className="text-lg capitalize">{intent.txType}</CardTitle>
                    <span className={`text-sm font-medium ${
                      intent.status === "matched" ? "text-green-500" : 
                      intent.status === "disputed" ? "text-red-500" : 
                      intent.status === "confirmed" ? "text-blue-500" : 
                      intent.status === "rejected" ? "text-muted-foreground" : 
                      "text-yellow-500"
                    }`}>
                      {intent.status.toUpperCase()}
                    </span>
                  </div>
                  <CardDescription>
                    Intent ID: {intent.id} • {formatTimeAgo(intent.timestamp)}
                  </CardDescription>
                </CardHeader>
                <CardContent className="pb-2">
                  <p>{createIntentDescription(intent.txType, intent.payload)}</p>
                  
                  {intent.status === "disputed" && (
                    <div className="mt-2 p-2 bg-red-500/10 rounded text-sm text-red-500">
                      <p className="font-medium">Discrepancy detected:</p>
                      <p>Transaction amount exceeds intended amount by 15%</p>
                    </div>
                  )}
                </CardContent>
                <CardFooter className="flex justify-end gap-2">
                  {intent.status === "disputed" && (
                    <>
                      <Button variant="destructive" size="sm">
                        Undo Transaction
                      </Button>
                      <Button 
                        variant="outline" 
                        size="sm"
                        asChild
                      >
                        <Link href="/disputes">Submit Dispute</Link>
                      </Button>
                    </>
                  )}
                  <Button variant="outline" size="sm">
                    View Details
                  </Button>
                </CardFooter>
              </Card>
            ))}
          </div>
        )}
      </div>
      
      {/* Modals */}
      <IntentModal
        isOpen={isIntentModalOpen}
        onClose={() => setIsIntentModalOpen(false)}
        onConfirm={handleConfirmIntent}
        onReject={handleRejectIntent}
        txType={currentIntent?.txType || ""}
        payload={currentIntent?.payload || {}}
      />
      
      <WalletConnectModal
        isOpen={isWalletModalOpen}
        onClose={() => setIsWalletModalOpen(false)}
      />
    </div>
  );
}