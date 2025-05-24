"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Select } from "@/components/ui/select";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Alert } from "@/components/ui/alert";
import { useWallet } from "@/context/wallet-context";
import { useTransactionIntent } from "@/hooks/use-transaction-intent";
import { IntentModal } from "@/components/forms/intent-modal";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { createIntentDescription } from "@/lib/utils/blockchain";
import { IntentRegistry } from "@/components/intent/intent-registry";
import { TransactionVerifier } from "@/components/intent/transaction-verifier";
import { Info, AlertTriangle } from "lucide-react";
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
        setErrorMessage(`Transaction verification failed: ${result.discrepancies ? result.discrepancies.join(", ") : "Unknown error"}`);
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
    <div className="container mx-auto py-10 max-w-6xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-3xl font-bold">Transaction Dashboard</h1>
        <div className="flex gap-2">
          <Link href="/disputes">
            <Button variant="outline">
              Dispute Center
            </Button>
          </Link>
        </div>
      </div>
      
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
      
      <Alert className="bg-blue-500/10 border-blue-500/20 mb-6">
        <div className="flex items-start gap-2">
          <Info className="h-4 w-4 text-blue-500 mt-0.5" />
          <div>
            <p className="text-sm font-medium text-blue-500">Intent-Based Transaction Protection</p>
            <p className="text-xs text-muted-foreground">
              Phoenix Protocol uses an intent-based verification system to protect your transactions. 
              Before execution, your transaction intent is recorded on-chain, allowing the system to verify 
              that the executed transaction matches your original intent. This protects you from malicious 
              frontends, phishing attacks, and accidental transactions.
            </p>
          </div>
        </div>
      </Alert>
      
      <Tabs defaultValue="create" className="w-full">
        <TabsList className="grid grid-cols-3 mb-6">
          <TabsTrigger value="create">Create Transaction</TabsTrigger>
          <TabsTrigger value="registry">Intent Registry</TabsTrigger>
          <TabsTrigger value="verify">Verify Transaction</TabsTrigger>
        </TabsList>
        
        <TabsContent value="create">
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
                    <Alert className="bg-yellow-500/10 border-yellow-500/20 mb-4">
                      <div className="flex items-start gap-2">
                        <AlertTriangle className="h-4 w-4 text-yellow-500 mt-0.5" />
                        <div>
                          <p className="text-sm font-medium text-yellow-500">Important</p>
                          <p className="text-xs text-muted-foreground">
                            By confirming this intent, you're creating an on-chain record of what you want to do.
                            This helps protect you from malicious transactions and enables recovery if something goes wrong.
                          </p>
                        </div>
                      </div>
                    </Alert>
                    
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
                    
                    <div className="flex gap-2 pt-4">
                      <Button 
                        variant="outline" 
                        className="flex-1"
                        onClick={handleRejectIntent}
                      >
                        Reject
                      </Button>
                      <Button 
                        variant="gradient" 
                        className="flex-1"
                        onClick={handleConfirmIntent}
                      >
                        Confirm
                      </Button>
                    </div>
                  </>
                ) : (
                  <div className="flex flex-col items-center justify-center py-8 text-center">
                    <p className="text-muted-foreground mb-4">
                      No active intent. Create a new transaction to get started.
                    </p>
                    <p className="text-xs text-muted-foreground max-w-xs">
                      All transactions in Phoenix Protocol start with an intent, which is a record of what you want to do.
                      This helps protect you from malicious transactions.
                    </p>
                  </div>
                )}
              </CardContent>
            </Card>
          </div>
          
          {/* Recent Transactions */}
          <div className="mt-10">
            <h2 className="text-2xl font-bold mb-4">Recent Transactions</h2>
            
            {intents.length === 0 ? (
              <Card>
                <CardContent className="py-8 text-center">
                  <p className="text-muted-foreground">No transactions yet</p>
                </CardContent>
              </Card>
            ) : (
              <div className="space-y-4">
                {intents.map((intent) => (
                  <Card key={intent.id}>
                    <CardHeader className="pb-2">
                      <div className="flex justify-between items-center">
                        <CardTitle className="text-base">
                          {createIntentDescription(intent.txType, intent.payload)}
                        </CardTitle>
                        <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                          intent.executed ? "bg-green-500/10 text-green-500" : 
                          intent.rejected ? "bg-red-500/10 text-red-500" : 
                          "bg-yellow-500/10 text-yellow-500"
                        }`}>
                          {intent.executed ? "EXECUTED" : intent.rejected ? "REJECTED" : "PENDING"}
                        </span>
                      </div>
                      <CardDescription>
                        {formatTimeAgo(intent.timestamp)}
                      </CardDescription>
                    </CardHeader>
                    <CardContent>
                      <div className="text-sm space-y-1">
                        <div className="flex justify-between">
                          <span className="text-muted-foreground">Intent ID:</span>
                          <span className="font-mono">{intent.id}</span>
                        </div>
                        {intent.executed && intent.txHash && (
                          <div className="flex justify-between">
                            <span className="text-muted-foreground">Transaction Hash:</span>
                            <span className="font-mono">{intent.txHash}</span>
                          </div>
                        )}
                        {intent.executed && intent.verification && (
                          <div className="flex justify-between">
                            <span className="text-muted-foreground">Verification:</span>
                            <span className={intent.verification.success ? "text-green-500" : "text-red-500"}>
                              {intent.verification.success ? "MATCHED" : "MISMATCH"}
                            </span>
                          </div>
                        )}
                      </div>
                    </CardContent>
                    <CardFooter>
                      {intent.executed && intent.verification && !intent.verification.success && (
                        <div className="w-full">
                          <div className="p-3 bg-red-500/10 rounded-md mb-3">
                            <p className="text-sm text-red-500 font-medium">Transaction does not match intent!</p>
                            <p className="text-xs text-red-500">
                              {intent.verification.discrepancies?.join(", ")}
                            </p>
                          </div>
                          <div className="flex justify-end">
                            <Link href="/disputes">
                              <Button variant="outline" size="sm">
                                Dispute Transaction
                              </Button>
                            </Link>
                          </div>
                        </div>
                      )}
                      
                      {intent.executed && intent.verification && intent.verification.success && (
                        <div className="w-full flex justify-end">
                          <Button variant="ghost" size="sm">
                            View Details
                          </Button>
                        </div>
                      )}
                      
                      {!intent.executed && !intent.rejected && (
                        <div className="w-full flex justify-end gap-2">
                          <Button 
                            variant="outline" 
                            size="sm"
                            onClick={() => {
                              // Mock execution for demo purposes
                              simulateTransactionExecution(intent.id);
                            }}
                          >
                            Execute
                          </Button>
                          <Button variant="ghost" size="sm">
                            Cancel
                          </Button>
                        </div>
                      )}
                    </CardFooter>
                  </Card>
                ))}
              </div>
            )}
          </div>
        </TabsContent>
        
        <TabsContent value="registry">
          <IntentRegistry />
        </TabsContent>
        
        <TabsContent value="verify">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            <div>
              <TransactionVerifier />
            </div>
            
            <div className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>How Intent Verification Works</CardTitle>
                  <CardDescription>
                    Understanding the Phoenix Protocol security model
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="space-y-4">
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">1</div>
                      <div>
                        <h3 className="text-sm font-medium">Intent Registration</h3>
                        <p className="text-sm text-muted-foreground">
                          Before any transaction is executed, your intent (what you want to do) is recorded on-chain with a unique ID.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">2</div>
                      <div>
                        <h3 className="text-sm font-medium">Transaction Execution</h3>
                        <p className="text-sm text-muted-foreground">
                          When the transaction is executed, it references the intent ID, creating a verifiable link.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">3</div>
                      <div>
                        <h3 className="text-sm font-medium">Automatic Verification</h3>
                        <p className="text-sm text-muted-foreground">
                          The system automatically compares the executed transaction against the original intent to detect any discrepancies.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">4</div>
                      <div>
                        <h3 className="text-sm font-medium">Recovery Options</h3>
                        <p className="text-sm text-muted-foreground">
                          If a mismatch is found, you can submit a dispute to the DAO for review and potential rollback.
                        </p>
                      </div>
                    </div>
                  </div>
                  
                  <Alert className="bg-green-500/10 border-green-500/20 text-green-500">
                    <p className="text-sm">
                      This process protects you from malicious frontends, phishing attacks, and accidental transactions.
                    </p>
                  </Alert>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader>
                  <CardTitle>Common Verification Issues</CardTitle>
                  <CardDescription>
                    Problems that can be detected and resolved
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="p-3 bg-muted rounded-md">
                      <p className="text-sm font-medium">Price Slippage Exceeded</p>
                      <p className="text-xs text-muted-foreground">
                        When a swap executes at a price significantly different from what you intended
                      </p>
                    </div>
                    
                    <div className="p-3 bg-muted rounded-md">
                      <p className="text-sm font-medium">Wrong Amount Transferred</p>
                      <p className="text-xs text-muted-foreground">
                        When the amount sent differs from what you specified in your intent
                      </p>
                    </div>
                    
                    <div className="p-3 bg-muted rounded-md">
                      <p className="text-sm font-medium">Incorrect Recipient</p>
                      <p className="text-xs text-muted-foreground">
                        When funds are sent to a different address than you intended
                      </p>
                    </div>
                    
                    <div className="p-3 bg-muted rounded-md">
                      <p className="text-sm font-medium">Unauthorized Transaction</p>
                      <p className="text-xs text-muted-foreground">
                        When a transaction executes without a corresponding intent
                      </p>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
          </div>
        </TabsContent>
      </Tabs>
      
      {currentIntent && (
        <IntentModal 
          isOpen={isIntentModalOpen} 
          onClose={() => setIsIntentModalOpen(false)}
          onConfirm={handleConfirmIntent}
          onReject={handleRejectIntent}
          txType={currentIntent.txType}
          payload={currentIntent.payload}
        />
      )}
      
      <WalletConnectModal 
        isOpen={isWalletModalOpen} 
        onClose={() => setIsWalletModalOpen(false)} 
      />
    </div>
  );
}