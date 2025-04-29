import React from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Select } from "@/components/ui/select";

export default function TransactionsPage() {
  return (
    <div className="container mx-auto py-10 max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Transaction Dashboard</h1>
      
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
            <form className="space-y-4">
              <div className="space-y-2">
                <label htmlFor="txType" className="text-sm font-medium">
                  Transaction Type
                </label>
                <Select id="txType" defaultValue="swap">
                  <option value="swap">Token Swap</option>
                  <option value="transfer">Token Transfer</option>
                  <option value="stake">Stake Tokens</option>
                  <option value="mint">Mint NFT</option>
                </Select>
              </div>
              
              {/* Swap Form Fields (default view) */}
              <div className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <label htmlFor="tokenIn" className="text-sm font-medium">
                      From Token
                    </label>
                    <Select id="tokenIn" defaultValue="usdc">
                      <option value="usdc">USDC</option>
                      <option value="avax">AVAX</option>
                      <option value="eth">ETH</option>
                    </Select>
                  </div>
                  <div className="space-y-2">
                    <label htmlFor="tokenOut" className="text-sm font-medium">
                      To Token
                    </label>
                    <Select id="tokenOut" defaultValue="avax">
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
                  <Input id="amount" type="text" placeholder="0.0" />
                </div>
                
                <div className="space-y-2">
                  <label htmlFor="slippage" className="text-sm font-medium">
                    Slippage Tolerance (%)
                  </label>
                  <Input id="slippage" type="text" placeholder="1.0" />
                </div>
              </div>
              
              <Button type="button" variant="gradient" className="w-full">
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
              Review your transaction intent before confirming
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="p-4 bg-muted rounded-lg">
              <p className="text-lg font-medium">
                Swap 100 USDC to AVAX with 2% slippage
              </p>
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Intent ID:</span>
                <span className="text-sm font-mono">#823989</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Status:</span>
                <span className="text-sm font-medium text-green-500">PENDING</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Created:</span>
                <span className="text-sm">Just now</span>
              </div>
            </div>
          </CardContent>
          <CardFooter className="flex flex-col gap-2">
            <Button variant="default" className="w-full">
              Confirm Intent
            </Button>
            <Button variant="outline" className="w-full">
              Reject Intent
            </Button>
          </CardFooter>
        </Card>
      </div>
      
      {/* Recent Transactions */}
      <div className="mt-10">
        <h2 className="text-2xl font-bold mb-4">Recent Transactions</h2>
        <div className="space-y-4">
          {/* Transaction Item */}
          <Card>
            <CardHeader className="pb-2">
              <div className="flex justify-between">
                <CardTitle className="text-lg">Token Swap</CardTitle>
                <span className="text-sm text-green-500 font-medium">MATCHED</span>
              </div>
              <CardDescription>
                Intent ID: #823988 • 2 hours ago
              </CardDescription>
            </CardHeader>
            <CardContent className="pb-2">
              <p>Swapped 50 USDC to 0.25 AVAX with 1% slippage</p>
            </CardContent>
            <CardFooter className="flex justify-end gap-2">
              <Button variant="outline" size="sm">
                View Details
              </Button>
            </CardFooter>
          </Card>
          
          {/* Transaction Item with Dispute */}
          <Card>
            <CardHeader className="pb-2">
              <div className="flex justify-between">
                <CardTitle className="text-lg">NFT Purchase</CardTitle>
                <span className="text-sm text-red-500 font-medium">DISPUTED</span>
              </div>
              <CardDescription>
                Intent ID: #823975 • 1 day ago
              </CardDescription>
            </CardHeader>
            <CardContent className="pb-2">
              <p>Purchased CryptoArt #1234 for 0.5 ETH</p>
              <div className="mt-2 p-2 bg-red-500/10 rounded text-sm text-red-500">
                <p className="font-medium">Discrepancy detected:</p>
                <p>Paid 1.5 ETH instead of intended 0.5 ETH</p>
              </div>
            </CardContent>
            <CardFooter className="flex justify-end gap-2">
              <Button variant="destructive" size="sm">
                Undo Transaction
              </Button>
              <Button variant="outline" size="sm">
                Submit Dispute
              </Button>
              <Button variant="outline" size="sm">
                View Details
              </Button>
            </CardFooter>
          </Card>
        </div>
      </div>
    </div>
  );
}