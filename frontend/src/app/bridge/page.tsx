import React from 'react';
import { Metadata } from 'next';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { InfoIcon } from 'lucide-react';

export const metadata: Metadata = {
  title: 'Cross-Chain Bridge | Phoenix Protocol',
  description: 'Bridge assets and intents between different blockchains',
};

export default function BridgePage() {
  return (
    <div className="container mx-auto py-8">
      <div className="max-w-5xl mx-auto">
        <h1 className="text-3xl font-bold mb-6">Cross-Chain Bridge</h1>
        
        <Alert className="mb-6">
          <InfoIcon className="h-4 w-4" />
          <AlertTitle>Bridge Information</AlertTitle>
          <AlertDescription>
            The Phoenix Protocol bridge allows you to transfer assets and synchronize intents between different blockchains.
            This creates a unified experience across multiple chains.
          </AlertDescription>
        </Alert>
        
        <Tabs defaultValue="assets" className="mb-8">
          <TabsList className="grid w-full grid-cols-2">
            <TabsTrigger value="assets">Asset Bridge</TabsTrigger>
            <TabsTrigger value="intents">Intent Synchronization</TabsTrigger>
          </TabsList>
          
          <TabsContent value="assets" className="mt-4">
            <div className="grid md:grid-cols-2 gap-8">
              <div>
                <h2 className="text-xl font-semibold mb-4">Bridge Assets</h2>
                <p className="text-gray-600 mb-4">
                  Transfer your assets between Ethereum and Avalanche blockchains securely.
                  The bridge uses secure technology to ensure safe and efficient cross-chain transfers.
                </p>
                
                <h3 className="text-lg font-medium mt-6 mb-2">Supported Assets</h3>
                <ul className="list-disc list-inside space-y-1 text-gray-600">
                  <li>ETH (Ethereum) ↔ AVAX (Avalanche)</li>
                  <li>USDC (Ethereum) ↔ USDC (Avalanche)</li>
                  <li>USDT (Ethereum) ↔ USDT (Avalanche)</li>
                  <li>WBTC (Ethereum) ↔ BTC.b (Avalanche)</li>
                </ul>
                
                <h3 className="text-lg font-medium mt-6 mb-2">Bridge Fees</h3>
                <p className="text-gray-600">
                  Bridge fees are calculated based on network conditions and gas prices.
                  The current fee is approximately 0.1% of the transaction amount.
                </p>
              </div>
              
              <div>
                <Card>
                  <CardHeader>
                    <CardTitle>Bridge Coming Soon</CardTitle>
                    <CardDescription>
                      Our cross-chain bridge is currently under development
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="bg-amber-50 border border-amber-200 rounded-md p-4">
                      <h3 className="text-amber-800 font-medium">Coming Soon</h3>
                      <p className="text-amber-700 text-sm mt-1">
                        The bridge functionality is currently under development and will be available in Q3 2025.
                        Stay tuned for updates!
                      </p>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </div>
          </TabsContent>
          
          <TabsContent value="intents" className="mt-4">
            <Card>
              <CardHeader>
                <CardTitle>Intent Synchronization</CardTitle>
                <CardDescription>
                  Synchronize your transaction intents across different blockchains
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <p>
                    Intent synchronization allows you to create a transaction intent on one blockchain
                    and have it automatically synchronized to another blockchain. This ensures that
                    your recovery and protection mechanisms work seamlessly across multiple chains.
                  </p>
                  
                  <div className="bg-amber-50 border border-amber-200 rounded-md p-4">
                    <h3 className="text-amber-800 font-medium">Coming Soon</h3>
                    <p className="text-amber-700 text-sm mt-1">
                      Intent synchronization is currently under development and will be available in Q3 2025.
                      Stay tuned for updates!
                    </p>
                  </div>
                  
                  <h3 className="text-lg font-medium mt-2">How It Works</h3>
                  <ol className="list-decimal list-inside space-y-2 text-gray-600">
                    <li>Create a transaction intent on either Ethereum or Avalanche</li>
                    <li>The intent is stored in the Intent Registry on the source blockchain</li>
                    <li>The bridge service detects the new intent and creates a corresponding intent on the target blockchain</li>
                    <li>Both intents are linked through a unique identifier</li>
                    <li>Any updates to the intent (execution, cancellation) are synchronized across both chains</li>
                  </ol>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
        
        <div className="grid md:grid-cols-2 gap-8 mt-8">
          <Card>
            <CardHeader>
              <CardTitle>Ethereum Integration</CardTitle>
              <CardDescription>
                Phoenix Protocol on Ethereum
              </CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-gray-600 mb-4">
                The Ethereum implementation uses Solidity smart contracts.
                It leverages Ethereum's security and wide adoption for robust transaction processing.
              </p>
              
              <h3 className="text-sm font-medium mb-2">Contract Addresses</h3>
              <div className="space-y-2 text-xs">
                <div className="flex justify-between">
                  <span className="text-gray-500">Phoenix Protocol:</span>
                  <span className="font-mono">0x7c2C195CD6D34B8F845992d380aADB2730bB9C6F</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Intent Registry:</span>
                  <span className="font-mono">0x8A791620dd6260079BF849Dc5567aDC3F2FdC318</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Recovery Module:</span>
                  <span className="font-mono">0xB0B195aEFA3650A6908f15CdaC7D92F8a5791B0B</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Token Vault:</span>
                  <span className="font-mono">0x3A5325F0E5Ee4da7dBD7e37615b53FA362AAFd22</span>
                </div>
              </div>
            </CardContent>
          </Card>
          
          <Card>
            <CardHeader>
              <CardTitle>Avalanche Integration</CardTitle>
              <CardDescription>
                Phoenix Protocol on Avalanche C-Chain
              </CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-gray-600 mb-4">
                The Avalanche implementation uses Solidity smart contracts deployed on the C-Chain.
                It leverages Avalanche's high throughput and low latency for fast transaction processing.
              </p>
              
              <h3 className="text-sm font-medium mb-2">Contract Addresses</h3>
              <div className="space-y-2 text-xs">
                <div className="flex justify-between">
                  <span className="text-gray-500">Phoenix Protocol:</span>
                  <span className="font-mono">0x9D7f74d0C41E726EC95884E0e97Fa6129e3b5E99</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Intent Registry:</span>
                  <span className="font-mono">0xF8A0BF9cF54Bb92F17374d9e9A321E6a111a51bD</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Recovery Module:</span>
                  <span className="font-mono">0x2170Ed0880ac9A755fd29B2688956BD959F933F8</span>
                </div>
              </div>
              
              <div className="bg-blue-50 border border-blue-200 rounded-md p-3 mt-4">
                <p className="text-blue-700 text-xs">
                  The Avalanche implementation is currently in development and will be fully deployed in Q3 2025.
                  The addresses above are for the testnet deployment.
                </p>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}