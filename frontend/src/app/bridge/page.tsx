import React from 'react';
import { Metadata } from 'next';
import BridgeForm from '@/components/bridge/bridge-form';
import TransactionHistory from '@/components/bridge/transaction-history';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { InfoIcon } from 'lucide-react';

export const metadata: Metadata = {
  title: 'Cross-Chain Bridge | Phoenix Protocol',
  description: 'Bridge assets and intents between Avalanche and Cardano blockchains',
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
            The Phoenix Protocol bridge allows you to transfer assets and synchronize intents between Avalanche and Cardano blockchains.
            This creates a unified experience across both chains.
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
                  Transfer your assets between Avalanche and Cardano blockchains securely.
                  The bridge uses Milkomeda technology to ensure safe and efficient cross-chain transfers.
                </p>
                
                <h3 className="text-lg font-medium mt-6 mb-2">Supported Assets</h3>
                <ul className="list-disc list-inside space-y-1 text-gray-600">
                  <li>AVAX (Avalanche) ↔ ADA (Cardano)</li>
                  <li>USDC (Avalanche) ↔ USDC (Cardano)</li>
                  <li>USDT (Avalanche) ↔ USDT (Cardano)</li>
                  <li>WETH (Avalanche) ↔ ETH (Cardano via Milkomeda)</li>
                  <li>WBTC (Avalanche) ↔ BTC (Cardano via Milkomeda)</li>
                </ul>
                
                <h3 className="text-lg font-medium mt-6 mb-2">Bridge Fees</h3>
                <p className="text-gray-600">
                  Bridge fees are calculated based on network conditions and gas prices.
                  The current fee is approximately 0.1% of the transaction amount.
                </p>
              </div>
              
              <div>
                <BridgeForm />
              </div>
            </div>
            
            <div className="mt-8">
              <TransactionHistory />
            </div>
          </TabsContent>
          
          <TabsContent value="intents" className="mt-4">
            <Card>
              <CardHeader>
                <CardTitle>Intent Synchronization</CardTitle>
                <CardDescription>
                  Synchronize your transaction intents across Avalanche and Cardano blockchains
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <p>
                    Intent synchronization allows you to create a transaction intent on one blockchain
                    and have it automatically synchronized to the other blockchain. This ensures that
                    your recovery and protection mechanisms work seamlessly across both chains.
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
                    <li>Create a transaction intent on either Avalanche or Cardano</li>
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
              <CardTitle>Cardano Integration</CardTitle>
              <CardDescription>
                Phoenix Protocol on Cardano
              </CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-gray-600 mb-4">
                The Cardano implementation uses Plutus scripts deployed on the Cardano blockchain.
                It leverages Cardano's formal verification and UTXO model for enhanced security.
              </p>
              
              <h3 className="text-sm font-medium mb-2">Script Addresses</h3>
              <div className="space-y-2 text-xs">
                <div className="flex justify-between">
                  <span className="text-gray-500">Intent Registry:</span>
                  <span className="font-mono">addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-500">Recovery Module:</span>
                  <span className="font-mono">addr1w9lzn5ygc6qvlsz4n0tzmw8kv4ym6yssvjyqx0f3qfln6tcyjy7wx</span>
                </div>
              </div>
              
              <div className="bg-blue-50 border border-blue-200 rounded-md p-3 mt-4">
                <p className="text-blue-700 text-xs">
                  The Cardano implementation is currently in development and will be fully deployed in Q3 2025.
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