import React, { useState, useEffect } from 'react';
import { useWallet } from '@/context/wallet-context';
import { BridgeService } from '@/services/bridge-service';
import { CHAIN_IDS } from '@/lib/blockchain/providers';
import { getChainName } from '@/lib/utils/blockchain';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from '@/components/ui/card';
import { Label } from '@/components/ui/label';
import { Loader2 } from 'lucide-react';

const BridgeForm: React.FC = () => {
  const { wallet } = useWallet();
  const [sourceChain, setSourceChain] = useState<number | null>(null);
  const [targetChain, setTargetChain] = useState<number | null>(null);
  const [amount, setAmount] = useState<string>('');
  const [token, setToken] = useState<string>('');
  const [targetAddress, setTargetAddress] = useState<string>('');
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [txHash, setTxHash] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  
  const bridgeService = BridgeService.getInstance();
  
  // Set default source chain based on connected wallet
  useEffect(() => {
    if (wallet?.chainId) {
      setSourceChain(wallet.chainId);
      
      // Set default target chain
      if (wallet.chainId === CHAIN_IDS.AVALANCHE_FUJI || wallet.chainId === CHAIN_IDS.AVALANCHE_MAINNET) {
        setTargetChain(CHAIN_IDS.CARDANO_TESTNET);
      } else if (wallet.chainId === CHAIN_IDS.CARDANO_TESTNET || wallet.chainId === CHAIN_IDS.CARDANO_MAINNET) {
        setTargetChain(CHAIN_IDS.AVALANCHE_FUJI);
      }
    }
  }, [wallet]);
  
  // Available chains for source and target
  const availableSourceChains = [
    CHAIN_IDS.AVALANCHE_FUJI,
    CHAIN_IDS.AVALANCHE_MAINNET,
    CHAIN_IDS.CARDANO_TESTNET,
    CHAIN_IDS.CARDANO_MAINNET,
  ];
  
  const availableTargetChains = (sourceChain: number | null) => {
    if (sourceChain === CHAIN_IDS.AVALANCHE_FUJI || sourceChain === CHAIN_IDS.AVALANCHE_MAINNET) {
      return [CHAIN_IDS.CARDANO_TESTNET, CHAIN_IDS.CARDANO_MAINNET];
    } else if (sourceChain === CHAIN_IDS.CARDANO_TESTNET || sourceChain === CHAIN_IDS.CARDANO_MAINNET) {
      return [CHAIN_IDS.AVALANCHE_FUJI, CHAIN_IDS.AVALANCHE_MAINNET];
    }
    return [];
  };
  
  // Available tokens based on source chain
  const availableTokens = (sourceChain: number | null) => {
    if (sourceChain === CHAIN_IDS.AVALANCHE_FUJI || sourceChain === CHAIN_IDS.AVALANCHE_MAINNET) {
      return ['AVAX', 'USDC', 'USDT', 'WETH', 'WBTC'];
    } else if (sourceChain === CHAIN_IDS.CARDANO_TESTNET || sourceChain === CHAIN_IDS.CARDANO_MAINNET) {
      return ['ADA', 'USDC', 'USDT', 'DJED'];
    }
    return [];
  };
  
  const handleSourceChainChange = (value: string) => {
    const chainId = parseInt(value);
    setSourceChain(chainId);
    
    // Reset target chain if it's the same as source
    if (targetChain === chainId) {
      setTargetChain(null);
    }
    
    // Reset token if it's not available on the new source chain
    if (token && !availableTokens(chainId).includes(token)) {
      setToken('');
    }
  };
  
  const handleTargetChainChange = (value: string) => {
    setTargetChain(parseInt(value));
  };
  
  const handleTokenChange = (value: string) => {
    setToken(value);
  };
  
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!wallet || !sourceChain || !targetChain || !amount || !token) {
      setError('Please fill in all fields');
      return;
    }
    
    setIsLoading(true);
    setError(null);
    setTxHash(null);
    
    try {
      let hash: string;
      
      // Bridge from Avalanche to Cardano
      if (sourceChain === CHAIN_IDS.AVALANCHE_FUJI || sourceChain === CHAIN_IDS.AVALANCHE_MAINNET) {
        if (!wallet.provider) {
          throw new Error('No provider available');
        }
        
        hash = await bridgeService.bridgeFromAvalancheToCardano(
          wallet.address,
          targetAddress || wallet.address, // Use input address or wallet address
          amount,
          token,
          wallet.provider
        );
      } 
      // Bridge from Cardano to Avalanche
      else if (sourceChain === CHAIN_IDS.CARDANO_TESTNET || sourceChain === CHAIN_IDS.CARDANO_MAINNET) {
        if (!wallet.provider) {
          throw new Error('No provider available');
        }
        
        hash = await bridgeService.bridgeFromCardanoToAvalanche(
          wallet.address,
          targetAddress || wallet.address, // Use input address or wallet address
          amount,
          token,
          wallet.provider
        );
      } else {
        throw new Error('Unsupported source chain');
      }
      
      setTxHash(hash);
    } catch (err) {
      console.error('Bridge error:', err);
      setError(err instanceof Error ? err.message : 'An unknown error occurred');
    } finally {
      setIsLoading(false);
    }
  };
  
  return (
    <Card className="w-full max-w-md mx-auto">
      <CardHeader>
        <CardTitle>Cross-Chain Bridge</CardTitle>
        <CardDescription>Transfer assets between Avalanche and Cardano</CardDescription>
      </CardHeader>
      <CardContent>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="sourceChain">Source Chain</Label>
            <Select
              value={sourceChain?.toString()}
              onValueChange={handleSourceChainChange}
            >
              <SelectTrigger id="sourceChain">
                <SelectValue placeholder="Select source chain" />
              </SelectTrigger>
              <SelectContent>
                {availableSourceChains.map((chainId) => (
                  <SelectItem key={chainId} value={chainId.toString()}>
                    {getChainName(chainId)}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          
          <div className="space-y-2">
            <Label htmlFor="targetChain">Target Chain</Label>
            <Select
              value={targetChain?.toString()}
              onValueChange={handleTargetChainChange}
              disabled={!sourceChain}
            >
              <SelectTrigger id="targetChain">
                <SelectValue placeholder="Select target chain" />
              </SelectTrigger>
              <SelectContent>
                {availableTargetChains(sourceChain).map((chainId) => (
                  <SelectItem key={chainId} value={chainId.toString()}>
                    {getChainName(chainId)}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          
          <div className="space-y-2">
            <Label htmlFor="token">Token</Label>
            <Select
              value={token}
              onValueChange={handleTokenChange}
              disabled={!sourceChain}
            >
              <SelectTrigger id="token">
                <SelectValue placeholder="Select token" />
              </SelectTrigger>
              <SelectContent>
                {availableTokens(sourceChain).map((tokenSymbol) => (
                  <SelectItem key={tokenSymbol} value={tokenSymbol}>
                    {tokenSymbol}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          
          <div className="space-y-2">
            <Label htmlFor="amount">Amount</Label>
            <Input
              id="amount"
              type="number"
              placeholder="0.0"
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              min="0"
              step="0.000001"
              required
            />
          </div>
          
          <div className="space-y-2">
            <Label htmlFor="targetAddress">Target Address (Optional)</Label>
            <Input
              id="targetAddress"
              type="text"
              placeholder="Enter recipient address"
              value={targetAddress}
              onChange={(e) => setTargetAddress(e.target.value)}
            />
            <p className="text-xs text-gray-500">
              Leave empty to use your current wallet address
            </p>
          </div>
          
          {error && (
            <div className="p-3 bg-red-100 border border-red-300 rounded-md text-red-800 text-sm">
              {error}
            </div>
          )}
          
          {txHash && (
            <div className="p-3 bg-green-100 border border-green-300 rounded-md text-green-800 text-sm">
              Transaction submitted! Hash: {txHash.slice(0, 10)}...{txHash.slice(-8)}
            </div>
          )}
        </form>
      </CardContent>
      <CardFooter>
        <Button 
          type="submit" 
          className="w-full" 
          disabled={isLoading || !sourceChain || !targetChain || !amount || !token || !wallet}
          onClick={handleSubmit}
        >
          {isLoading ? (
            <>
              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              Processing...
            </>
          ) : (
            'Bridge Assets'
          )}
        </Button>
      </CardFooter>
    </Card>
  );
};

export default BridgeForm;