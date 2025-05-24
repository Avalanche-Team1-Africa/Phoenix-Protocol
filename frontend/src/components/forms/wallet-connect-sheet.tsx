"use client";

import React, { useState } from "react";
import Image from "next/image";
import { useWallet, WalletType } from "@/context/wallet-context";
import { BottomSheet } from "@/components/ui/bottom-sheet";
import { formatAddress } from "@/lib/utils/blockchain";
import { Loader2, AlertCircle } from "lucide-react";
import { Button } from "@/components/ui/button";

interface WalletOption {
  id: WalletType;
  name: string;
  icon: string;
  description: string;
}

const walletOptions: WalletOption[] = [
  {
    id: "metamask",
    name: "MetaMask",
    icon: "/wallets/metamask.svg",
    description: "Connect to your MetaMask Wallet",
  },
  {
    id: "core",
    name: "Core Wallet",
    icon: "/wallets/core.svg",
    description: "Avalanche's Core Wallet Extension",
  },
  {
    id: "coinbase",
    name: "Coinbase Wallet",
    icon: "/wallets/coinbase.svg",
    description: "Connect with Coinbase Wallet",
  },
  {
    id: "walletconnect",
    name: "WalletConnect",
    icon: "/wallets/walletconnect.svg",
    description: "Scan with any mobile wallet",
  },
  {
    id: "trust",
    name: "Trust Wallet",
    icon: "/wallets/trust.svg",
    description: "Connect to Trust Wallet",
  },
  {
    id: "phantom",
    name: "Phantom",
    icon: "/wallets/phantom.svg",
    description: "Solana's Phantom Wallet",
  },
];

interface WalletConnectSheetProps {
  isOpen: boolean;
  onClose: () => void;
}

export function WalletConnectSheet({ isOpen, onClose }: WalletConnectSheetProps) {
  const { connect, wallet, disconnect, isConnecting, error } = useWallet();

  const [connectingWallet, setConnectingWallet] = useState<WalletType | null>(null);
  const [rememberWallet, setRememberWallet] = useState<boolean>(
    localStorage.getItem("phoenixAutoConnect") === "true"
  );

  const handleConnect = async (walletType: WalletType) => {
    setConnectingWallet(walletType);
    try {
      const success = await connect(walletType);
      if (success) {
        // Save auto-connect preference
        if (rememberWallet) {
          localStorage.setItem("phoenixAutoConnect", "true");
        } else {
          localStorage.removeItem("phoenixAutoConnect");
        }
        onClose();
      }
    } finally {
      setConnectingWallet(null);
    }
  };
  
  const toggleRememberWallet = () => {
    setRememberWallet(!rememberWallet);
  };

  return (
    <BottomSheet 
      isOpen={isOpen} 
      onClose={onClose}
      title="Connect Wallet"
      height="auto"
    >
      {error && (
        <div className="mb-4 rounded-lg bg-red-500/10 p-3 text-red-500">
          <div className="flex items-start">
            <AlertCircle className="mr-2 mt-0.5 h-4 w-4 flex-shrink-0" />
            <div>
              <p className="text-sm font-medium">{error}</p>
              {error.includes("not installed") && (
                <p className="mt-1 text-xs">
                  Click the wallet option again to visit the download page.
                </p>
              )}
            </div>
          </div>
        </div>
      )}

      {wallet.connected ? (
        <div className="space-y-4">
          <div className="rounded-lg border p-4">
            <div className="flex items-center mb-4">
              <div className="mr-3 flex h-10 w-10 items-center justify-center rounded-full bg-muted">
                <Image
                  src={`/wallets/${wallet.walletType || 'metamask'}.svg`}
                  alt={wallet.walletType || 'wallet'}
                  width={24}
                  height={24}
                />
              </div>
              <div>
                <h4 className="font-medium capitalize">{wallet.walletType} Wallet</h4>
                <p className="text-xs text-muted-foreground">Connected</p>
              </div>
            </div>
            
            <div className="space-y-3">
              <div className="rounded-md bg-muted/30 p-3">
                <div className="flex justify-between items-center mb-1">
                  <span className="text-sm text-muted-foreground">Address</span>
                  <button 
                    className="text-xs text-orange-500"
                    onClick={() => navigator.clipboard.writeText(wallet.address)}
                  >
                    Copy
                  </button>
                </div>
                <div className="font-mono text-sm">{formatAddress(wallet.address, 12)}</div>
              </div>
              
              <div className="rounded-md bg-muted/30 p-3">
                <div className="flex justify-between items-center mb-1">
                  <span className="text-sm text-muted-foreground">Balance</span>
                  <span className="text-xs text-orange-500">Native Token</span>
                </div>
                <div className="font-medium">
                  {parseFloat(wallet.balance).toFixed(4)} 
                  {wallet.chainId === 43114 || wallet.chainId === 43113 ? " AVAX" : " ETH"}
                </div>
              </div>
              
              <div className="rounded-md bg-muted/30 p-3">
                <div className="flex justify-between items-center mb-1">
                  <span className="text-sm text-muted-foreground">Network</span>
                  <span className="text-xs px-2 py-0.5 bg-green-500/10 text-green-500 rounded-full">Connected</span>
                </div>
                <div className="font-medium">
                  {wallet.chainId === 1 ? "Ethereum Mainnet" : 
                   wallet.chainId === 11155111 ? "Sepolia Testnet" :
                   wallet.chainId === 43114 ? "Avalanche C-Chain" : 
                   wallet.chainId === 43113 ? "Avalanche Fuji Testnet" :
                   `Chain ID: ${wallet.chainId}`}
                </div>
              </div>
            </div>
          </div>
          
          <div className="flex space-x-3">
            <Button
              variant="outline"
              className="flex-1"
              onClick={disconnect}
            >
              Disconnect
            </Button>
            <Button
              className="flex-1 bg-orange-500 hover:bg-orange-600 text-white"
              onClick={onClose}
            >
              Continue
            </Button>
          </div>
        </div>
      ) : (
        <div className="space-y-6">
          <div className="grid grid-cols-2 gap-3">
            {walletOptions.map((option, index) => {
              const isConnectingThis = connectingWallet === option.id;
              return (
                <button
                  key={option.id}
                  className={`flex flex-col items-center rounded-lg border p-3 ${
                    isConnectingThis 
                      ? 'border-orange-500' 
                      : 'border-muted-foreground/30'
                  } transition-all ${
                    isConnecting && !isConnectingThis ? 'opacity-50 cursor-not-allowed' : ''
                  }`}
                  onClick={() => handleConnect(option.id)}
                  disabled={isConnecting}
                >
                  <div className="relative mb-2 flex h-10 w-10 items-center justify-center">
                    {isConnectingThis && (
                      <div className="absolute inset-0 flex items-center justify-center">
                        <Loader2 className="h-10 w-10 animate-spin text-orange-500" />
                      </div>
                    )}
                    <Image
                      src={option.icon}
                      alt={option.name}
                      width={32}
                      height={32}
                      className={`transition-opacity ${isConnectingThis ? 'opacity-70' : 'opacity-100'}`}
                    />
                  </div>
                  <span className="text-sm font-medium">
                    {option.name}
                  </span>
                  <span className="text-xs text-muted-foreground text-center">
                    {isConnectingThis ? 'Connecting...' : option.description}
                  </span>
                </button>
              );
            })}
          </div>
          
          <div className="flex items-center justify-center">
            <label className="flex cursor-pointer items-center">
              <input
                type="checkbox"
                checked={rememberWallet}
                onChange={toggleRememberWallet}
                className="h-4 w-4 rounded border-muted-foreground/30 text-orange-500 focus:ring-orange-500"
              />
              <span className="ml-2 text-sm text-muted-foreground">
                Remember wallet and connect automatically
              </span>
            </label>
          </div>
        </div>
      )}
    </BottomSheet>
  );
}