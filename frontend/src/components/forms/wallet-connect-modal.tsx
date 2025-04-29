"use client";

import React, { useState, useEffect } from "react";
import Image from "next/image";
import { useWallet, WalletType } from "@/context/wallet-context";
import { Button } from "@/components/ui/button";
import { formatAddress } from "@/lib/utils/blockchain";

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
    id: "phantom",
    name: "Phantom",
    icon: "/wallets/phantom.svg",
    description: "Connect to Phantom (Solana)",
  },
  {
    id: "walletconnect",
    name: "WalletConnect",
    icon: "/wallets/walletconnect.svg",
    description: "Connect with WalletConnect",
  },
  {
    id: "coinbase",
    name: "Coinbase Wallet",
    icon: "/wallets/coinbase.svg",
    description: "Connect to Coinbase Wallet",
  },
  {
    id: "trust",
    name: "Trust Wallet",
    icon: "/wallets/trust.svg",
    description: "Connect to Trust Wallet",
  },
];

interface WalletConnectModalProps {
  isOpen: boolean;
  onClose: () => void;
}

export function WalletConnectModal({ isOpen, onClose }: WalletConnectModalProps) {
  const { connect, wallet, disconnect, isConnecting, error } = useWallet();

  const handleConnect = async (walletType: WalletType) => {
    const success = await connect(walletType);
    if (success) {
      onClose();
    }
  };

  const [isVisible, setIsVisible] = useState(false);

  useEffect(() => {
    if (isOpen) {
      setIsVisible(true);
    }
  }, [isOpen]);

  if (!isOpen) return null;

  return (
    <div 
      className={`fixed inset-0 bg-black/50 flex items-center justify-center z-50 transition-opacity duration-300 ${isVisible ? 'opacity-100' : 'opacity-0'}`}
    >
      <div 
        className={`bg-background rounded-lg shadow-lg max-w-md w-full p-6 m-4 transition-all duration-300 ${isVisible ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}
      >
        <div className="flex justify-between items-center mb-6">
          <h3 className="text-xl font-bold">Connect Wallet</h3>
          <button
            onClick={onClose}
            className="text-muted-foreground hover:text-foreground transition-colors"
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="24"
              height="24"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeWidth="2"
              strokeLinecap="round"
              strokeLinejoin="round"
            >
              <line x1="18" y1="6" x2="6" y2="18"></line>
              <line x1="6" y1="6" x2="18" y2="18"></line>
            </svg>
          </button>
        </div>

        {error && (
          <div className="mb-4 p-3 bg-red-500/10 text-red-500 rounded-md text-sm">
            {error}
          </div>
        )}

        {wallet.connected ? (
          <div className="space-y-4">
            <div className="p-4 border rounded-lg bg-muted/30">
              <div className="flex items-center mb-4">
                <div className="w-10 h-10 rounded-full bg-orange-500/10 flex items-center justify-center mr-3">
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
                <div className="bg-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-muted-foreground">Address</span>
                    <button 
                      className="text-xs text-orange-500 hover:text-orange-600"
                      onClick={() => navigator.clipboard.writeText(wallet.address)}
                    >
                      Copy
                    </button>
                  </div>
                  <div className="font-mono text-sm">{formatAddress(wallet.address, 12)}</div>
                </div>
                
                <div className="bg-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-muted-foreground">Balance</span>
                    <span className="text-xs text-orange-500">Native Token</span>
                  </div>
                  <div className="font-medium">
                    {parseFloat(wallet.balance).toFixed(4)} 
                    {wallet.chainId === 43114 || wallet.chainId === 43113 ? " AVAX" : 
                     wallet.chainId === 2 || wallet.chainId === 3 ? " ADA" : " ETH"}
                  </div>
                </div>
                
                <div className="bg-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-muted-foreground">Network</span>
                    <span className="text-xs px-2 py-0.5 bg-green-500/10 text-green-500 rounded-full">Connected</span>
                  </div>
                  <div className="font-medium">
                    {wallet.chainId === 1 ? "Ethereum Mainnet" : 
                     wallet.chainId === 11155111 ? "Sepolia Testnet" :
                     wallet.chainId === 43114 ? "Avalanche C-Chain" : 
                     wallet.chainId === 43113 ? "Avalanche Fuji Testnet" :
                     wallet.chainId === 2 ? "Cardano Mainnet" :
                     wallet.chainId === 3 ? "Cardano Testnet" :
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
                variant="gradient"
                className="flex-1"
                onClick={onClose}
              >
                Continue
              </Button>
            </div>
          </div>
        ) : (
          <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
            {walletOptions.map((option, index) => (
              <button
                key={option.id}
                className={`flex flex-col items-center p-4 border rounded-lg hover:bg-muted transition-all hover:shadow-md hover:scale-[1.03] active:scale-[0.98] animate-fadeIn`}
                style={{ animationDelay: `${index * 50}ms` }}
                onClick={() => handleConnect(option.id)}
                disabled={isConnecting}
              >
                <div className="w-12 h-12 mb-3 flex items-center justify-center">
                  <Image
                    src={option.icon}
                    alt={option.name}
                    width={48}
                    height={48}
                    className="rounded-full"
                  />
                </div>
                <span className="font-medium">{option.name}</span>
                <span className="text-xs text-muted-foreground mt-1 text-center">
                  {option.description}
                </span>
              </button>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}