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
            <div className="p-4 border rounded-lg">
              <div className="flex justify-between items-center mb-2">
                <span className="text-sm text-muted-foreground">Connected Wallet</span>
                <span className="text-sm font-medium capitalize">{wallet.walletType}</span>
              </div>
              <div className="font-mono text-lg mb-2">{formatAddress(wallet.address)}</div>
              <div className="flex justify-between text-sm">
                <span>Balance:</span>
                <span>{parseFloat(wallet.balance).toFixed(4)} ETH</span>
              </div>
              <div className="flex justify-between text-sm">
                <span>Network:</span>
                <span>
                  {wallet.chainId === 1 ? "Ethereum Mainnet" : 
                   wallet.chainId === 43114 ? "Avalanche C-Chain" : 
                   `Chain ID: ${wallet.chainId}`}
                </span>
              </div>
            </div>
            
            <Button
              variant="outline"
              className="w-full"
              onClick={disconnect}
            >
              Disconnect Wallet
            </Button>
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