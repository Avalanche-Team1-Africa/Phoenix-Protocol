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

interface WalletConnectModalProps {
  isOpen: boolean;
  onClose: () => void;
}

export function WalletConnectModal({ isOpen, onClose }: WalletConnectModalProps) {
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
          <div className="mb-4 p-3 bg-phoenix-light-error-light dark:bg-phoenix-dark-error-light text-phoenix-light-error dark:text-phoenix-dark-error rounded-md">
            <div className="flex items-start">
              <svg 
                xmlns="http://www.w3.org/2000/svg" 
                width="20" 
                height="20" 
                viewBox="0 0 24 24" 
                fill="none" 
                stroke="currentColor" 
                strokeWidth="2" 
                strokeLinecap="round" 
                strokeLinejoin="round"
                className="mr-2 mt-0.5 flex-shrink-0"
              >
                <circle cx="12" cy="12" r="10"></circle>
                <line x1="12" y1="8" x2="12" y2="12"></line>
                <line x1="12" y1="16" x2="12.01" y2="16"></line>
              </svg>
              <div>
                <p className="font-medium text-sm">{error}</p>
                {error.includes("not installed") && (
                  <p className="text-xs mt-1">
                    Click the wallet option again to visit the download page.
                  </p>
                )}
              </div>
            </div>
          </div>
        )}

        {wallet.connected ? (
          <div className="space-y-4">
            <div className="p-4 border rounded-lg bg-muted/30">
              <div className="flex items-center mb-4">
                <div className="w-10 h-10 rounded-full bg-phoenix-light-primary/10 dark:bg-phoenix-dark-primary/10 flex items-center justify-center mr-3">
                  <Image
                    src={`/wallets/${wallet.walletType || 'metamask'}.svg`}
                    alt={wallet.walletType || 'wallet'}
                    width={24}
                    height={24}
                  />
                </div>
                <div>
                  <h4 className="font-medium capitalize">{wallet.walletType} Wallet</h4>
                  <p className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Connected</p>
                </div>
              </div>
              
              <div className="space-y-3">
                <div className="bg-phoenix-light-background dark:bg-phoenix-dark-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Address</span>
                    <button 
                      className="text-xs text-phoenix-light-primary dark:text-phoenix-dark-primary hover:text-phoenix-light-primary-hover dark:hover:text-phoenix-dark-primary-hover"
                      onClick={() => navigator.clipboard.writeText(wallet.address)}
                    >
                      Copy
                    </button>
                  </div>
                  <div className="font-mono text-sm text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">{formatAddress(wallet.address, 12)}</div>
                </div>
                
                <div className="bg-phoenix-light-background dark:bg-phoenix-dark-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Balance</span>
                    <span className="text-xs text-phoenix-light-primary dark:text-phoenix-dark-primary">Native Token</span>
                  </div>
                  <div className="font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
                    {parseFloat(wallet.balance).toFixed(4)} 
                    {wallet.chainId === 43114 || wallet.chainId === 43113 ? " AVAX" : " ETH"}
                  </div>
                </div>
                
                <div className="bg-phoenix-light-background dark:bg-phoenix-dark-background p-3 rounded-md">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">Network</span>
                    <span className="text-xs px-2 py-0.5 bg-phoenix-light-success-light dark:bg-phoenix-dark-success-light text-phoenix-light-success dark:text-phoenix-dark-success rounded-full">Connected</span>
                  </div>
                  <div className="font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
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
                className="flex-1 border-phoenix-light-border dark:border-phoenix-dark-border text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary"
                onClick={disconnect}
              >
                Disconnect
              </Button>
              <Button
                className="flex-1 bg-phoenix-gradient dark:bg-phoenix-gradient-dark text-white"
                onClick={onClose}
              >
                Continue
              </Button>
            </div>
          </div>
        ) : (
          <div className="space-y-6">
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
              {walletOptions.map((option, index) => {
                const isConnectingThis = connectingWallet === option.id;
                return (
                  <button
                    key={option.id}
                    className={`flex flex-col items-center p-4 border ${
                      isConnectingThis 
                        ? 'border-phoenix-light-primary dark:border-phoenix-dark-primary' 
                        : 'border-phoenix-light-border dark:border-phoenix-dark-border'
                    } rounded-lg hover:bg-phoenix-light-muted dark:hover:bg-phoenix-dark-muted transition-all hover:shadow-md hover:scale-[1.03] active:scale-[0.98] animate-fadeIn ${
                      isConnecting && !isConnectingThis ? 'opacity-50 cursor-not-allowed' : ''
                    }`}
                    style={{ animationDelay: `${index * 50}ms` }}
                    onClick={() => handleConnect(option.id)}
                    disabled={isConnecting}
                  >
                    <div className="w-12 h-12 mb-3 flex items-center justify-center relative">
                      {isConnectingThis && (
                        <div className="absolute inset-0 flex items-center justify-center">
                          <div className="w-12 h-12 rounded-full border-2 border-transparent border-t-phoenix-light-primary dark:border-t-phoenix-dark-primary animate-spin"></div>
                        </div>
                      )}
                      <Image
                        src={option.icon}
                        alt={option.name}
                        width={48}
                        height={48}
                        className={`rounded-full transition-opacity ${isConnectingThis ? 'opacity-70' : 'opacity-100'}`}
                      />
                    </div>
                    <span className="font-medium text-phoenix-light-text-primary dark:text-phoenix-dark-text-primary">
                      {option.name}
                    </span>
                    <span className="text-xs text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary mt-1 text-center">
                      {isConnectingThis ? 'Connecting...' : option.description}
                    </span>
                  </button>
                );
              })}
            </div>
            
            <div className="flex items-center justify-center mt-4">
              <label className="flex items-center cursor-pointer">
                <input
                  type="checkbox"
                  checked={rememberWallet}
                  onChange={toggleRememberWallet}
                  className="h-4 w-4 rounded border-phoenix-light-border dark:border-phoenix-dark-border text-phoenix-light-primary dark:text-phoenix-dark-primary focus:ring-phoenix-light-primary dark:focus:ring-phoenix-dark-primary"
                />
                <span className="ml-2 text-sm text-phoenix-light-text-secondary dark:text-phoenix-dark-text-secondary">
                  Remember wallet and connect automatically
                </span>
              </label>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}