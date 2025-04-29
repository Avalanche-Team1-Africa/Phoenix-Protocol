"use client";

import React, { useState } from "react";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { useWallet } from "@/context/wallet-context";
import { formatAddress } from "@/lib/utils/blockchain";

export function Header() {
  const [isWalletModalOpen, setIsWalletModalOpen] = useState(false);
  const { wallet } = useWallet();

  return (
    <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="container flex h-16 items-center justify-between">
        <div className="flex items-center gap-6 md:gap-10">
          <Link href="/" className="flex items-center space-x-2">
            <span className="text-2xl font-bold bg-gradient-to-r from-orange-500 to-red-500 bg-clip-text text-transparent">
              Phoenix Protocol
            </span>
          </Link>
          <nav className="hidden md:flex gap-6">
            <Link
              href="/"
              className="text-sm font-medium transition-colors hover:text-primary"
            >
              Home
            </Link>
            <Link
              href="/transactions"
              className="text-sm font-medium transition-colors hover:text-primary"
            >
              Transactions
            </Link>
            <Link
              href="/disputes"
              className="text-sm font-medium transition-colors hover:text-primary"
            >
              Disputes
            </Link>
            <Link
              href="/recovery"
              className="text-sm font-medium transition-colors hover:text-primary"
            >
              Recovery
            </Link>
          </nav>
        </div>
        <div className="flex items-center gap-2">
          {wallet.connected ? (
            <Button
              variant="outline"
              size="sm"
              className="hidden md:flex"
              onClick={() => setIsWalletModalOpen(true)}
            >
              <span className="flex items-center">
                <span className="w-2 h-2 bg-green-500 rounded-full mr-2"></span>
                {formatAddress(wallet.address)}
              </span>
            </Button>
          ) : (
            <Button
              variant="outline"
              size="sm"
              className="hidden md:flex"
              onClick={() => setIsWalletModalOpen(true)}
            >
              Connect Wallet
            </Button>
          )}
          <Button
            variant="gradient"
            size="sm"
            asChild
          >
            <Link href="/transactions">
              Launch App
            </Link>
          </Button>
        </div>
      </div>
      
      <WalletConnectModal 
        isOpen={isWalletModalOpen} 
        onClose={() => setIsWalletModalOpen(false)} 
      />
    </header>
  );
}