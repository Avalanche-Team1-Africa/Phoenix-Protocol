"use client";

import React, { useState } from "react";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { useWallet } from "@/context/wallet-context";
import { formatAddress } from "@/lib/utils/blockchain";
import { ThemeToggle } from "@/components/ui/theme-toggle";
import { LanguageSelector } from "@/components/ui/language-selector";
import { NotificationCenter } from "@/components/ui/notification-center";
import { Menu, X } from "lucide-react";

export function Header() {
  const [isWalletModalOpen, setIsWalletModalOpen] = useState(false);
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);
  const { wallet } = useWallet();

  return (
    <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="container flex h-16 items-center justify-between">
        <div className="flex items-center gap-6 md:gap-10">
          <Link href="/" className="flex items-center space-x-2">
            <span className="text-2xl font-bold bg-phoenix-gradient bg-clip-text text-transparent">
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
          <ThemeToggle />
          <LanguageSelector />
          
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
            className="hidden md:flex"
          >
            <Link href="/transactions">
              Launch App
            </Link>
          </Button>
          
          {/* Mobile menu button */}
          <Button
            variant="ghost"
            size="icon"
            className="md:hidden"
            onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
          >
            {isMobileMenuOpen ? (
              <X className="h-6 w-6" />
            ) : (
              <Menu className="h-6 w-6" />
            )}
          </Button>
        </div>
      </div>
      
      {/* Mobile menu */}
      {isMobileMenuOpen && (
        <div className="md:hidden border-t">
          <div className="container py-4 space-y-4">
            <nav className="flex flex-col space-y-4">
              <Link
                href="/"
                className="text-sm font-medium transition-colors hover:text-primary"
                onClick={() => setIsMobileMenuOpen(false)}
              >
                Home
              </Link>
              <Link
                href="/transactions"
                className="text-sm font-medium transition-colors hover:text-primary"
                onClick={() => setIsMobileMenuOpen(false)}
              >
                Transactions
              </Link>
              <Link
                href="/disputes"
                className="text-sm font-medium transition-colors hover:text-primary"
                onClick={() => setIsMobileMenuOpen(false)}
              >
                Disputes
              </Link>
              <Link
                href="/recovery"
                className="text-sm font-medium transition-colors hover:text-primary"
                onClick={() => setIsMobileMenuOpen(false)}
              >
                Recovery
              </Link>
            </nav>
            <div className="flex flex-col space-y-2">
              {wallet.connected ? (
                <Button
                  variant="outline"
                  onClick={() => {
                    setIsWalletModalOpen(true);
                    setIsMobileMenuOpen(false);
                  }}
                >
                  <span className="flex items-center">
                    <span className="w-2 h-2 bg-green-500 rounded-full mr-2"></span>
                    {formatAddress(wallet.address)}
                  </span>
                </Button>
              ) : (
                <Button
                  variant="outline"
                  onClick={() => {
                    setIsWalletModalOpen(true);
                    setIsMobileMenuOpen(false);
                  }}
                >
                  Connect Wallet
                </Button>
              )}
              <Button
                variant="gradient"
                asChild
                onClick={() => setIsMobileMenuOpen(false)}
              >
                <Link href="/transactions">
                  Launch App
                </Link>
              </Button>
            </div>
          </div>
        </div>
      )}
      
      <WalletConnectModal 
        isOpen={isWalletModalOpen} 
        onClose={() => setIsWalletModalOpen(false)} 
      />
    </header>
  );
}