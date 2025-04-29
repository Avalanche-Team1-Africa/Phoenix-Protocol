"use client";

import React, { useState } from "react";
import Link from "next/link";
import Image from "next/image";
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
            <Image 
              src="/PhoenixProtocol.png" 
              alt="Phoenix Protocol Logo" 
              width={40} 
              height={40} 
              className="mr-2"
            />
            <span className="text-2xl font-bold text-phoenix-gradient">
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
          <NotificationCenter />
          
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
              <span className="flex items-center">
                <svg 
                  xmlns="http://www.w3.org/2000/svg" 
                  className="h-4 w-4 mr-1" 
                  viewBox="0 0 24 24" 
                  fill="none" 
                  stroke="currentColor" 
                  strokeWidth="2" 
                  strokeLinecap="round" 
                  strokeLinejoin="round"
                >
                  <path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4"></path>
                  <polyline points="10 17 15 12 10 7"></polyline>
                  <line x1="15" y1="12" x2="3" y2="12"></line>
                </svg>
                Manage Transactions
              </span>
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
            <div className="flex items-center mb-4">
              <Image 
                src="/PhoenixProtocol.png" 
                alt="Phoenix Protocol Logo" 
                width={32} 
                height={32} 
                className="mr-2"
              />
              <span className="text-xl font-bold text-phoenix-gradient">
                Phoenix Protocol
              </span>
            </div>
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