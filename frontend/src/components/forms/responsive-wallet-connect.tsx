"use client";

import React, { useState, useEffect } from "react";
import Image from "next/image";
import { useWallet } from "@/context/wallet-context";
import { WalletConnectModal } from "./wallet-connect-modal";
import { WalletConnectSheet } from "./wallet-connect-sheet";
import { Button } from "@/components/ui/button";
import { Wallet } from "lucide-react";
import { formatAddress } from "@/lib/utils/blockchain";

export function ResponsiveWalletConnect() {
  const { wallet, disconnect } = useWallet();
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [isSheetOpen, setIsSheetOpen] = useState(false);
  const [isMobile, setIsMobile] = useState(false);

  // Detect if we're on mobile
  useEffect(() => {
    const checkIfMobile = () => {
      setIsMobile(window.innerWidth < 768);
    };

    // Check on mount
    checkIfMobile();

    // Add resize listener
    window.addEventListener("resize", checkIfMobile);

    // Clean up
    return () => {
      window.removeEventListener("resize", checkIfMobile);
    };
  }, []);

  const handleOpenConnect = () => {
    if (isMobile) {
      setIsSheetOpen(true);
    } else {
      setIsModalOpen(true);
    }
  };

  return (
    <>
      {wallet.connected ? (
        <Button 
          variant="outline" 
          size="lg" 
          onClick={() => disconnect()}
          className="bg-green-500/10 text-green-500 border-green-500/30"
        >
          <div className="flex items-center">
            <Image
              src={`/wallets/${wallet.walletType || 'metamask'}.svg`}
              alt={wallet.walletType || 'wallet'}
              width={20}
              height={20}
              className="mr-2"
            />
            {formatAddress(wallet.address, 6)}
          </div>
        </Button>
      ) : (
        <Button 
          variant="outline" 
          size="lg" 
          onClick={handleOpenConnect}
          className="border-orange-500/30"
        >
          <Wallet className="mr-2 h-5 w-5" />
          Connect Wallet
        </Button>
      )}
      
      {/* Use modal for desktop */}
      <WalletConnectModal 
        isOpen={isModalOpen && !isMobile} 
        onClose={() => setIsModalOpen(false)} 
      />
      
      {/* Use bottom sheet for mobile */}
      <WalletConnectSheet
        isOpen={isSheetOpen && isMobile}
        onClose={() => setIsSheetOpen(false)}
      />
    </>
  );
}