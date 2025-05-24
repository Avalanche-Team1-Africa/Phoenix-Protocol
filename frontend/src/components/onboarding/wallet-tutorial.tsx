"use client";

import React, { useState, useEffect } from "react";
import { TutorialModal } from "./tutorial-modal";
import { useWallet } from "@/context/wallet-context";

// Tutorial steps for wallet connection
const walletTutorialSteps = [
  {
    title: "Welcome to Phoenix Protocol",
    description: "This quick tutorial will guide you through connecting your wallet and using our platform safely. Let's get started!",
    image: "/onboarding/welcome.svg",
  },
  {
    title: "Choose Your Wallet",
    description: "Select from popular wallets like MetaMask, Core Wallet, or WalletConnect. If you don't have a wallet yet, you'll be guided to install one.",
    image: "/onboarding/choose-wallet.svg",
  },
  {
    title: "Connect Securely",
    description: "When prompted by your wallet, verify the connection details. Only approve connections to phoenixprotocol.io to stay safe from phishing.",
    image: "/onboarding/connect-securely.svg",
  },
  {
    title: "Verify Transactions",
    description: "Phoenix Protocol helps you verify transaction details before execution. Always check amounts, addresses, and gas fees before confirming.",
    image: "/onboarding/verify-tx.svg",
  },
  {
    title: "Recovery Protection",
    description: "Our platform offers transaction recovery in case of errors. Set up your recovery preferences in the settings to enable this protection.",
    image: "/onboarding/recovery.svg",
  },
];

export function WalletTutorial() {
  const { wallet } = useWallet();
  const [showTutorial, setShowTutorial] = useState(false);
  
  // Check if this is the user's first visit
  useEffect(() => {
    const hasSeenTutorial = localStorage.getItem("phoenixWalletTutorialComplete");
    if (!hasSeenTutorial && !wallet.connected) {
      // Only show after a short delay to not overwhelm the user immediately
      const timer = setTimeout(() => {
        setShowTutorial(true);
      }, 1500);
      
      return () => clearTimeout(timer);
    }
  }, [wallet.connected]);
  
  const handleTutorialComplete = () => {
    localStorage.setItem("phoenixWalletTutorialComplete", "true");
  };
  
  return (
    <TutorialModal
      isOpen={showTutorial}
      onClose={() => setShowTutorial(false)}
      steps={walletTutorialSteps}
      onComplete={handleTutorialComplete}
    />
  );
}

// Component to manually trigger the tutorial
export function WalletTutorialTrigger({ children }: { children: React.ReactNode }) {
  const [showTutorial, setShowTutorial] = useState(false);
  
  const handleTutorialComplete = () => {
    localStorage.setItem("phoenixWalletTutorialComplete", "true");
  };
  
  return (
    <>
      <div onClick={() => setShowTutorial(true)}>{children}</div>
      <TutorialModal
        isOpen={showTutorial}
        onClose={() => setShowTutorial(false)}
        steps={walletTutorialSteps}
        onComplete={handleTutorialComplete}
      />
    </>
  );
}