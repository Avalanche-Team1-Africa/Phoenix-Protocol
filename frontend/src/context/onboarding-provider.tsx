"use client";

import React, { createContext, useContext, useState, useEffect } from "react";
import { TutorialModal } from "@/components/onboarding/tutorial-modal";

interface OnboardingContextType {
  showTutorial: () => void;
  completeTutorial: () => void;
  resetOnboarding: () => void;
  hasCompletedOnboarding: boolean;
  isFirstVisit: boolean;
  markTooltipAsSeen: (id: string) => void;
  hasSeenTooltip: (id: string) => boolean;
}

const OnboardingContext = createContext<OnboardingContextType | undefined>(undefined);

// Tutorial steps
const tutorialSteps = [
  {
    title: "Welcome to Phoenix Protocol",
    description: "Phoenix Protocol is a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems. Let's get you started with the basics.",
    image: "/tutorial/welcome.png",
  },
  {
    title: "Connect Your Wallet",
    description: "First, connect your wallet by clicking the 'Connect Wallet' button in the top right corner. We support multiple wallet types including MetaMask, WalletConnect, and hardware wallets.",
    image: "/tutorial/connect-wallet.png",
  },
  {
    title: "Create Transaction Intents",
    description: "Before executing a transaction, create an intent that specifies what you want to do. This helps protect you from malicious transactions and provides a safety net.",
    image: "/tutorial/create-intent.png",
  },
  {
    title: "Verify Transactions",
    description: "When you execute a transaction, Phoenix Protocol verifies that it matches your intent. If there's a discrepancy, you'll be alerted and can prevent potential loss of funds.",
    image: "/tutorial/verify-transaction.png",
  },
  {
    title: "Recover When Needed",
    description: "If something goes wrong, you can use our recovery features to roll back transactions or recover funds. This provides an extra layer of security for your digital assets.",
    image: "/tutorial/recovery.png",
  },
];

export function OnboardingProvider({ children }: { children: React.ReactNode }) {
  const [isTutorialOpen, setIsTutorialOpen] = useState(false);
  const [hasCompletedOnboarding, setHasCompletedOnboarding] = useState(false);
  const [isFirstVisit, setIsFirstVisit] = useState(true);
  const [seenTooltips, setSeenTooltips] = useState<string[]>([]);
  
  // Check if user has completed onboarding
  useEffect(() => {
    if (typeof window !== "undefined") {
      const completed = localStorage.getItem("phoenixOnboardingCompleted");
      if (completed === "true") {
        setHasCompletedOnboarding(true);
      }
      
      const visited = localStorage.getItem("phoenixHasVisited");
      if (visited === "true") {
        setIsFirstVisit(false);
      } else {
        localStorage.setItem("phoenixHasVisited", "true");
        setIsTutorialOpen(true);
      }
      
      const tooltips = localStorage.getItem("phoenixSeenTooltips");
      if (tooltips) {
        setSeenTooltips(JSON.parse(tooltips));
      }
    }
  }, []);
  
  const showTutorial = () => {
    setIsTutorialOpen(true);
  };
  
  const completeTutorial = () => {
    setHasCompletedOnboarding(true);
    localStorage.setItem("phoenixOnboardingCompleted", "true");
  };
  
  const resetOnboarding = () => {
    setHasCompletedOnboarding(false);
    setIsFirstVisit(true);
    setSeenTooltips([]);
    localStorage.removeItem("phoenixOnboardingCompleted");
    localStorage.removeItem("phoenixHasVisited");
    localStorage.removeItem("phoenixSeenTooltips");
  };
  
  const markTooltipAsSeen = (id: string) => {
    if (!seenTooltips.includes(id)) {
      const updatedTooltips = [...seenTooltips, id];
      setSeenTooltips(updatedTooltips);
      localStorage.setItem("phoenixSeenTooltips", JSON.stringify(updatedTooltips));
    }
  };
  
  const hasSeenTooltip = (id: string) => {
    return seenTooltips.includes(id);
  };
  
  return (
    <OnboardingContext.Provider
      value={{
        showTutorial,
        completeTutorial,
        resetOnboarding,
        hasCompletedOnboarding,
        isFirstVisit,
        markTooltipAsSeen,
        hasSeenTooltip,
      }}
    >
      {children}
      
      <TutorialModal
        isOpen={isTutorialOpen}
        onClose={() => setIsTutorialOpen(false)}
        steps={tutorialSteps}
        onComplete={completeTutorial}
      />
    </OnboardingContext.Provider>
  );
}

export function useOnboarding() {
  const context = useContext(OnboardingContext);
  if (context === undefined) {
    throw new Error("useOnboarding must be used within an OnboardingProvider");
  }
  return context;
}