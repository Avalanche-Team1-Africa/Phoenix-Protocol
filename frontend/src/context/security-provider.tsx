"use client";

import React, { createContext, useContext, useState, useEffect } from "react";
import { useRouter } from "next/navigation";
import { useWallet } from "@/context/wallet-context";

interface SecurityContextType {
  isTwoFactorEnabled: boolean;
  isSessionActive: boolean;
  lastActivity: number;
  sessionTimeout: number;
  enableTwoFactor: () => Promise<boolean>;
  disableTwoFactor: () => Promise<boolean>;
  verifyTwoFactor: (code: string) => Promise<boolean>;
  resetSession: () => void;
  extendSession: () => void;
  endSession: () => void;
  isHardwareWalletConnected: boolean;
  connectHardwareWallet: (type: string) => Promise<boolean>;
  disconnectHardwareWallet: () => void;
  isSecurityModalOpen: boolean;
  openSecurityModal: (action: "enable" | "disable") => void;
  closeSecurityModal: () => void;
  securityModalAction: "enable" | "disable";
}

const SecurityContext = createContext<SecurityContextType | undefined>(undefined);

export function SecurityProvider({ children }: { children: React.ReactNode }) {
  const router = useRouter();
  const { wallet } = useWallet();
  
  // Security state
  const [isTwoFactorEnabled, setIsTwoFactorEnabled] = useState(false);
  const [isSessionActive, setIsSessionActive] = useState(true);
  const [lastActivity, setLastActivity] = useState(Date.now());
  const [sessionTimeout, setSessionTimeout] = useState(30 * 60 * 1000); // 30 minutes by default
  const [isHardwareWalletConnected, setIsHardwareWalletConnected] = useState(false);
  const [isSecurityModalOpen, setIsSecurityModalOpen] = useState(false);
  const [securityModalAction, setSecurityModalAction] = useState<"enable" | "disable">("enable");
  
  // Load security settings from localStorage on mount
  useEffect(() => {
    if (typeof window !== "undefined") {
      const storedTwoFactor = localStorage.getItem("phoenixTwoFactorEnabled");
      const storedSessionTimeout = localStorage.getItem("phoenixSessionTimeout");
      const storedHardwareWallet = localStorage.getItem("phoenixHardwareWallet");
      
      if (storedTwoFactor) {
        setIsTwoFactorEnabled(storedTwoFactor === "true");
      }
      
      if (storedSessionTimeout) {
        setSessionTimeout(parseInt(storedSessionTimeout));
      }
      
      if (storedHardwareWallet) {
        setIsHardwareWalletConnected(storedHardwareWallet === "true");
      }
    }
  }, []);
  
  // Session timeout monitoring
  useEffect(() => {
    if (!isSessionActive || !wallet.connected) return;
    
    const checkActivity = () => {
      const now = Date.now();
      if (now - lastActivity > sessionTimeout) {
        endSession();
      }
    };
    
    const interval = setInterval(checkActivity, 60000); // Check every minute
    
    return () => clearInterval(interval);
  }, [isSessionActive, lastActivity, sessionTimeout, wallet.connected]);
  
  // Track user activity
  useEffect(() => {
    if (!isSessionActive) return;
    
    const updateActivity = () => {
      setLastActivity(Date.now());
    };
    
    // Listen for user activity
    window.addEventListener("mousemove", updateActivity);
    window.addEventListener("keydown", updateActivity);
    window.addEventListener("click", updateActivity);
    window.addEventListener("scroll", updateActivity);
    
    return () => {
      window.removeEventListener("mousemove", updateActivity);
      window.removeEventListener("keydown", updateActivity);
      window.removeEventListener("click", updateActivity);
      window.removeEventListener("scroll", updateActivity);
    };
  }, [isSessionActive]);
  
  // Enable 2FA
  const enableTwoFactor = async (): Promise<boolean> => {
    try {
      // In a real app, this would:
      // 1. Generate a secret key
      // 2. Show QR code to user for scanning with authenticator app
      // 3. Verify initial code
      // 4. Store association on server
      
      // Mock implementation for demo
      setIsTwoFactorEnabled(true);
      localStorage.setItem("phoenixTwoFactorEnabled", "true");
      return true;
    } catch (error) {
      console.error("Error enabling 2FA:", error);
      return false;
    }
  };
  
  // Disable 2FA
  const disableTwoFactor = async (): Promise<boolean> => {
    try {
      // In a real app, this would verify identity before disabling
      setIsTwoFactorEnabled(false);
      localStorage.setItem("phoenixTwoFactorEnabled", "false");
      return true;
    } catch (error) {
      console.error("Error disabling 2FA:", error);
      return false;
    }
  };
  
  // Verify 2FA code
  const verifyTwoFactor = async (code: string): Promise<boolean> => {
    try {
      // In a real app, this would validate the code against the stored secret
      // Mock implementation for demo - accept any 6-digit code
      return /^\d{6}$/.test(code);
    } catch (error) {
      console.error("Error verifying 2FA code:", error);
      return false;
    }
  };
  
  // Reset session timer
  const resetSession = () => {
    setIsSessionActive(true);
    setLastActivity(Date.now());
  };
  
  // Extend session
  const extendSession = () => {
    setLastActivity(Date.now());
  };
  
  // End session (logout)
  const endSession = () => {
    setIsSessionActive(false);
    // In a real app, this would clear sensitive data
    router.push("/");
  };
  
  // Connect hardware wallet
  const connectHardwareWallet = async (type: string): Promise<boolean> => {
    try {
      // In a real app, this would:
      // 1. Initialize connection to hardware wallet
      // 2. Request public key
      // 3. Verify connection
      
      // Mock implementation for demo
      setIsHardwareWalletConnected(true);
      localStorage.setItem("phoenixHardwareWallet", "true");
      localStorage.setItem("phoenixHardwareWalletType", type);
      return true;
    } catch (error) {
      console.error("Error connecting hardware wallet:", error);
      return false;
    }
  };
  
  // Disconnect hardware wallet
  const disconnectHardwareWallet = () => {
    setIsHardwareWalletConnected(false);
    localStorage.removeItem("phoenixHardwareWallet");
    localStorage.removeItem("phoenixHardwareWalletType");
  };
  
  // Open security modal
  const openSecurityModal = (action: "enable" | "disable") => {
    setSecurityModalAction(action);
    setIsSecurityModalOpen(true);
  };
  
  // Close security modal
  const closeSecurityModal = () => {
    setIsSecurityModalOpen(false);
  };
  
  const value = {
    isTwoFactorEnabled,
    isSessionActive,
    lastActivity,
    sessionTimeout,
    enableTwoFactor,
    disableTwoFactor,
    verifyTwoFactor,
    resetSession,
    extendSession,
    endSession,
    isHardwareWalletConnected,
    connectHardwareWallet,
    disconnectHardwareWallet,
    isSecurityModalOpen,
    openSecurityModal,
    closeSecurityModal,
    securityModalAction,
  };
  
  return (
    <SecurityContext.Provider value={value}>
      {children}
    </SecurityContext.Provider>
  );
}

export function useSecurity() {
  const context = useContext(SecurityContext);
  if (context === undefined) {
    throw new Error("useSecurity must be used within a SecurityProvider");
  }
  return context;
}