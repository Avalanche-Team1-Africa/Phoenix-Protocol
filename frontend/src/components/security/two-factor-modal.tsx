"use client";

import React, { useState } from "react";
import Image from "next/image";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { useSecurity } from "@/context/security-provider";
import { Shield, Copy, CheckCircle, AlertTriangle } from "lucide-react";

interface TwoFactorModalProps {
  isOpen: boolean;
  onClose: () => void;
  action: "enable" | "disable";
  onSuccess: () => void;
  onError: (message: string) => void;
}

export function TwoFactorModal({
  isOpen,
  onClose,
  action,
  onSuccess,
  onError,
}: TwoFactorModalProps) {
  const { enableTwoFactor, disableTwoFactor, verifyTwoFactor } = useSecurity();
  
  const [step, setStep] = useState(1);
  const [verificationCode, setVerificationCode] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState("");
  
  // Mock secret key for demo
  const secretKey = "ABCDEFGHIJKLMNOP";
  
  // Handle copy secret key
  const handleCopySecretKey = () => {
    navigator.clipboard.writeText(secretKey);
  };
  
  // Handle verification code submission
  const handleVerifyCode = async () => {
    if (!verificationCode) {
      setError("Please enter a verification code");
      return;
    }
    
    setIsLoading(true);
    setError("");
    
    try {
      const isValid = await verifyTwoFactor(verificationCode);
      
      if (isValid) {
        if (action === "enable") {
          await enableTwoFactor();
        } else {
          await disableTwoFactor();
        }
        
        setIsLoading(false);
        onSuccess();
        onClose();
        setStep(1);
        setVerificationCode("");
      } else {
        setError("Invalid verification code. Please try again.");
        setIsLoading(false);
      }
    } catch (error: any) {
      setError(error.message || "An error occurred");
      setIsLoading(false);
      onError(error.message || "An error occurred");
    }
  };
  
  // Handle cancel
  const handleCancel = () => {
    setStep(1);
    setVerificationCode("");
    setError("");
    onClose();
  };
  
  if (!isOpen) return null;
  
  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="bg-background rounded-lg shadow-lg w-full max-w-md p-6 relative">
        <button
          onClick={handleCancel}
          className="absolute top-4 right-4 text-muted-foreground hover:text-foreground"
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
        
        <div className="text-center mb-6">
          <div className="mx-auto w-12 h-12 rounded-full bg-orange-500/10 flex items-center justify-center mb-4">
            <Shield className="h-6 w-6 text-orange-500" />
          </div>
          <h2 className="text-2xl font-bold">
            {action === "enable" ? "Enable" : "Disable"} Two-Factor Authentication
          </h2>
          <p className="text-muted-foreground mt-2">
            {action === "enable"
              ? "Enhance your account security with 2FA"
              : "Remove two-factor authentication from your account"}
          </p>
        </div>
        
        {action === "enable" && step === 1 && (
          <div className="space-y-6">
            <div className="p-4 bg-muted rounded-lg">
              <p className="text-sm mb-4">
                Scan the QR code below with your authenticator app (Google Authenticator, Authy, etc.)
                or enter the secret key manually.
              </p>
              
              <div className="flex justify-center mb-4">
                <div className="relative h-48 w-48 bg-white p-2 rounded-lg">
                  <Image
                    src="/qr-code-placeholder.png"
                    alt="QR Code"
                    fill
                    className="object-contain"
                  />
                </div>
              </div>
              
              <div className="flex items-center justify-between p-2 bg-background rounded border">
                <code className="text-sm font-mono">{secretKey}</code>
                <Button variant="ghost" size="sm" onClick={handleCopySecretKey}>
                  <Copy className="h-4 w-4" />
                </Button>
              </div>
            </div>
            
            <div className="flex justify-end space-x-2">
              <Button variant="outline" onClick={handleCancel}>
                Cancel
              </Button>
              <Button variant="gradient" onClick={() => setStep(2)}>
                Continue
              </Button>
            </div>
          </div>
        )}
        
        {((action === "enable" && step === 2) || action === "disable") && (
          <div className="space-y-6">
            <div className="space-y-4">
              <div className="space-y-2">
                <label htmlFor="verificationCode" className="text-sm font-medium">
                  Verification Code
                </label>
                <Input
                  id="verificationCode"
                  type="text"
                  placeholder="Enter 6-digit code"
                  value={verificationCode}
                  onChange={(e) => setVerificationCode(e.target.value)}
                  maxLength={6}
                />
              </div>
              
              {error && (
                <div className="p-3 bg-red-500/10 text-red-500 rounded-md flex items-center">
                  <AlertTriangle className="h-4 w-4 mr-2" />
                  <span className="text-sm">{error}</span>
                </div>
              )}
              
              <div className="p-3 bg-muted rounded-md flex items-start">
                <div className="mt-0.5">
                  <CheckCircle className="h-4 w-4 text-orange-500 mr-2" />
                </div>
                <span className="text-sm">
                  {action === "enable"
                    ? "You'll need to enter a verification code when performing critical operations."
                    : "You're about to disable two-factor authentication. This will reduce your account security."}
                </span>
              </div>
            </div>
            
            <div className="flex justify-end space-x-2">
              <Button variant="outline" onClick={handleCancel}>
                Cancel
              </Button>
              <Button
                variant={action === "enable" ? "gradient" : "destructive"}
                onClick={handleVerifyCode}
                disabled={isLoading}
              >
                {isLoading ? "Processing..." : action === "enable" ? "Enable 2FA" : "Disable 2FA"}
              </Button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}