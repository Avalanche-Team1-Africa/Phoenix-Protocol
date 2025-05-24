"use client";

import React, { useState, useRef, useEffect } from "react";
import { AlertTriangle, Shield, CheckCircle, Smartphone, Mail, Key } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";

interface TwoFactorAuthProps {
  onVerify: (method: string, code: string) => Promise<boolean>;
  onCancel: () => void;
  availableMethods: Array<"email" | "sms" | "authenticator">;
  userEmail?: string;
  userPhone?: string;
}

export function TwoFactorAuth({
  onVerify,
  onCancel,
  availableMethods,
  userEmail,
  userPhone,
}: TwoFactorAuthProps) {
  const [selectedMethod, setSelectedMethod] = useState<string>(availableMethods[0]);
  const [code, setCode] = useState<string[]>(["", "", "", "", "", ""]);
  const [isVerifying, setIsVerifying] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [countdown, setCountdown] = useState(0);
  const [resendDisabled, setResendDisabled] = useState(false);
  
  const inputRefs = useRef<(HTMLInputElement | null)[]>([]);

  // Handle countdown for resend code
  useEffect(() => {
    if (countdown > 0) {
      const timer = setTimeout(() => setCountdown(countdown - 1), 1000);
      return () => clearTimeout(timer);
    } else if (countdown === 0 && resendDisabled) {
      setResendDisabled(false);
    }
  }, [countdown, resendDisabled]);

  // Handle input change
  const handleInputChange = (index: number, value: string) => {
    if (value.length > 1) {
      // Handle paste event
      const pastedValue = value.slice(0, 6).split("");
      const newCode = [...code];
      
      pastedValue.forEach((char, i) => {
        if (index + i < 6) {
          newCode[index + i] = char;
        }
      });
      
      setCode(newCode);
      
      // Focus on the next empty input or the last one
      const nextIndex = Math.min(index + pastedValue.length, 5);
      inputRefs.current[nextIndex]?.focus();
    } else {
      // Handle single character input
      const newCode = [...code];
      newCode[index] = value;
      setCode(newCode);
      
      // Auto-focus next input
      if (value && index < 5) {
        inputRefs.current[index + 1]?.focus();
      }
    }
  };

  // Handle key down
  const handleKeyDown = (index: number, e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Backspace" && !code[index] && index > 0) {
      // Move to previous input on backspace if current is empty
      inputRefs.current[index - 1]?.focus();
    } else if (e.key === "ArrowLeft" && index > 0) {
      // Move to previous input on left arrow
      inputRefs.current[index - 1]?.focus();
    } else if (e.key === "ArrowRight" && index < 5) {
      // Move to next input on right arrow
      inputRefs.current[index + 1]?.focus();
    }
  };

  // Handle verification
  const handleVerify = async () => {
    const fullCode = code.join("");
    
    if (fullCode.length !== 6) {
      setError("Please enter a 6-digit code");
      return;
    }
    
    setIsVerifying(true);
    setError(null);
    
    try {
      const success = await onVerify(selectedMethod, fullCode);
      
      if (!success) {
        setError("Invalid verification code. Please try again.");
        // Clear the code
        setCode(["", "", "", "", "", ""]);
        // Focus on first input
        inputRefs.current[0]?.focus();
      }
    } catch (err: any) {
      setError(err.message || "Verification failed");
    } finally {
      setIsVerifying(false);
    }
  };

  // Handle resend code
  const handleResendCode = () => {
    setResendDisabled(true);
    setCountdown(60);
    // In a real app, you would call an API to resend the code
    console.log(`Resending code via ${selectedMethod}`);
  };

  // Mask email or phone
  const maskEmail = (email: string) => {
    const [username, domain] = email.split("@");
    return `${username.charAt(0)}${"*".repeat(username.length - 2)}${username.charAt(username.length - 1)}@${domain}`;
  };
  
  const maskPhone = (phone: string) => {
    return `${"*".repeat(phone.length - 4)}${phone.slice(-4)}`;
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center">
          <Shield className="mr-2 h-5 w-5 text-orange-500" />
          <h2 className="text-xl font-bold">Two-Factor Authentication</h2>
        </div>
      </div>

      <div className="rounded-lg border border-orange-500/30 bg-orange-500/5 p-4">
        <div className="flex items-start">
          <Key className="mr-3 mt-0.5 h-5 w-5 text-orange-500" />
          <div>
            <h3 className="font-medium text-orange-500">Security Verification</h3>
            <p className="mt-1 text-sm text-orange-500/80">
              For your security, we need to verify your identity. Please select a
              verification method and enter the code you receive.
            </p>
          </div>
        </div>
      </div>

      {/* Verification Methods */}
      <Tabs defaultValue={selectedMethod} onValueChange={setSelectedMethod}>
        <TabsList className="grid w-full grid-cols-3">
          {availableMethods.includes("email") && (
            <TabsTrigger value="email" disabled={!userEmail}>
              <Mail className="mr-2 h-4 w-4" />
              Email
            </TabsTrigger>
          )}
          {availableMethods.includes("sms") && (
            <TabsTrigger value="sms" disabled={!userPhone}>
              <Smartphone className="mr-2 h-4 w-4" />
              SMS
            </TabsTrigger>
          )}
          {availableMethods.includes("authenticator") && (
            <TabsTrigger value="authenticator">
              <Key className="mr-2 h-4 w-4" />
              Authenticator
            </TabsTrigger>
          )}
        </TabsList>

        {availableMethods.includes("email") && (
          <TabsContent value="email" className="mt-4 space-y-4">
            <div className="rounded-lg bg-muted/30 p-4 text-center">
              <p className="text-sm">
                We've sent a verification code to{" "}
                <span className="font-medium">{userEmail ? maskEmail(userEmail) : "your email"}</span>
              </p>
            </div>
          </TabsContent>
        )}

        {availableMethods.includes("sms") && (
          <TabsContent value="sms" className="mt-4 space-y-4">
            <div className="rounded-lg bg-muted/30 p-4 text-center">
              <p className="text-sm">
                We've sent a verification code to{" "}
                <span className="font-medium">{userPhone ? maskPhone(userPhone) : "your phone"}</span>
              </p>
            </div>
          </TabsContent>
        )}

        {availableMethods.includes("authenticator") && (
          <TabsContent value="authenticator" className="mt-4 space-y-4">
            <div className="rounded-lg bg-muted/30 p-4 text-center">
              <p className="text-sm">
                Open your authenticator app and enter the 6-digit code
              </p>
            </div>
          </TabsContent>
        )}
      </Tabs>

      {/* Code Input */}
      <div className="space-y-4">
        <div className="flex justify-center space-x-2">
          {code.map((digit, index) => (
            <input
              key={index}
              ref={(el) => (inputRefs.current[index] = el)}
              type="text"
              inputMode="numeric"
              maxLength={6}
              value={digit}
              onChange={(e) => handleInputChange(index, e.target.value)}
              onKeyDown={(e) => handleKeyDown(index, e)}
              className="h-12 w-12 rounded-md border border-muted-foreground/30 bg-transparent text-center text-xl focus:border-orange-500 focus:outline-none focus:ring-1 focus:ring-orange-500"
            />
          ))}
        </div>

        {/* Resend Code */}
        <div className="text-center">
          <button
            onClick={handleResendCode}
            disabled={resendDisabled}
            className="text-sm text-orange-500 hover:text-orange-600 disabled:text-muted-foreground"
          >
            {resendDisabled
              ? `Resend code in ${countdown}s`
              : "Didn't receive a code? Resend"}
          </button>
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="rounded-lg bg-red-500/10 p-3 text-sm text-red-500">
          <div className="flex items-start">
            <AlertTriangle className="mr-2 mt-0.5 h-4 w-4" />
            <span>{error}</span>
          </div>
        </div>
      )}

      {/* Action Buttons */}
      <div className="flex justify-end space-x-3">
        <Button variant="outline" onClick={onCancel} disabled={isVerifying}>
          Cancel
        </Button>
        <Button
          variant="gradient"
          onClick={handleVerify}
          disabled={isVerifying || code.join("").length !== 6}
          className="min-w-[120px]"
        >
          {isVerifying ? (
            <div className="flex items-center">
              <div className="mr-2 h-4 w-4 animate-spin rounded-full border-2 border-current border-t-transparent"></div>
              Verifying...
            </div>
          ) : (
            <div className="flex items-center">
              <CheckCircle className="mr-2 h-4 w-4" />
              Verify
            </div>
          )}
        </Button>
      </div>
    </div>
  );
}