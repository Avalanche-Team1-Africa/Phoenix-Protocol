"use client";

import React, { useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Select } from "@/components/ui/select";
import { useSecurity } from "@/context/security-provider";
import { useWallet } from "@/context/wallet-context";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { TwoFactorModal } from "@/components/security/two-factor-modal";
import { Shield, Clock, Key, AlertTriangle, Smartphone, CheckCircle, XCircle } from "lucide-react";

export default function SecuritySettingsPage() {
  const { wallet } = useWallet();
  const { 
    isTwoFactorEnabled, 
    sessionTimeout, 
    enableTwoFactor, 
    disableTwoFactor,
    isHardwareWalletConnected,
    connectHardwareWallet,
    disconnectHardwareWallet
  } = useSecurity();
  
  const [isWalletModalOpen, setIsWalletModalOpen] = useState(false);
  const [isTwoFactorModalOpen, setIsTwoFactorModalOpen] = useState(false);
  const [twoFactorAction, setTwoFactorAction] = useState<"enable" | "disable">("enable");
  const [selectedTimeout, setSelectedTimeout] = useState(String(sessionTimeout / (60 * 1000)));
  const [hardwareWalletType, setHardwareWalletType] = useState("ledger");
  const [successMessage, setSuccessMessage] = useState("");
  const [errorMessage, setErrorMessage] = useState("");
  
  // Handle 2FA toggle
  const handleTwoFactorToggle = () => {
    if (isTwoFactorEnabled) {
      setTwoFactorAction("disable");
    } else {
      setTwoFactorAction("enable");
    }
    setIsTwoFactorModalOpen(true);
  };
  
  // Handle session timeout change
  const handleTimeoutChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const minutes = parseInt(e.target.value);
    setSelectedTimeout(e.target.value);
    
    // In a real app, this would update the server setting
    localStorage.setItem("phoenixSessionTimeout", String(minutes * 60 * 1000));
    setSuccessMessage("Session timeout updated successfully");
    setTimeout(() => setSuccessMessage(""), 3000);
  };
  
  // Handle hardware wallet connection
  const handleHardwareWalletToggle = async () => {
    if (!wallet.connected) {
      setIsWalletModalOpen(true);
      return;
    }
    
    try {
      if (isHardwareWalletConnected) {
        disconnectHardwareWallet();
        setSuccessMessage("Hardware wallet disconnected successfully");
      } else {
        const success = await connectHardwareWallet(hardwareWalletType);
        if (success) {
          setSuccessMessage("Hardware wallet connected successfully");
        } else {
          setErrorMessage("Failed to connect hardware wallet");
        }
      }
      setTimeout(() => {
        setSuccessMessage("");
        setErrorMessage("");
      }, 3000);
    } catch (error: any) {
      setErrorMessage(error.message || "An error occurred");
      setTimeout(() => setErrorMessage(""), 3000);
    }
  };
  
  return (
    <div className="container mx-auto py-10 max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Security Settings</h1>
      
      {successMessage && (
        <div className="mb-6 p-4 bg-green-500/10 text-green-500 rounded-md">
          {successMessage}
        </div>
      )}
      
      {errorMessage && (
        <div className="mb-6 p-4 bg-red-500/10 text-red-500 rounded-md">
          {errorMessage}
        </div>
      )}
      
      {!wallet.connected && (
        <Card className="mb-6">
          <CardContent className="py-4">
            <div className="flex items-center justify-between">
              <p className="text-muted-foreground">Connect your wallet to manage security settings</p>
              <Button variant="gradient" onClick={() => setIsWalletModalOpen(true)}>
                Connect Wallet
              </Button>
            </div>
          </CardContent>
        </Card>
      )}
      
      <div className="grid grid-cols-1 gap-6">
        {/* Two-Factor Authentication */}
        <Card>
          <CardHeader className="pb-2">
            <div className="flex items-center">
              <Shield className="h-5 w-5 mr-2 text-orange-500" />
              <CardTitle>Two-Factor Authentication</CardTitle>
            </div>
            <CardDescription>
              Add an extra layer of security to your account
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="mb-4">
              Two-factor authentication adds an additional security layer to your account. 
              When enabled, you'll need to provide a verification code from your authenticator app 
              when performing critical operations.
            </p>
            <div className="flex items-center justify-between p-4 bg-muted rounded-md">
              <div className="flex items-center">
                {isTwoFactorEnabled ? (
                  <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                ) : (
                  <XCircle className="h-5 w-5 text-red-500 mr-2" />
                )}
                <span>
                  {isTwoFactorEnabled ? "Enabled" : "Disabled"}
                </span>
              </div>
              <Button 
                variant={isTwoFactorEnabled ? "outline" : "gradient"}
                onClick={handleTwoFactorToggle}
                disabled={!wallet.connected}
              >
                {isTwoFactorEnabled ? "Disable" : "Enable"} 2FA
              </Button>
            </div>
          </CardContent>
        </Card>
        
        {/* Session Timeout */}
        <Card>
          <CardHeader className="pb-2">
            <div className="flex items-center">
              <Clock className="h-5 w-5 mr-2 text-orange-500" />
              <CardTitle>Session Timeout</CardTitle>
            </div>
            <CardDescription>
              Automatically log out after a period of inactivity
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="mb-4">
              For your security, your session will automatically end after a period of inactivity.
              Choose a timeout duration that balances security and convenience.
            </p>
            <div className="flex items-center space-x-4">
              <div className="w-full max-w-xs">
                <Select 
                  value={selectedTimeout}
                  onChange={handleTimeoutChange}
                  disabled={!wallet.connected}
                >
                  <option value="5">5 minutes</option>
                  <option value="15">15 minutes</option>
                  <option value="30">30 minutes</option>
                  <option value="60">1 hour</option>
                  <option value="120">2 hours</option>
                </Select>
              </div>
            </div>
          </CardContent>
        </Card>
        
        {/* Hardware Wallet */}
        <Card>
          <CardHeader className="pb-2">
            <div className="flex items-center">
              <Key className="h-5 w-5 mr-2 text-orange-500" />
              <CardTitle>Hardware Wallet</CardTitle>
            </div>
            <CardDescription>
              Connect a hardware wallet for enhanced security
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="mb-4">
              Hardware wallets provide the highest level of security by keeping your private keys offline.
              Connect your hardware wallet to sign transactions with maximum protection.
            </p>
            
            <div className="space-y-4">
              <div className="flex items-center justify-between p-4 bg-muted rounded-md">
                <div className="flex items-center">
                  {isHardwareWalletConnected ? (
                    <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                  ) : (
                    <XCircle className="h-5 w-5 text-red-500 mr-2" />
                  )}
                  <span>
                    {isHardwareWalletConnected ? "Connected" : "Not Connected"}
                  </span>
                </div>
                <Button 
                  variant={isHardwareWalletConnected ? "outline" : "gradient"}
                  onClick={handleHardwareWalletToggle}
                  disabled={!wallet.connected}
                >
                  {isHardwareWalletConnected ? "Disconnect" : "Connect"}
                </Button>
              </div>
              
              {!isHardwareWalletConnected && (
                <div className="flex items-center space-x-4">
                  <div className="w-full max-w-xs">
                    <Select 
                      value={hardwareWalletType}
                      onChange={(e) => setHardwareWalletType(e.target.value)}
                      disabled={!wallet.connected || isHardwareWalletConnected}
                    >
                      <option value="ledger">Ledger</option>
                      <option value="trezor">Trezor</option>
                      <option value="gridplus">GridPlus</option>
                    </Select>
                  </div>
                </div>
              )}
            </div>
          </CardContent>
        </Card>
        
        {/* Security Recommendations */}
        <Card>
          <CardHeader className="pb-2">
            <div className="flex items-center">
              <AlertTriangle className="h-5 w-5 mr-2 text-orange-500" />
              <CardTitle>Security Recommendations</CardTitle>
            </div>
            <CardDescription>
              Tips to keep your account secure
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ul className="space-y-2">
              <li className="flex items-start">
                <CheckCircle className="h-4 w-4 text-green-500 mr-2 mt-1" />
                <span>Use a hardware wallet for the highest level of security</span>
              </li>
              <li className="flex items-start">
                <CheckCircle className="h-4 w-4 text-green-500 mr-2 mt-1" />
                <span>Enable two-factor authentication for all critical operations</span>
              </li>
              <li className="flex items-start">
                <CheckCircle className="h-4 w-4 text-green-500 mr-2 mt-1" />
                <span>Never share your recovery phrase or private keys with anyone</span>
              </li>
              <li className="flex items-start">
                <CheckCircle className="h-4 w-4 text-green-500 mr-2 mt-1" />
                <span>Set up social recovery with trusted guardians</span>
              </li>
              <li className="flex items-start">
                <CheckCircle className="h-4 w-4 text-green-500 mr-2 mt-1" />
                <span>Verify all transaction details before signing</span>
              </li>
            </ul>
          </CardContent>
        </Card>
      </div>
      
      <WalletConnectModal 
        isOpen={isWalletModalOpen} 
        onClose={() => setIsWalletModalOpen(false)} 
      />
      
      <TwoFactorModal
        isOpen={isTwoFactorModalOpen}
        onClose={() => setIsTwoFactorModalOpen(false)}
        action={twoFactorAction}
        onSuccess={() => {
          setSuccessMessage(`Two-factor authentication ${twoFactorAction === "enable" ? "enabled" : "disabled"} successfully`);
          setTimeout(() => setSuccessMessage(""), 3000);
        }}
        onError={(message) => {
          setErrorMessage(message || "An error occurred");
          setTimeout(() => setErrorMessage(""), 3000);
        }}
      />
    </div>
  );
}