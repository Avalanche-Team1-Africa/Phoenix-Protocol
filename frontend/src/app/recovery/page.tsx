import React from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";

export default function RecoveryPage() {
  return (
    <div className="container mx-auto py-10 max-w-4xl">
      <h1 className="text-3xl font-bold mb-6">Wallet Recovery</h1>
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
        {/* Guardian Setup */}
        <Card>
          <CardHeader>
            <CardTitle>Guardian Setup</CardTitle>
            <CardDescription>
              Assign trusted contacts who can help recover your wallet
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <label htmlFor="walletAddress" className="text-sm font-medium">
                Your Wallet Address
              </label>
              <Input 
                id="walletAddress" 
                type="text" 
                placeholder="0x..." 
                value="0x7F5EB5bB5cF88cfcEe9613368636f458800e62CB"
                readOnly
              />
            </div>
            
            <div className="space-y-2">
              <div className="flex justify-between">
                <label htmlFor="guardianAddress" className="text-sm font-medium">
                  Guardian Address
                </label>
                <span className="text-xs text-muted-foreground">
                  2 of 3 guardians required for recovery
                </span>
              </div>
              <Input 
                id="guardianAddress" 
                type="text" 
                placeholder="0x..." 
              />
            </div>
            
            <Button variant="default" className="w-full">
              Add Guardian
            </Button>
            
            <div className="pt-4">
              <h4 className="text-sm font-medium mb-2">Current Guardians</h4>
              <div className="space-y-2">
                <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                  <span className="font-mono text-sm">0x1234...5678</span>
                  <Button variant="ghost" size="sm">Remove</Button>
                </div>
                <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                  <span className="font-mono text-sm">0xabcd...efgh</span>
                  <Button variant="ghost" size="sm">Remove</Button>
                </div>
              </div>
            </div>
          </CardContent>
          <CardFooter>
            <Button variant="gradient" className="w-full">
              Update Guardian Configuration
            </Button>
          </CardFooter>
        </Card>
        
        {/* Recovery Process */}
        <Card>
          <CardHeader>
            <CardTitle>Recover Wallet</CardTitle>
            <CardDescription>
              Initiate the recovery process for a lost wallet
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <label htmlFor="lostWalletAddress" className="text-sm font-medium">
                Wallet Address to Recover
              </label>
              <Input 
                id="lostWalletAddress" 
                type="text" 
                placeholder="0x..." 
              />
            </div>
            
            <div className="space-y-2">
              <label htmlFor="newWalletAddress" className="text-sm font-medium">
                New Wallet Address
              </label>
              <Input 
                id="newWalletAddress" 
                type="text" 
                placeholder="0x..." 
              />
            </div>
            
            <div className="space-y-2">
              <label htmlFor="recoveryReason" className="text-sm font-medium">
                Reason for Recovery
              </label>
              <select 
                id="recoveryReason" 
                className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
              >
                <option value="lost">Lost Private Key</option>
                <option value="compromised">Compromised Wallet</option>
                <option value="hardware">Hardware Failure</option>
                <option value="other">Other</option>
              </select>
            </div>
            
            <Button variant="default" className="w-full">
              Initiate Recovery
            </Button>
            
            <div className="pt-4">
              <h4 className="text-sm font-medium mb-2">Recovery Status</h4>
              <div className="p-4 bg-yellow-500/10 rounded-md">
                <div className="flex justify-between mb-2">
                  <span className="text-sm font-medium">Guardian Approvals</span>
                  <span className="text-sm">1 of 3</span>
                </div>
                <div className="w-full bg-background rounded-full h-2.5 mb-4">
                  <div 
                    className="bg-yellow-500 h-2.5 rounded-full" 
                    style={{ width: '33%' }}
                  ></div>
                </div>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span>0x1234...5678</span>
                    <span className="text-green-500">Approved</span>
                  </div>
                  <div className="flex justify-between">
                    <span>0xabcd...efgh</span>
                    <span className="text-yellow-500">Pending</span>
                  </div>
                  <div className="flex justify-between">
                    <span>0x9876...5432</span>
                    <span className="text-yellow-500">Pending</span>
                  </div>
                </div>
              </div>
            </div>
          </CardContent>
          <CardFooter className="flex justify-between">
            <Button variant="outline">
              Cancel Recovery
            </Button>
            <Button variant="gradient" disabled>
              Complete Recovery
            </Button>
          </CardFooter>
        </Card>
      </div>
      
      {/* Threshold Signature Section */}
      <div className="mt-10">
        <h2 className="text-2xl font-bold mb-4">Guardian Login</h2>
        <Card>
          <CardHeader>
            <CardTitle>Approve Recovery Request</CardTitle>
            <CardDescription>
              Verify and approve a wallet recovery request as a guardian
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <label htmlFor="recoveryId" className="text-sm font-medium">
                Recovery Request ID
              </label>
              <Input 
                id="recoveryId" 
                type="text" 
                placeholder="Enter recovery ID" 
              />
            </div>
            
            <div className="p-4 bg-muted rounded-md">
              <h4 className="text-sm font-medium mb-2">Recovery Details</h4>
              <div className="space-y-2 text-sm">
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Wallet to Recover:</span>
                  <span className="font-mono">0x7F5E...62CB</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">New Wallet:</span>
                  <span className="font-mono">0x3A1F...D45E</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Initiated By:</span>
                  <span className="font-mono">0x1234...5678</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Reason:</span>
                  <span>Lost Private Key</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Requested:</span>
                  <span>2 hours ago</span>
                </div>
              </div>
            </div>
            
            <div className="space-y-2">
              <label htmlFor="guardianSignature" className="text-sm font-medium">
                Guardian Signature
              </label>
              <Input 
                id="guardianSignature" 
                type="text" 
                placeholder="Sign with your wallet" 
              />
              <p className="text-xs text-muted-foreground">
                This will require a signature from your wallet to verify your identity as a guardian.
              </p>
            </div>
          </CardContent>
          <CardFooter className="flex justify-between">
            <Button variant="outline">
              Reject Request
            </Button>
            <Button variant="gradient">
              Approve Recovery
            </Button>
          </CardFooter>
        </Card>
      </div>
    </div>
  );
}