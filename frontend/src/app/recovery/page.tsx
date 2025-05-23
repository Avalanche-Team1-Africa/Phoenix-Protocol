"use client";

import React, { useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Alert } from "@/components/ui/alert";
import { Shield, Clock, AlertTriangle, Users, Key, RefreshCw, CheckCircle, XCircle, Info } from "lucide-react";
import { useWallet } from "@/context/wallet-context";

export default function RecoveryPage() {
  const [activeTab, setActiveTab] = useState("setup");
  const [guardianAddress, setGuardianAddress] = useState("");
  const [guardianName, setGuardianName] = useState("");
  const [guardianEmail, setGuardianEmail] = useState("");
  const [guardianThreshold, setGuardianThreshold] = useState(2);
  const [totalGuardians, setTotalGuardians] = useState(3);
  const [showInfoModal, setShowInfoModal] = useState(false);
  
  // Mock data for guardians
  const [guardians, setGuardians] = useState([
    { address: "0x1234...5678", name: "Alice (Friend)", email: "alice@example.com" },
    { address: "0xabcd...efgh", name: "Bob (Family)", email: "bob@example.com" }
  ]);
  
  const handleAddGuardian = () => {
    if (guardianAddress && (guardianName || guardianEmail)) {
      setGuardians([...guardians, { 
        address: guardianAddress, 
        name: guardianName || "Unnamed Guardian", 
        email: guardianEmail || "" 
      }]);
      setGuardianAddress("");
      setGuardianName("");
      setGuardianEmail("");
    }
  };
  
  const handleRemoveGuardian = (index) => {
    const newGuardians = [...guardians];
    newGuardians.splice(index, 1);
    setGuardians(newGuardians);
  };
  
  return (
    <div className="container mx-auto py-10 max-w-5xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-3xl font-bold">Wallet Recovery System</h1>
        <Button 
          variant="outline" 
          className="flex items-center gap-2"
          onClick={() => setShowInfoModal(!showInfoModal)}
        >
          <Info size={16} />
          How It Works
        </Button>
      </div>
      
      {showInfoModal && (
        <Card className="mb-8 border-orange-500/50">
          <CardHeader className="pb-2">
            <CardTitle className="flex items-center gap-2">
              <Shield className="h-5 w-5 text-orange-500" />
              How Phoenix Protocol Wallet Recovery Works
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4 text-sm">
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="p-4 bg-muted rounded-md">
                <h3 className="font-medium mb-2 flex items-center gap-2">
                  <Users className="h-4 w-4 text-orange-500" />
                  Social Recovery via Guardians
                </h3>
                <p className="text-muted-foreground mb-2">
                  Appoint trusted guardians (friends, family, or organizations) who can help recover your wallet if you lose access.
                </p>
                <ul className="list-disc pl-4 space-y-1 text-xs">
                  <li>Guardians don't have access to your wallet or private key</li>
                  <li>They only verify your identity during recovery</li>
                  <li>A threshold of guardians (e.g., 3 of 5) must approve recovery</li>
                </ul>
              </div>
              
              <div className="p-4 bg-muted rounded-md">
                <h3 className="font-medium mb-2 flex items-center gap-2">
                  <RefreshCw className="h-4 w-4 text-orange-500" />
                  Intent-Based Verification
                </h3>
                <p className="text-muted-foreground mb-2">
                  All transactions are logged with your intent before execution, enabling:
                </p>
                <ul className="list-disc pl-4 space-y-1 text-xs">
                  <li>Detection of mismatches between intent and execution</li>
                  <li>Rollback of transactions if something goes wrong</li>
                  <li>Protection against malicious frontends or accidental transactions</li>
                </ul>
              </div>
              
              <div className="p-4 bg-muted rounded-md">
                <h3 className="font-medium mb-2 flex items-center gap-2">
                  <Users className="h-4 w-4 text-orange-500" />
                  Dispute Resolution DAO
                </h3>
                <p className="text-muted-foreground mb-2">
                  If there's a dispute about recovery:
                </p>
                <ul className="list-disc pl-4 space-y-1 text-xs">
                  <li>Both parties can submit evidence</li>
                  <li>DAO members vote based on verifiable inputs</li>
                  <li>Fraudulent recovery attempts are penalized</li>
                  <li>Provides a human-layer safeguard against abuse</li>
                </ul>
              </div>
            </div>
            
            <div className="p-4 bg-green-500/10 rounded-md">
              <h3 className="font-medium mb-2 text-green-500">Security Measures</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div>
                  <p className="text-xs mb-1 font-medium">Guardian Thresholds & Time Delays</p>
                  <p className="text-xs text-muted-foreground">Multiple guardians required with time delays to prevent unauthorized recovery</p>
                </div>
                <div>
                  <p className="text-xs mb-1 font-medium">Cryptographic Proofs</p>
                  <p className="text-xs text-muted-foreground">All guardian votes are signed messages verified on-chain</p>
                </div>
                <div>
                  <p className="text-xs mb-1 font-medium">Event Monitoring</p>
                  <p className="text-xs text-muted-foreground">System watches for abnormal recovery requests</p>
                </div>
                <div>
                  <p className="text-xs mb-1 font-medium">DAO Governance</p>
                  <p className="text-xs text-muted-foreground">Ultimate arbiter that can reverse malicious recoveries</p>
                </div>
              </div>
            </div>
            
            <Alert className="bg-orange-500/10 text-orange-500 border-orange-500/20">
              <p className="text-xs font-medium">Important: No private keys or seed phrases are ever exposed or transferred during recovery</p>
              <p className="text-xs">Phoenix Protocol only reassigns wallet control through on-chain logic</p>
            </Alert>
          </CardContent>
        </Card>
      )}
      
      <Tabs defaultValue="setup" value={activeTab} onValueChange={setActiveTab} className="w-full">
        <TabsList className="grid grid-cols-3 mb-8">
          <TabsTrigger value="setup" className="flex items-center gap-2">
            <Shield className="h-4 w-4" />
            Guardian Setup
          </TabsTrigger>
          <TabsTrigger value="recover" className="flex items-center gap-2">
            <Key className="h-4 w-4" />
            Recover Wallet
          </TabsTrigger>
          <TabsTrigger value="guardian" className="flex items-center gap-2">
            <Users className="h-4 w-4" />
            Guardian Actions
          </TabsTrigger>
        </TabsList>
        
        <TabsContent value="setup">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="md:col-span-2">
              <Card>
                <CardHeader>
                  <CardTitle>Social Guardian Setup</CardTitle>
                  <CardDescription>
                    Appoint trusted contacts who can help recover your wallet if you lose access
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <Alert className="bg-blue-500/10 border-blue-500/20 mb-4">
                    <div className="flex items-start gap-2">
                      <Info className="h-4 w-4 text-blue-500 mt-0.5" />
                      <div>
                        <p className="text-sm font-medium text-blue-500">How Guardian Recovery Works</p>
                        <p className="text-xs text-muted-foreground">
                          Guardians can't access your wallet, but they can verify your identity to help you regain access if you lose your keys.
                          You'll need {guardianThreshold} of {totalGuardians} guardians to approve any recovery request.
                        </p>
                      </div>
                    </div>
                  </Alert>
                  
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
                  
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div className="space-y-2">
                      <label htmlFor="guardianThreshold" className="text-sm font-medium">
                        Required Approvals
                      </label>
                      <select 
                        id="guardianThreshold"
                        className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                        value={guardianThreshold}
                        onChange={(e) => setGuardianThreshold(parseInt(e.target.value))}
                      >
                        <option value="1">1 Guardian</option>
                        <option value="2">2 Guardians</option>
                        <option value="3">3 Guardians</option>
                        <option value="4">4 Guardians</option>
                        <option value="5">5 Guardians</option>
                      </select>
                    </div>
                    
                    <div className="space-y-2">
                      <label htmlFor="totalGuardians" className="text-sm font-medium">
                        Total Guardians
                      </label>
                      <select 
                        id="totalGuardians"
                        className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                        value={totalGuardians}
                        onChange={(e) => setTotalGuardians(parseInt(e.target.value))}
                      >
                        <option value="1">1 Guardian</option>
                        <option value="2">2 Guardians</option>
                        <option value="3">3 Guardians</option>
                        <option value="4">4 Guardians</option>
                        <option value="5">5 Guardians</option>
                        <option value="7">7 Guardians</option>
                      </select>
                    </div>
                  </div>
                  
                  <div className="pt-4 border-t">
                    <h4 className="text-sm font-medium mb-4">Add New Guardian</h4>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="space-y-2">
                        <label htmlFor="guardianAddress" className="text-sm font-medium">
                          Guardian Wallet Address
                        </label>
                        <Input 
                          id="guardianAddress" 
                          type="text" 
                          placeholder="0x..." 
                          value={guardianAddress}
                          onChange={(e) => setGuardianAddress(e.target.value)}
                        />
                      </div>
                      
                      <div className="space-y-2">
                        <label htmlFor="guardianName" className="text-sm font-medium">
                          Guardian Name/Relationship (Optional)
                        </label>
                        <Input 
                          id="guardianName" 
                          type="text" 
                          placeholder="e.g., Alice (Friend)" 
                          value={guardianName}
                          onChange={(e) => setGuardianName(e.target.value)}
                        />
                      </div>
                      
                      <div className="space-y-2 md:col-span-2">
                        <label htmlFor="guardianEmail" className="text-sm font-medium">
                          Guardian Email (Optional, for notifications only)
                        </label>
                        <Input 
                          id="guardianEmail" 
                          type="email" 
                          placeholder="guardian@example.com" 
                          value={guardianEmail}
                          onChange={(e) => setGuardianEmail(e.target.value)}
                        />
                        <p className="text-xs text-muted-foreground">
                          Email is stored encrypted and only used to notify your guardian of recovery requests
                        </p>
                      </div>
                    </div>
                    
                    <Button 
                      variant="default" 
                      className="w-full mt-4"
                      onClick={handleAddGuardian}
                      disabled={!guardianAddress}
                    >
                      Add Guardian
                    </Button>
                  </div>
                  
                  <div className="pt-4">
                    <div className="flex justify-between items-center mb-2">
                      <h4 className="text-sm font-medium">Current Guardians</h4>
                      <span className="text-xs text-muted-foreground">
                        {guardians.length} of {totalGuardians} added
                      </span>
                    </div>
                    
                    {guardians.length === 0 ? (
                      <div className="p-4 bg-muted rounded-md text-center text-sm text-muted-foreground">
                        No guardians added yet
                      </div>
                    ) : (
                      <div className="space-y-2">
                        {guardians.map((guardian, index) => (
                          <div key={index} className="flex justify-between items-center p-3 bg-muted rounded-md">
                            <div className="flex flex-col">
                              <span className="font-mono text-sm">{guardian.address}</span>
                              {guardian.name && (
                                <span className="text-xs text-muted-foreground">{guardian.name}</span>
                              )}
                            </div>
                            <Button 
                              variant="ghost" 
                              size="sm"
                              onClick={() => handleRemoveGuardian(index)}
                            >
                              Remove
                            </Button>
                          </div>
                        ))}
                      </div>
                    )}
                  </div>
                </CardContent>
                <CardFooter>
                  <Button variant="gradient" className="w-full">
                    Update Guardian Configuration
                  </Button>
                </CardFooter>
              </Card>
            </div>
            
            <div>
              <Card>
                <CardHeader>
                  <CardTitle>Recovery Settings</CardTitle>
                  <CardDescription>
                    Configure additional security parameters
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="space-y-2">
                    <label htmlFor="cooldownPeriod" className="text-sm font-medium">
                      Recovery Cooldown Period
                    </label>
                    <select 
                      id="cooldownPeriod"
                      className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                    >
                      <option value="24">24 hours</option>
                      <option value="48">48 hours</option>
                      <option value="72" selected>72 hours</option>
                      <option value="168">7 days</option>
                    </select>
                    <p className="text-xs text-muted-foreground">
                      Time delay before recovery takes effect, allowing you to cancel if unauthorized
                    </p>
                  </div>
                  
                  <div className="space-y-2">
                    <label htmlFor="notificationMethod" className="text-sm font-medium">
                      Recovery Notifications
                    </label>
                    <div className="space-y-2">
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="emailNotif" className="rounded border-gray-300" checked />
                        <label htmlFor="emailNotif" className="text-sm">Email notifications</label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="pushNotif" className="rounded border-gray-300" checked />
                        <label htmlFor="pushNotif" className="text-sm">Push notifications</label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="guardianNotif" className="rounded border-gray-300" checked />
                        <label htmlFor="guardianNotif" className="text-sm">Notify guardians</label>
                      </div>
                    </div>
                  </div>
                  
                  <div className="pt-4 border-t">
                    <h4 className="text-sm font-medium mb-2">Advanced Security</h4>
                    <div className="space-y-4">
                      <div className="flex items-center justify-between">
                        <div>
                          <p className="text-sm">Guardian Rotation</p>
                          <p className="text-xs text-muted-foreground">Periodically change guardians</p>
                        </div>
                        <select 
                          className="h-8 rounded-md border border-input bg-background px-2 py-1 text-xs"
                        >
                          <option value="never">Never</option>
                          <option value="6months">Every 6 months</option>
                          <option value="1year">Yearly</option>
                        </select>
                      </div>
                      
                      <div className="flex items-center justify-between">
                        <div>
                          <p className="text-sm">DAO Oversight</p>
                          <p className="text-xs text-muted-foreground">Allow DAO to review recovery</p>
                        </div>
                        <div className="flex h-6 items-center">
                          <input type="checkbox" id="daoOversight" className="rounded border-gray-300" checked />
                        </div>
                      </div>
                      
                      <div className="flex items-center justify-between">
                        <div>
                          <p className="text-sm">Anomaly Detection</p>
                          <p className="text-xs text-muted-foreground">AI-driven security monitoring</p>
                        </div>
                        <div className="flex h-6 items-center">
                          <input type="checkbox" id="anomalyDetection" className="rounded border-gray-300" checked />
                        </div>
                      </div>
                    </div>
                  </div>
                </CardContent>
                <CardFooter>
                  <Button variant="outline" className="w-full">
                    Save Settings
                  </Button>
                </CardFooter>
              </Card>
            </div>
          </div>
        </TabsContent>
        
        <TabsContent value="recover">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            <Card>
              <CardHeader>
                <CardTitle>Recover Wallet Access</CardTitle>
                <CardDescription>
                  Initiate the social recovery process for a lost wallet
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <Alert className="bg-orange-500/10 border-orange-500/20 mb-4">
                  <div className="flex items-start gap-2">
                    <AlertTriangle className="h-4 w-4 text-orange-500 mt-0.5" />
                    <div>
                      <p className="text-sm font-medium text-orange-500">Important Recovery Information</p>
                      <p className="text-xs text-muted-foreground">
                        This process will transfer control of your wallet to a new address. 
                        A {guardianThreshold}-of-{totalGuardians} guardian approval is required, 
                        followed by a 72-hour security delay.
                      </p>
                    </div>
                  </div>
                </Alert>
                
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
                  <p className="text-xs text-muted-foreground">
                    This is the address that will gain control of your assets after recovery
                  </p>
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
                
                <div className="space-y-2">
                  <label htmlFor="recoveryDetails" className="text-sm font-medium">
                    Additional Details
                  </label>
                  <textarea 
                    id="recoveryDetails" 
                    className="flex min-h-[80px] w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                    placeholder="Provide any additional information that might help your guardians verify your identity..."
                  ></textarea>
                </div>
                
                <div className="space-y-2">
                  <label htmlFor="identityVerification" className="text-sm font-medium">
                    Identity Verification
                  </label>
                  <div className="p-4 bg-muted rounded-md">
                    <p className="text-sm mb-2">Select verification method(s):</p>
                    <div className="space-y-2">
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="verifyEmail" className="rounded border-gray-300" />
                        <label htmlFor="verifyEmail" className="text-sm">Email verification</label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="verifyVideo" className="rounded border-gray-300" />
                        <label htmlFor="verifyVideo" className="text-sm">Video verification</label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <input type="checkbox" id="verifyDocument" className="rounded border-gray-300" />
                        <label htmlFor="verifyDocument" className="text-sm">Document upload</label>
                      </div>
                    </div>
                  </div>
                </div>
                
                <Button variant="gradient" className="w-full">
                  Initiate Recovery Process
                </Button>
              </CardContent>
            </Card>
            
            <Card>
              <CardHeader>
                <CardTitle>Active Recovery Requests</CardTitle>
                <CardDescription>
                  Track the status of your recovery requests
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="p-4 bg-yellow-500/10 rounded-md">
                  <div className="flex justify-between items-start mb-2">
                    <div>
                      <h4 className="text-sm font-medium">Recovery ID: #R-8721</h4>
                      <p className="text-xs text-muted-foreground">Initiated 2 days ago</p>
                    </div>
                    <span className="px-2 py-1 bg-yellow-500/20 text-yellow-500 rounded-full text-xs font-medium">
                      In Progress
                    </span>
                  </div>
                  
                  <div className="space-y-4">
                    <div>
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
                          <span className="text-green-500 flex items-center gap-1">
                            <CheckCircle className="h-3 w-3" /> Approved
                          </span>
                        </div>
                        <div className="flex justify-between">
                          <span>0xabcd...efgh</span>
                          <span className="text-yellow-500 flex items-center gap-1">
                            <Clock className="h-3 w-3" /> Pending
                          </span>
                        </div>
                        <div className="flex justify-between">
                          <span>0x9876...5432</span>
                          <span className="text-yellow-500 flex items-center gap-1">
                            <Clock className="h-3 w-3" /> Pending
                          </span>
                        </div>
                      </div>
                    </div>
                    
                    <div>
                      <div className="flex justify-between mb-2">
                        <span className="text-sm font-medium">Cooldown Period</span>
                        <span className="text-sm">70 hours remaining</span>
                      </div>
                      <div className="w-full bg-background rounded-full h-2.5">
                        <div 
                          className="bg-blue-500 h-2.5 rounded-full" 
                          style={{ width: '3%' }}
                        ></div>
                      </div>
                    </div>
                    
                    <div className="pt-2 text-xs text-muted-foreground">
                      <p>After guardian approval, a 72-hour security delay begins before recovery completes.</p>
                    </div>
                  </div>
                </div>
                
                <div className="p-4 bg-red-500/10 rounded-md">
                  <div className="flex justify-between items-start mb-2">
                    <div>
                      <h4 className="text-sm font-medium">Recovery ID: #R-8532</h4>
                      <p className="text-xs text-muted-foreground">Initiated 5 days ago</p>
                    </div>
                    <span className="px-2 py-1 bg-red-500/20 text-red-500 rounded-full text-xs font-medium">
                      Disputed
                    </span>
                  </div>
                  
                  <div className="space-y-2 text-sm">
                    <p className="text-red-500 text-xs">
                      This recovery request has been disputed and is under review by the DAO.
                    </p>
                    <Button variant="outline" size="sm" className="w-full text-xs">
                      View Dispute Details
                    </Button>
                  </div>
                </div>
                
                <div className="p-4 bg-green-500/10 rounded-md">
                  <div className="flex justify-between items-start mb-2">
                    <div>
                      <h4 className="text-sm font-medium">Recovery ID: #R-7921</h4>
                      <p className="text-xs text-muted-foreground">Completed 2 weeks ago</p>
                    </div>
                    <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs font-medium">
                      Completed
                    </span>
                  </div>
                  
                  <div className="space-y-2 text-sm">
                    <div className="flex justify-between">
                      <span className="text-muted-foreground">Recovered Wallet:</span>
                      <span className="font-mono">0x7F5E...62CB</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-muted-foreground">New Wallet:</span>
                      <span className="font-mono">0x3A1F...D45E</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-muted-foreground">Transaction Hash:</span>
                      <a href="#" className="text-blue-500 underline">View</a>
                    </div>
                  </div>
                </div>
              </CardContent>
              <CardFooter className="flex justify-between">
                <Button variant="outline">
                  Cancel Active Recovery
                </Button>
                <Button variant="default">
                  Contact Guardians
                </Button>
              </CardFooter>
            </Card>
          </div>
        </TabsContent>
        
        <TabsContent value="guardian">
          <div className="grid grid-cols-1 gap-8">
            <Card>
              <CardHeader>
                <CardTitle>Guardian Dashboard</CardTitle>
                <CardDescription>
                  Manage recovery requests for wallets where you are a guardian
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                <Alert className="bg-blue-500/10 border-blue-500/20 mb-4">
                  <div className="flex items-start gap-2">
                    <Info className="h-4 w-4 text-blue-500 mt-0.5" />
                    <div>
                      <p className="text-sm font-medium text-blue-500">Your Role as a Guardian</p>
                      <p className="text-xs text-muted-foreground">
                        As a guardian, you help verify the identity of users who have lost access to their wallets.
                        Your approval is critical to the security of the recovery process.
                        Always verify the identity of the person requesting recovery before approving.
                      </p>
                    </div>
                  </div>
                </Alert>
                
                <div className="space-y-2">
                  <h3 className="text-lg font-medium">Pending Recovery Requests</h3>
                  
                  <Card className="border-yellow-500/50">
                    <CardHeader className="pb-2">
                      <div className="flex justify-between items-start">
                        <CardTitle className="text-base">Recovery Request #R-9103</CardTitle>
                        <span className="px-2 py-1 bg-yellow-500/20 text-yellow-500 rounded-full text-xs font-medium">
                          Awaiting Approval
                        </span>
                      </div>
                      <CardDescription>
                        Requested 6 hours ago
                      </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
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
                            <span className="text-muted-foreground">Guardian Approvals:</span>
                            <span>1 of 3</span>
                          </div>
                        </div>
                      </div>
                      
                      <div className="space-y-2">
                        <h4 className="text-sm font-medium">Verification Methods</h4>
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-2">
                          <Button variant="outline" size="sm" className="text-xs">
                            View Video Verification
                          </Button>
                          <Button variant="outline" size="sm" className="text-xs">
                            View Documents
                          </Button>
                          <Button variant="outline" size="sm" className="text-xs">
                            Contact Requester
                          </Button>
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
                      <Button variant="outline" className="border-red-500 text-red-500 hover:bg-red-500/10">
                        Reject Request
                      </Button>
                      <Button variant="gradient">
                        Approve Recovery
                      </Button>
                    </CardFooter>
                  </Card>
                </div>
                
                <div className="space-y-2">
                  <h3 className="text-lg font-medium">Your Guardian Status</h3>
                  
                  <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm text-muted-foreground">Wallets Protected</CardTitle>
                      </CardHeader>
                      <CardContent>
                        <p className="text-3xl font-bold">5</p>
                      </CardContent>
                    </Card>
                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm text-muted-foreground">Recoveries Approved</CardTitle>
                      </CardHeader>
                      <CardContent>
                        <p className="text-3xl font-bold text-green-500">3</p>
                      </CardContent>
                    </Card>
                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm text-muted-foreground">Pending Requests</CardTitle>
                      </CardHeader>
                      <CardContent>
                        <p className="text-3xl font-bold text-yellow-500">1</p>
                      </CardContent>
                    </Card>
                  </div>
                </div>
                
                <div className="space-y-2">
                  <h3 className="text-lg font-medium">Wallets You Protect</h3>
                  
                  <div className="space-y-2">
                    <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                      <div className="flex flex-col">
                        <span className="font-mono text-sm">0x8765...4321</span>
                        <span className="text-xs text-muted-foreground">John D. (Friend)</span>
                      </div>
                      <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs">
                        Active
                      </span>
                    </div>
                    <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                      <div className="flex flex-col">
                        <span className="font-mono text-sm">0xfedc...ba98</span>
                        <span className="text-xs text-muted-foreground">Sarah M. (Family)</span>
                      </div>
                      <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs">
                        Active
                      </span>
                    </div>
                    <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                      <div className="flex flex-col">
                        <span className="font-mono text-sm">0xaaaa...bbbb</span>
                        <span className="text-xs text-muted-foreground">Work DAO</span>
                      </div>
                      <span className="px-2 py-1 bg-green-500/20 text-green-500 rounded-full text-xs">
                        Active
                      </span>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>
      </Tabs>
    </div>
  );
}