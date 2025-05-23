"use client";

import React, { useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Alert } from "@/components/ui/alert";
import { DisputeForm } from "@/components/disputes/dispute-form";
import { TransactionVerifier } from "@/components/intent/transaction-verifier";
import { Shield, Users, AlertTriangle, Info, CheckCircle, XCircle, FileText, Vote } from "lucide-react";

export default function DisputesPage() {
  const [activeTab, setActiveTab] = useState("dashboard");
  const [showDisputeForm, setShowDisputeForm] = useState(false);
  
  // Mock data for disputes
  const disputes = [
    {
      id: "dispute-001",
      title: "Token Swap Slippage Exceeded",
      description: "User intended 1% max slippage but received 15% slippage on DEX swap",
      status: "Under Review",
      date: "2 days ago",
      evidence: [
        "Transaction hash: 0x1234...5678",
        "Intent hash: 0xabcd...efgh",
        "Slippage in transaction: 15%",
        "Slippage in intent: 1%"
      ],
      votes: {
        infavor: 7,
        against: 2
      }
    },
    {
      id: "dispute-002",
      title: "NFT Purchase Price Mismatch",
      description: "User was charged 2.5 ETH instead of advertised 0.25 ETH for NFT mint",
      status: "Resolved",
      date: "3 days ago",
      evidence: [
        "Transaction hash: 0x8765...4321",
        "Intent hash: 0xfedc...ba98",
        "Price in transaction: 2.5 ETH",
        "Price in intent: 0.25 ETH",
        "UI screenshot showing 0.25 ETH price"
      ],
      votes: {
        infavor: 12,
        against: 1
      },
      resolution: "Refund of 2.25 ETH approved"
    },
    {
      id: "dispute-003",
      title: "Unauthorized Token Transfer",
      description: "User claims they did not authorize a transfer of 1000 USDC",
      status: "Pending Evidence",
      date: "1 day ago",
      evidence: [
        "Transaction hash: 0xaaaa...bbbb",
        "No matching intent hash found"
      ],
      votes: {
        infavor: 0,
        against: 0
      }
    },
    {
      id: "dispute-004",
      title: "Contested Wallet Recovery",
      description: "User disputes recovery request claiming they did not lose access to their wallet",
      status: "Under Review",
      date: "4 days ago",
      evidence: [
        "Recovery ID: R-8532",
        "Original wallet: 0x7F5E...62CB",
        "New wallet: 0x4B2C...F67D",
        "Guardian approvals: 2 of 3"
      ],
      votes: {
        infavor: 5,
        against: 8
      }
    }
  ];

  return (
    <div className="container mx-auto py-10 max-w-6xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-3xl font-bold">Dispute Resolution DAO</h1>
        <div className="flex gap-2">
          {!showDisputeForm && activeTab === "dashboard" && (
            <Button 
              variant="gradient"
              onClick={() => setShowDisputeForm(true)}
            >
              Submit New Dispute
            </Button>
          )}
          {showDisputeForm && activeTab === "dashboard" && (
            <Button 
              variant="outline"
              onClick={() => setShowDisputeForm(false)}
            >
              Back to Dashboard
            </Button>
          )}
        </div>
      </div>
      
      <Tabs defaultValue="dashboard" value={activeTab} onValueChange={setActiveTab} className="w-full">
        <TabsList className="grid grid-cols-3 mb-8">
          <TabsTrigger value="dashboard" className="flex items-center gap-2">
            <Shield className="h-4 w-4" />
            Dispute Dashboard
          </TabsTrigger>
          <TabsTrigger value="verify" className="flex items-center gap-2">
            <AlertTriangle className="h-4 w-4" />
            Verify Transactions
          </TabsTrigger>
          <TabsTrigger value="governance" className="flex items-center gap-2">
            <Users className="h-4 w-4" />
            DAO Governance
          </TabsTrigger>
        </TabsList>
        
        <TabsContent value="dashboard">
          {showDisputeForm ? (
            <DisputeForm />
          ) : (
            <>
              <Alert className="bg-blue-500/10 border-blue-500/20 mb-8">
                <div className="flex items-start gap-2">
                  <Info className="h-4 w-4 text-blue-500 mt-0.5" />
                  <div>
                    <p className="text-sm font-medium text-blue-500">About the Dispute Resolution DAO</p>
                    <p className="text-xs text-muted-foreground">
                      The Dispute Resolution DAO serves as the ultimate arbiter for contested transactions and recovery requests.
                      DAO members review evidence, vote on outcomes, and can reverse malicious transactions or recovery attempts.
                      This provides a human-layer governance safeguard against abuse of the Phoenix Protocol.
                    </p>
                  </div>
                </div>
              </Alert>
              
              {/* Dispute Stats */}
              <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm text-muted-foreground">Total Disputes</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-3xl font-bold">127</p>
                  </CardContent>
                </Card>
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm text-muted-foreground">Resolved</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-3xl font-bold text-green-500">98</p>
                  </CardContent>
                </Card>
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm text-muted-foreground">Under Review</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-3xl font-bold text-yellow-500">24</p>
                  </CardContent>
                </Card>
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm text-muted-foreground">Rejected</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-3xl font-bold text-red-500">5</p>
                  </CardContent>
                </Card>
              </div>
              
              {/* Disputes List */}
              <div className="space-y-6">
                <div className="flex justify-between items-center">
                  <h2 className="text-2xl font-bold">Open Cases</h2>
                  <div className="flex items-center gap-2">
                    <span className="text-sm text-muted-foreground">Filter:</span>
                    <select className="text-sm border rounded-md px-2 py-1">
                      <option value="all">All Cases</option>
                      <option value="transaction">Transaction Disputes</option>
                      <option value="recovery">Recovery Disputes</option>
                      <option value="review">Under Review</option>
                      <option value="resolved">Resolved</option>
                    </select>
                  </div>
                </div>
                
                {disputes.map((dispute) => (
                  <Card key={dispute.id} className={dispute.status === "Resolved" ? "border-green-500/50" : ""}>
                    <CardHeader>
                      <div className="flex justify-between items-start">
                        <div>
                          <CardTitle>{dispute.title}</CardTitle>
                          <CardDescription>{dispute.description}</CardDescription>
                        </div>
                        <div className="flex flex-col items-end">
                          <span className={`px-3 py-1 rounded-full text-xs font-medium flex items-center gap-1 ${
                            dispute.status === "Resolved" ? "bg-green-500/10 text-green-500" : 
                            dispute.status === "Under Review" ? "bg-yellow-500/10 text-yellow-500" : 
                            "bg-blue-500/10 text-blue-500"
                          }`}>
                            {dispute.status === "Resolved" ? <CheckCircle className="h-3 w-3" /> : 
                             dispute.status === "Under Review" ? <AlertTriangle className="h-3 w-3" /> : 
                             <Info className="h-3 w-3" />}
                            {dispute.status}
                          </span>
                          <span className="text-xs text-muted-foreground mt-1">{dispute.date}</span>
                        </div>
                      </div>
                    </CardHeader>
                    <CardContent>
                      <div className="space-y-4">
                        <div>
                          <h4 className="text-sm font-medium mb-2 flex items-center gap-1">
                            <FileText className="h-4 w-4 text-orange-500" /> Evidence
                          </h4>
                          <ul className="space-y-1 text-sm bg-muted p-3 rounded-md font-mono">
                            {dispute.evidence.map((item, index) => (
                              <li key={index}>{item}</li>
                            ))}
                          </ul>
                        </div>
                        
                        {dispute.resolution && (
                          <div>
                            <h4 className="text-sm font-medium mb-2 flex items-center gap-1">
                              <CheckCircle className="h-4 w-4 text-green-500" /> Resolution
                            </h4>
                            <p className="text-sm bg-green-500/10 text-green-500 p-3 rounded-md">
                              {dispute.resolution}
                            </p>
                          </div>
                        )}
                        
                        {dispute.status === "Under Review" && (
                          <div>
                            <h4 className="text-sm font-medium mb-2 flex items-center gap-1">
                              <Vote className="h-4 w-4 text-orange-500" /> DAO Voting
                            </h4>
                            <div className="bg-muted p-3 rounded-md">
                              <div className="flex justify-between mb-2">
                                <span className="text-sm flex items-center gap-1">
                                  <CheckCircle className="h-3 w-3 text-green-500" /> In Favor: {dispute.votes.infavor}
                                </span>
                                <span className="text-sm flex items-center gap-1">
                                  <XCircle className="h-3 w-3 text-red-500" /> Against: {dispute.votes.against}
                                </span>
                              </div>
                              <div className="w-full bg-background rounded-full h-2.5">
                                <div 
                                  className="bg-green-500 h-2.5 rounded-full" 
                                  style={{ width: `${(dispute.votes.infavor / (dispute.votes.infavor + dispute.votes.against)) * 100}%` }}
                                ></div>
                              </div>
                              <div className="mt-2 text-xs text-muted-foreground">
                                <p>Voting ends in 3 days. Requires 60% majority to pass.</p>
                              </div>
                            </div>
                          </div>
                        )}
                      </div>
                    </CardContent>
                    <CardFooter className="flex justify-end gap-2">
                      {dispute.status !== "Resolved" && (
                        <>
                          <Button variant="outline" size="sm">
                            Add Evidence
                          </Button>
                          {dispute.status === "Under Review" && (
                            <>
                              <Button variant="default" size="sm" className="bg-green-500 hover:bg-green-600">
                                Vote In Favor
                              </Button>
                              <Button variant="outline" size="sm" className="border-red-500 text-red-500 hover:bg-red-500/10">
                                Vote Against
                              </Button>
                            </>
                          )}
                        </>
                      )}
                      <Button variant="ghost" size="sm">
                        View Full Details
                      </Button>
                    </CardFooter>
                  </Card>
                ))}
              </div>
            </>
          )}
        </TabsContent>
        
        <TabsContent value="verify">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            <div>
              <TransactionVerifier />
            </div>
            
            <div className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>How Transaction Verification Works</CardTitle>
                  <CardDescription>
                    Understanding the intent verification process
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="space-y-4">
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">1</div>
                      <div>
                        <h3 className="text-sm font-medium">Intent Registration</h3>
                        <p className="text-sm text-muted-foreground">
                          Before any transaction is executed, your intent (what you want to do) is recorded on-chain with a unique ID.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">2</div>
                      <div>
                        <h3 className="text-sm font-medium">Transaction Execution</h3>
                        <p className="text-sm text-muted-foreground">
                          When the transaction is executed, it references the intent ID, creating a verifiable link.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">3</div>
                      <div>
                        <h3 className="text-sm font-medium">Verification</h3>
                        <p className="text-sm text-muted-foreground">
                          The system compares the executed transaction against the original intent to detect any discrepancies.
                        </p>
                      </div>
                    </div>
                    
                    <div className="flex items-start gap-3">
                      <div className="bg-orange-500/20 text-orange-500 rounded-full h-6 w-6 flex items-center justify-center mt-0.5">4</div>
                      <div>
                        <h3 className="text-sm font-medium">Dispute Resolution</h3>
                        <p className="text-sm text-muted-foreground">
                          If a mismatch is found, you can submit a dispute to the DAO for review and potential rollback.
                        </p>
                      </div>
                    </div>
                  </div>
                  
                  <Alert className="bg-green-500/10 border-green-500/20 text-green-500">
                    <p className="text-sm">
                      This process protects you from malicious frontends, phishing attacks, and accidental transactions.
                    </p>
                  </Alert>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader>
                  <CardTitle>Recent Verification Results</CardTitle>
                  <CardDescription>
                    Latest transaction verifications in the protocol
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex justify-between items-center p-3 bg-green-500/10 rounded-md">
                      <div>
                        <p className="text-sm font-medium">ETH Transfer</p>
                        <p className="text-xs text-muted-foreground">0x1234...5678</p>
                      </div>
                      <span className="text-green-500 flex items-center gap-1 text-xs">
                        <CheckCircle className="h-3 w-3" /> Matched
                      </span>
                    </div>
                    
                    <div className="flex justify-between items-center p-3 bg-red-500/10 rounded-md">
                      <div>
                        <p className="text-sm font-medium">NFT Purchase</p>
                        <p className="text-xs text-muted-foreground">0xabcd...efgh</p>
                      </div>
                      <span className="text-red-500 flex items-center gap-1 text-xs">
                        <XCircle className="h-3 w-3" /> Mismatch
                      </span>
                    </div>
                    
                    <div className="flex justify-between items-center p-3 bg-green-500/10 rounded-md">
                      <div>
                        <p className="text-sm font-medium">Token Swap</p>
                        <p className="text-xs text-muted-foreground">0xfedc...ba98</p>
                      </div>
                      <span className="text-green-500 flex items-center gap-1 text-xs">
                        <CheckCircle className="h-3 w-3" /> Matched
                      </span>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
          </div>
        </TabsContent>
        
        <TabsContent value="governance">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="md:col-span-2">
              <Card>
                <CardHeader>
                  <CardTitle>DAO Governance Overview</CardTitle>
                  <CardDescription>
                    How the Dispute Resolution DAO works
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-6">
                  <Alert className="bg-blue-500/10 border-blue-500/20">
                    <div className="flex items-start gap-2">
                      <Info className="h-4 w-4 text-blue-500 mt-0.5" />
                      <div>
                        <p className="text-sm font-medium text-blue-500">About the Dispute Resolution DAO</p>
                        <p className="text-xs text-muted-foreground">
                          The Dispute Resolution DAO is a decentralized governance system that reviews and resolves disputes
                          related to transaction intent mismatches and contested wallet recoveries. It provides a human-layer
                          safeguard against abuse of the Phoenix Protocol.
                        </p>
                      </div>
                    </div>
                  </Alert>
                  
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div className="space-y-4">
                      <h3 className="text-lg font-medium">DAO Structure</h3>
                      <div className="space-y-2">
                        <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                          <span className="text-sm font-medium">Total Members</span>
                          <span className="text-sm">21</span>
                        </div>
                        <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                          <span className="text-sm font-medium">Voting Threshold</span>
                          <span className="text-sm">60%</span>
                        </div>
                        <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                          <span className="text-sm font-medium">Voting Period</span>
                          <span className="text-sm">7 days</span>
                        </div>
                        <div className="flex justify-between items-center p-3 bg-muted rounded-md">
                          <span className="text-sm font-medium">Execution Delay</span>
                          <span className="text-sm">48 hours</span>
                        </div>
                      </div>
                    </div>
                    
                    <div className="space-y-4">
                      <h3 className="text-lg font-medium">Governance Process</h3>
                      <ol className="space-y-2">
                        <li className="flex items-start gap-2 p-3 bg-muted rounded-md">
                          <span className="bg-orange-500/20 text-orange-500 rounded-full h-5 w-5 flex items-center justify-center flex-shrink-0 mt-0.5">1</span>
                          <div>
                            <p className="text-sm font-medium">Dispute Submission</p>
                            <p className="text-xs text-muted-foreground">User submits dispute with evidence</p>
                          </div>
                        </li>
                        <li className="flex items-start gap-2 p-3 bg-muted rounded-md">
                          <span className="bg-orange-500/20 text-orange-500 rounded-full h-5 w-5 flex items-center justify-center flex-shrink-0 mt-0.5">2</span>
                          <div>
                            <p className="text-sm font-medium">Evidence Collection</p>
                            <p className="text-xs text-muted-foreground">Both parties can submit evidence</p>
                          </div>
                        </li>
                        <li className="flex items-start gap-2 p-3 bg-muted rounded-md">
                          <span className="bg-orange-500/20 text-orange-500 rounded-full h-5 w-5 flex items-center justify-center flex-shrink-0 mt-0.5">3</span>
                          <div>
                            <p className="text-sm font-medium">DAO Voting</p>
                            <p className="text-xs text-muted-foreground">Members vote based on evidence</p>
                          </div>
                        </li>
                        <li className="flex items-start gap-2 p-3 bg-muted rounded-md">
                          <span className="bg-orange-500/20 text-orange-500 rounded-full h-5 w-5 flex items-center justify-center flex-shrink-0 mt-0.5">4</span>
                          <div>
                            <p className="text-sm font-medium">Resolution Execution</p>
                            <p className="text-xs text-muted-foreground">Approved actions are executed on-chain</p>
                          </div>
                        </li>
                      </ol>
                    </div>
                  </div>
                  
                  <div className="pt-4 border-t">
                    <h3 className="text-lg font-medium mb-4">DAO Powers</h3>
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                      <div className="p-4 bg-muted rounded-md">
                        <h4 className="text-sm font-medium mb-2">Transaction Rollbacks</h4>
                        <p className="text-xs text-muted-foreground">
                          Can reverse transactions that don't match user intent
                        </p>
                      </div>
                      <div className="p-4 bg-muted rounded-md">
                        <h4 className="text-sm font-medium mb-2">Recovery Approvals</h4>
                        <p className="text-xs text-muted-foreground">
                          Can approve or reject contested wallet recoveries
                        </p>
                      </div>
                      <div className="p-4 bg-muted rounded-md">
                        <h4 className="text-sm font-medium mb-2">Guardian Management</h4>
                        <p className="text-xs text-muted-foreground">
                          Can slash guardians for collusion or misconduct
                        </p>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
            
            <div className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Active Proposals</CardTitle>
                  <CardDescription>
                    Governance proposals currently being voted on
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-3">
                  <div className="p-3 bg-muted rounded-md">
                    <p className="text-sm font-medium">DRIP-23: Update Voting Threshold</p>
                    <div className="flex justify-between text-xs text-muted-foreground mt-1">
                      <span>Ends in 3 days</span>
                      <span>72% in favor</span>
                    </div>
                    <div className="w-full bg-background rounded-full h-1.5 mt-2">
                      <div 
                        className="bg-green-500 h-1.5 rounded-full" 
                        style={{ width: '72%' }}
                      ></div>
                    </div>
                  </div>
                  
                  <div className="p-3 bg-muted rounded-md">
                    <p className="text-sm font-medium">DRIP-24: Add New DAO Members</p>
                    <div className="flex justify-between text-xs text-muted-foreground mt-1">
                      <span>Ends in 5 days</span>
                      <span>45% in favor</span>
                    </div>
                    <div className="w-full bg-background rounded-full h-1.5 mt-2">
                      <div 
                        className="bg-yellow-500 h-1.5 rounded-full" 
                        style={{ width: '45%' }}
                      ></div>
                    </div>
                  </div>
                  
                  <div className="p-3 bg-muted rounded-md">
                    <p className="text-sm font-medium">DRIP-25: Increase Cooldown Period</p>
                    <div className="flex justify-between text-xs text-muted-foreground mt-1">
                      <span>Ends in 2 days</span>
                      <span>89% in favor</span>
                    </div>
                    <div className="w-full bg-background rounded-full h-1.5 mt-2">
                      <div 
                        className="bg-green-500 h-1.5 rounded-full" 
                        style={{ width: '89%' }}
                      ></div>
                    </div>
                  </div>
                </CardContent>
                <CardFooter>
                  <Button variant="outline" className="w-full">
                    View All Proposals
                  </Button>
                </CardFooter>
              </Card>
              
              <Card>
                <CardHeader>
                  <CardTitle>DAO Membership</CardTitle>
                  <CardDescription>
                    How to become a DAO member
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-sm text-muted-foreground">
                    The Dispute Resolution DAO is composed of trusted community members who help maintain the security and fairness of the Phoenix Protocol.
                  </p>
                  
                  <div className="space-y-2">
                    <h4 className="text-sm font-medium">Requirements:</h4>
                    <ul className="list-disc pl-5 text-sm space-y-1 text-muted-foreground">
                      <li>Hold at least 10,000 PHOENIX tokens</li>
                      <li>Pass KYC verification</li>
                      <li>Demonstrate expertise in blockchain security</li>
                      <li>Commit to active participation</li>
                    </ul>
                  </div>
                  
                  <Button variant="default" className="w-full">
                    Apply for Membership
                  </Button>
                </CardContent>
              </Card>
            </div>
          </div>
        </TabsContent>
      </Tabs>
    </div>
  );
}