import React from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";

export default function DisputesPage() {
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
    }
  ];

  return (
    <div className="container mx-auto py-10 max-w-6xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-3xl font-bold">Dispute Dashboard</h1>
        <Button variant="gradient">Submit New Dispute</Button>
      </div>
      
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
        <h2 className="text-2xl font-bold">Open Cases</h2>
        
        {disputes.map((dispute) => (
          <Card key={dispute.id} className={dispute.status === "Resolved" ? "border-green-500/50" : ""}>
            <CardHeader>
              <div className="flex justify-between items-start">
                <div>
                  <CardTitle>{dispute.title}</CardTitle>
                  <CardDescription>{dispute.description}</CardDescription>
                </div>
                <div className="flex flex-col items-end">
                  <span className={`px-3 py-1 rounded-full text-xs font-medium ${
                    dispute.status === "Resolved" ? "bg-green-500/10 text-green-500" : 
                    dispute.status === "Under Review" ? "bg-yellow-500/10 text-yellow-500" : 
                    "bg-blue-500/10 text-blue-500"
                  }`}>
                    {dispute.status}
                  </span>
                  <span className="text-xs text-muted-foreground mt-1">{dispute.date}</span>
                </div>
              </div>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div>
                  <h4 className="text-sm font-medium mb-2">Evidence</h4>
                  <ul className="space-y-1 text-sm bg-muted p-3 rounded-md font-mono">
                    {dispute.evidence.map((item, index) => (
                      <li key={index}>{item}</li>
                    ))}
                  </ul>
                </div>
                
                {dispute.resolution && (
                  <div>
                    <h4 className="text-sm font-medium mb-2">Resolution</h4>
                    <p className="text-sm bg-green-500/10 text-green-500 p-3 rounded-md">
                      {dispute.resolution}
                    </p>
                  </div>
                )}
                
                {dispute.status === "Under Review" && (
                  <div>
                    <h4 className="text-sm font-medium mb-2">DAO Voting</h4>
                    <div className="bg-muted p-3 rounded-md">
                      <div className="flex justify-between mb-2">
                        <span className="text-sm">In Favor: {dispute.votes.infavor}</span>
                        <span className="text-sm">Against: {dispute.votes.against}</span>
                      </div>
                      <div className="w-full bg-background rounded-full h-2.5">
                        <div 
                          className="bg-green-500 h-2.5 rounded-full" 
                          style={{ width: `${(dispute.votes.infavor / (dispute.votes.infavor + dispute.votes.against)) * 100}%` }}
                        ></div>
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
                      <Button variant="default" size="sm">
                        Vote In Favor
                      </Button>
                      <Button variant="outline" size="sm">
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
    </div>
  );
}