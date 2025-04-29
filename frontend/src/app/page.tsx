import Image from "next/image";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";

export default function Home() {
  // Mock data for recent disputes and rollback cases
  const recentCases = [
    {
      id: "case-001",
      type: "Dispute",
      title: "Token Swap Slippage Exceeded",
      description: "User intended 1% max slippage but received 15% slippage on DEX swap",
      status: "Resolved",
      date: "2 days ago"
    },
    {
      id: "case-002",
      type: "Rollback",
      title: "NFT Purchase Price Mismatch",
      description: "User was charged 2.5 ETH instead of advertised 0.25 ETH for NFT mint",
      status: "Completed",
      date: "3 days ago"
    },
    {
      id: "case-003",
      type: "Recovery",
      title: "Wallet Access Restored",
      description: "User regained access through social recovery after private key loss",
      status: "Completed",
      date: "1 week ago"
    }
  ];

  return (
    <>
      {/* Hero Section */}
      <section className="py-20 px-4 bg-gradient-to-b from-background to-muted">
        <div className="container mx-auto max-w-6xl">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-12 items-center">
            <div className="space-y-6">
              <h1 className="text-4xl md:text-6xl font-bold leading-tight">
                <span className="bg-gradient-to-r from-orange-500 to-red-500 bg-clip-text text-transparent">
                  Phoenix Protocol
                </span>
                <br />
                <span>Smart Contract Recovery</span>
              </h1>
              <p className="text-xl text-muted-foreground">
                Interact with smart contracts in a human-readable, intent-confirmed, and rollback-safe manner, preventing common transaction mishaps.
              </p>
              <div className="flex flex-col sm:flex-row gap-4 pt-4">
                <Button variant="gradient" size="lg">
                  Launch App
                </Button>
                <Button variant="outline" size="lg">
                  Read Documentation
                </Button>
              </div>
            </div>
            <div className="relative h-[400px] rounded-lg overflow-hidden shadow-xl">
              <div className="absolute inset-0 bg-gradient-to-br from-orange-500/20 to-red-500/20 z-10"></div>
              <div className="absolute inset-0 flex items-center justify-center z-20">
                <div className="text-center p-6 bg-background/80 backdrop-blur-sm rounded-lg shadow-lg max-w-md">
                  <h3 className="text-2xl font-bold mb-2">Transaction Protection</h3>
                  <p className="mb-4">Secure your DeFi and NFT transactions with intent verification and rollback capability</p>
                  <div className="flex justify-center">
                    <span className="px-3 py-1 bg-green-500/10 text-green-500 rounded-full text-sm font-medium">
                      Active on Avalanche & Cardano
                    </span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-16 px-4">
        <div className="container mx-auto max-w-6xl">
          <h2 className="text-3xl font-bold text-center mb-12">Core Features</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <Card>
              <CardHeader>
                <CardTitle>Intent Registry</CardTitle>
              </CardHeader>
              <CardContent>
                <p>Store your transaction intent in an immutable registry before execution, ensuring your actions are verified against your original intentions.</p>
              </CardContent>
            </Card>
            <Card>
              <CardHeader>
                <CardTitle>Transaction Verification</CardTitle>
              </CardHeader>
              <CardContent>
                <p>Automatically compare executed transactions against your intended parameters to detect mismatches in token amounts, recipients, or slippage.</p>
              </CardContent>
            </Card>
            <Card>
              <CardHeader>
                <CardTitle>Recovery Controller</CardTitle>
              </CardHeader>
              <CardContent>
                <p>Manage transaction rollbacks, wallet recovery, and asset restoration through our secure and verified recovery process.</p>
              </CardContent>
            </Card>
          </div>
        </div>
      </section>

      {/* Recent Cases Section */}
      <section className="py-16 px-4 bg-muted">
        <div className="container mx-auto max-w-6xl">
          <h2 className="text-3xl font-bold mb-12">Recent Disputes & Rollbacks</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {recentCases.map((caseItem) => (
              <Card key={caseItem.id}>
                <CardHeader>
                  <div className="flex justify-between items-center mb-2">
                    <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                      caseItem.type === 'Dispute' ? 'bg-yellow-500/10 text-yellow-500' : 
                      caseItem.type === 'Rollback' ? 'bg-blue-500/10 text-blue-500' : 
                      'bg-green-500/10 text-green-500'
                    }`}>
                      {caseItem.type}
                    </span>
                    <span className="text-xs text-muted-foreground">{caseItem.date}</span>
                  </div>
                  <CardTitle className="text-xl">{caseItem.title}</CardTitle>
                  <CardDescription>{caseItem.description}</CardDescription>
                </CardHeader>
                <CardFooter>
                  <div className="flex justify-between items-center w-full">
                    <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                      caseItem.status === 'Resolved' || caseItem.status === 'Completed' 
                        ? 'bg-green-500/10 text-green-500' 
                        : 'bg-orange-500/10 text-orange-500'
                    }`}>
                      {caseItem.status}
                    </span>
                    <Button variant="ghost" size="sm" asChild>
                      <Link href={`/cases/${caseItem.id}`}>View Details</Link>
                    </Button>
                  </div>
                </CardFooter>
              </Card>
            ))}
          </div>
          <div className="mt-8 text-center">
            <Button variant="outline" asChild>
              <Link href="/cases">View All Cases</Link>
            </Button>
          </div>
        </div>
      </section>

      {/* Call to Action */}
      <section className="py-20 px-4">
        <div className="container mx-auto max-w-4xl text-center">
          <h2 className="text-3xl md:text-4xl font-bold mb-6">Ready to Secure Your Blockchain Transactions?</h2>
          <p className="text-xl text-muted-foreground mb-8 max-w-2xl mx-auto">
            Join Phoenix Protocol today and experience worry-free DeFi and NFT interactions with built-in recovery mechanisms.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button variant="gradient" size="lg">Get Started</Button>
            <Button variant="outline" size="lg">For Developers</Button>
          </div>
        </div>
      </section>
    </>
  );
}
