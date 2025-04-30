"use client";

import React, { useState, useEffect } from "react";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { useWallet } from "@/context/wallet-context";
import { useTransactionIntent } from "@/hooks/use-transaction-intent";
import { formatTokenAmount } from "@/lib/utils/blockchain";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Button } from "@/components/ui/button";
import { Select } from "@/components/ui/select";
import { WalletConnectModal } from "@/components/forms/wallet-connect-modal";
import { 
  BarChart, 
  Bar, 
  LineChart, 
  Line, 
  PieChart, 
  Pie, 
  Cell, 
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  Legend, 
  ResponsiveContainer 
} from 'recharts';
import { Download, Calendar, Filter } from "lucide-react";

// Mock data for charts
const generateMockTransactionData = (days = 30): TransactionDataItem[] => {
  const data: TransactionDataItem[] = [];
  const types = ['swap', 'transfer', 'stake', 'mint'];
  const tokens = ['USDC', 'AVAX', 'ETH', 'BTC', 'ADA'];
  const now = new Date();
  
  for (let i = 0; i < days; i++) {
    const date = new Date(now);
    date.setDate(date.getDate() - i);
    
    const swapCount = Math.floor(Math.random() * 5);
    const transferCount = Math.floor(Math.random() * 3);
    const stakeCount = Math.floor(Math.random() * 2);
    const mintCount = Math.floor(Math.random() * 1);
    
    data.push({
      date: date.toISOString().split('T')[0],
      swap: swapCount,
      transfer: transferCount,
      stake: stakeCount,
      mint: mintCount,
      total: swapCount + transferCount + stakeCount + mintCount,
      volume: Math.floor(Math.random() * 1000 + 100),
      gasSpent: Math.random() * 0.5,
    });
  }
  
  return data.reverse();
};

const generateTokenDistribution = (): DistributionItem[] => {
  return [
    { name: 'USDC', value: 45 },
    { name: 'AVAX', value: 25 },
    { name: 'ETH', value: 15 },
    { name: 'BTC', value: 10 },
    { name: 'Other', value: 5 },
  ];
};

const generateChainDistribution = (): DistributionItem[] => {
  return [
    { name: 'Avalanche', value: 60 },
    { name: 'Ethereum', value: 25 },
    { name: 'Cardano', value: 15 },
  ];
};

const COLORS = ['#0088FE', '#00C49F', '#FFBB28', '#FF8042', '#8884d8'];

// Define types for our data structures
type TransactionDataItem = {
  date: string;
  swap: number;
  transfer: number;
  stake: number;
  mint: number;
  total: number;
  volume: number;
  gasSpent: number;
};

type DistributionItem = {
  name: string;
  value: number;
};

export default function AnalyticsPage() {
  const { wallet } = useWallet();
  const { getAllIntents } = useTransactionIntent();
  const [isWalletModalOpen, setIsWalletModalOpen] = useState(false);
  const [timeRange, setTimeRange] = useState('30d');
  const [transactionData, setTransactionData] = useState<TransactionDataItem[]>([]);
  const [tokenDistribution, setTokenDistribution] = useState<DistributionItem[]>([]);
  const [chainDistribution, setChainDistribution] = useState<DistributionItem[]>([]);
  const [activeTab, setActiveTab] = useState('overview');
  
  // Load data when component mounts
  useEffect(() => {
    // In a real app, this would fetch data from an API
    setTransactionData(generateMockTransactionData(timeRange === '7d' ? 7 : timeRange === '30d' ? 30 : 90));
    setTokenDistribution(generateTokenDistribution());
    setChainDistribution(generateChainDistribution());
  }, [timeRange]);
  
  // Define type for stats
  type Stats = {
    total: number;
    volume: number;
    gas: string;
  };

  // Calculate summary statistics
  const calculateStats = (): Stats => {
    if (!transactionData.length) return { total: 0, volume: 0, gas: '0' };
    
    const total = transactionData.reduce((sum, day) => sum + day.total, 0);
    const volume = transactionData.reduce((sum, day) => sum + day.volume, 0);
    const gas = transactionData.reduce((sum, day) => sum + day.gasSpent, 0).toFixed(4);
    
    return { total, volume, gas };
  };
  
  const stats = calculateStats();
  
  // Format date for display
  const formatDate = (dateStr: string): string => {
    const date = new Date(dateStr);
    return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
  };
  
  // Export data as CSV
  const exportData = (): void => {
    const headers = 'Date,Swaps,Transfers,Stakes,Mints,Total,Volume,Gas\n';
    const csvContent = headers + transactionData.map(day => 
      `${day.date},${day.swap},${day.transfer},${day.stake},${day.mint},${day.total},${day.volume},${day.gasSpent}`
    ).join('\n');
    
    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.setAttribute('href', url);
    link.setAttribute('download', `phoenix-analytics-${timeRange}.csv`);
    link.style.visibility = 'hidden';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  };
  
  return (
    <div className="container mx-auto py-10 max-w-7xl">
      <div className="flex flex-col md:flex-row justify-between items-start md:items-center mb-6">
        <div>
          <h1 className="text-3xl font-bold">Analytics Dashboard</h1>
          <p className="text-muted-foreground">Track and analyze your blockchain transactions</p>
        </div>
        
        <div className="flex items-center space-x-2 mt-4 md:mt-0">
          <div className="flex items-center space-x-2">
            <Calendar className="h-4 w-4 text-muted-foreground" />
            <Select 
              value={timeRange}
              onChange={(e) => setTimeRange(e.target.value)}
              className="w-24"
            >
              <option value="7d">7 days</option>
              <option value="30d">30 days</option>
              <option value="90d">90 days</option>
            </Select>
          </div>
          
          <Button variant="outline" size="sm" onClick={exportData}>
            <Download className="h-4 w-4 mr-2" />
            Export
          </Button>
        </div>
      </div>
      
      {!wallet.connected ? (
        <Card className="mb-6">
          <CardContent className="py-8">
            <div className="text-center space-y-4">
              <p className="text-muted-foreground">Connect your wallet to view your personalized analytics</p>
              <Button variant="gradient" onClick={() => setIsWalletModalOpen(true)}>
                Connect Wallet
              </Button>
            </div>
          </CardContent>
        </Card>
      ) : (
        <>
          {/* Summary Cards */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-6">
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium text-muted-foreground">
                  Total Transactions
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">{stats.total}</div>
                <p className="text-xs text-muted-foreground mt-1">
                  Last {timeRange === '7d' ? '7' : timeRange === '30d' ? '30' : '90'} days
                </p>
              </CardContent>
            </Card>
            
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium text-muted-foreground">
                  Transaction Volume
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">${stats.volume.toLocaleString()}</div>
                <p className="text-xs text-muted-foreground mt-1">
                  In USD equivalent
                </p>
              </CardContent>
            </Card>
            
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm font-medium text-muted-foreground">
                  Gas Spent
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">{stats.gas} ETH</div>
                <p className="text-xs text-muted-foreground mt-1">
                  Across all networks
                </p>
              </CardContent>
            </Card>
          </div>
          
          <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
            <TabsList className="grid grid-cols-3 md:w-[400px]">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="transactions">Transactions</TabsTrigger>
              <TabsTrigger value="assets">Assets</TabsTrigger>
            </TabsList>
            
            <TabsContent value="overview" className="space-y-6">
              {/* Transaction Activity Chart */}
              <Card>
                <CardHeader>
                  <CardTitle>Transaction Activity</CardTitle>
                  <CardDescription>
                    Number of transactions by type over time
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="h-[300px]">
                    <ResponsiveContainer width="100%" height="100%">
                      <BarChart
                        data={transactionData}
                        margin={{ top: 20, right: 30, left: 20, bottom: 5 }}
                      >
                        <CartesianGrid strokeDasharray="3 3" />
                        <XAxis 
                          dataKey="date" 
                          tickFormatter={formatDate}
                          tick={{ fontSize: 12 }}
                        />
                        <YAxis />
                        <Tooltip />
                        <Legend />
                        <Bar dataKey="swap" stackId="a" fill="#8884d8" name="Swaps" />
                        <Bar dataKey="transfer" stackId="a" fill="#82ca9d" name="Transfers" />
                        <Bar dataKey="stake" stackId="a" fill="#ffc658" name="Stakes" />
                        <Bar dataKey="mint" stackId="a" fill="#ff8042" name="Mints" />
                      </BarChart>
                    </ResponsiveContainer>
                  </div>
                </CardContent>
              </Card>
              
              {/* Volume and Distribution Charts */}
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <Card>
                  <CardHeader>
                    <CardTitle>Transaction Volume</CardTitle>
                    <CardDescription>
                      Daily transaction volume in USD
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="h-[250px]">
                      <ResponsiveContainer width="100%" height="100%">
                        <LineChart
                          data={transactionData}
                          margin={{ top: 5, right: 30, left: 20, bottom: 5 }}
                        >
                          <CartesianGrid strokeDasharray="3 3" />
                          <XAxis 
                            dataKey="date" 
                            tickFormatter={formatDate}
                            tick={{ fontSize: 12 }}
                          />
                          <YAxis />
                          <Tooltip />
                          <Legend />
                          <Line 
                            type="monotone" 
                            dataKey="volume" 
                            stroke="#8884d8" 
                            activeDot={{ r: 8 }}
                            name="Volume (USD)"
                          />
                        </LineChart>
                      </ResponsiveContainer>
                    </div>
                  </CardContent>
                </Card>
                
                <Card>
                  <CardHeader>
                    <CardTitle>Asset Distribution</CardTitle>
                    <CardDescription>
                      Breakdown of assets by value
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="h-[250px]">
                      <ResponsiveContainer width="100%" height="100%">
                        <PieChart>
                          <Pie
                            data={tokenDistribution}
                            cx="50%"
                            cy="50%"
                            labelLine={false}
                            label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
                            outerRadius={80}
                            fill="#8884d8"
                            dataKey="value"
                          >
                            {tokenDistribution.map((entry, index) => (
                              <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                            ))}
                          </Pie>
                          <Tooltip />
                          <Legend />
                        </PieChart>
                      </ResponsiveContainer>
                    </div>
                  </CardContent>
                </Card>
              </div>
              
              {/* Network Distribution */}
              <Card>
                <CardHeader>
                  <CardTitle>Network Distribution</CardTitle>
                  <CardDescription>
                    Transactions by blockchain network
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="h-[250px]">
                    <ResponsiveContainer width="100%" height="100%">
                      <PieChart>
                        <Pie
                          data={chainDistribution}
                          cx="50%"
                          cy="50%"
                          labelLine={false}
                          label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
                          outerRadius={80}
                          fill="#8884d8"
                          dataKey="value"
                        >
                          {chainDistribution.map((entry, index) => (
                            <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                          ))}
                        </Pie>
                        <Tooltip />
                        <Legend />
                      </PieChart>
                    </ResponsiveContainer>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
            
            <TabsContent value="transactions" className="space-y-6">
              {/* Transaction History Table */}
              <Card>
                <CardHeader>
                  <CardTitle>Transaction History</CardTitle>
                  <CardDescription>
                    Detailed view of your recent transactions
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="rounded-md border">
                    <table className="min-w-full divide-y divide-border">
                      <thead>
                        <tr className="bg-muted/50">
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Date</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Type</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Asset</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Amount</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Status</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Network</th>
                        </tr>
                      </thead>
                      <tbody className="bg-background divide-y divide-border">
                        {/* Mock transaction data */}
                        {[...Array(10)].map((_, i) => {
                          const types = ['Swap', 'Transfer', 'Stake', 'Mint'];
                          const assets = ['USDC', 'AVAX', 'ETH', 'BTC', 'ADA'];
                          const networks = ['Avalanche', 'Ethereum', 'Cardano'];
                          const statuses = ['Completed', 'Pending', 'Failed'];
                          
                          const type = types[Math.floor(Math.random() * types.length)];
                          const asset = assets[Math.floor(Math.random() * assets.length)];
                          const network = networks[Math.floor(Math.random() * networks.length)];
                          const status = statuses[Math.floor(Math.random() * statuses.length)];
                          const amount = (Math.random() * 100).toFixed(2);
                          
                          const date = new Date();
                          date.setDate(date.getDate() - i);
                          
                          return (
                            <tr key={i}>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                {date.toLocaleDateString()}
                              </td>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                {type}
                              </td>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                {asset}
                              </td>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                {amount}
                              </td>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                                  status === 'Completed' ? 'bg-green-500/10 text-green-500' : 
                                  status === 'Pending' ? 'bg-yellow-500/10 text-yellow-500' : 
                                  'bg-red-500/10 text-red-500'
                                }`}>
                                  {status}
                                </span>
                              </td>
                              <td className="px-4 py-4 whitespace-nowrap text-sm">
                                {network}
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  </div>
                </CardContent>
              </Card>
              
              {/* Gas Usage Chart */}
              <Card>
                <CardHeader>
                  <CardTitle>Gas Usage</CardTitle>
                  <CardDescription>
                    Daily gas consumption in ETH
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="h-[250px]">
                    <ResponsiveContainer width="100%" height="100%">
                      <LineChart
                        data={transactionData}
                        margin={{ top: 5, right: 30, left: 20, bottom: 5 }}
                      >
                        <CartesianGrid strokeDasharray="3 3" />
                        <XAxis 
                          dataKey="date" 
                          tickFormatter={formatDate}
                          tick={{ fontSize: 12 }}
                        />
                        <YAxis />
                        <Tooltip />
                        <Legend />
                        <Line 
                          type="monotone" 
                          dataKey="gasSpent" 
                          stroke="#ff8042" 
                          activeDot={{ r: 8 }}
                          name="Gas (ETH)"
                        />
                      </LineChart>
                    </ResponsiveContainer>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
            
            <TabsContent value="assets" className="space-y-6">
              {/* Asset Portfolio */}
              <Card>
                <CardHeader>
                  <CardTitle>Asset Portfolio</CardTitle>
                  <CardDescription>
                    Current value of your assets
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="rounded-md border">
                    <table className="min-w-full divide-y divide-border">
                      <thead>
                        <tr className="bg-muted/50">
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Asset</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Balance</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Value (USD)</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">24h Change</th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-muted-foreground uppercase tracking-wider">Network</th>
                        </tr>
                      </thead>
                      <tbody className="bg-background divide-y divide-border">
                        {/* Mock asset data */}
                        {[
                          { asset: 'USDC', balance: '1,250.00', value: '1,250.00', change: '+0.01%', network: 'Avalanche' },
                          { asset: 'AVAX', balance: '45.75', value: '1,372.50', change: '-2.34%', network: 'Avalanche' },
                          { asset: 'ETH', balance: '0.85', value: '2,550.00', change: '+1.25%', network: 'Ethereum' },
                          { asset: 'BTC', balance: '0.025', value: '750.00', change: '+0.75%', network: 'Wrapped (Avalanche)' },
                          { asset: 'ADA', balance: '500.00', value: '150.00', change: '-1.50%', network: 'Cardano' },
                        ].map((asset, i) => (
                          <tr key={i}>
                            <td className="px-4 py-4 whitespace-nowrap">
                              <div className="flex items-center">
                                <div className="h-8 w-8 rounded-full bg-muted flex items-center justify-center mr-3">
                                  {asset.asset.charAt(0)}
                                </div>
                                <div>
                                  <div className="font-medium">{asset.asset}</div>
                                </div>
                              </div>
                            </td>
                            <td className="px-4 py-4 whitespace-nowrap text-sm">
                              {asset.balance}
                            </td>
                            <td className="px-4 py-4 whitespace-nowrap text-sm">
                              ${asset.value}
                            </td>
                            <td className="px-4 py-4 whitespace-nowrap text-sm">
                              <span className={asset.change.startsWith('+') ? 'text-green-500' : 'text-red-500'}>
                                {asset.change}
                              </span>
                            </td>
                            <td className="px-4 py-4 whitespace-nowrap text-sm">
                              {asset.network}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </CardContent>
              </Card>
              
              {/* Asset Performance Chart */}
              <Card>
                <CardHeader>
                  <CardTitle>Asset Performance</CardTitle>
                  <CardDescription>
                    Value change over time
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="h-[300px]">
                    <ResponsiveContainer width="100%" height="100%">
                      <LineChart
                        margin={{ top: 5, right: 30, left: 20, bottom: 5 }}
                      >
                        <CartesianGrid strokeDasharray="3 3" />
                        <XAxis 
                          dataKey="date" 
                          tick={{ fontSize: 12 }}
                        />
                        <YAxis />
                        <Tooltip />
                        <Legend />
                        <Line 
                          type="monotone" 
                          dataKey="value" 
                          stroke="#8884d8" 
                          name="Portfolio Value"
                          data={[
                            { date: '2023-01-01', value: 4500 },
                            { date: '2023-01-15', value: 4800 },
                            { date: '2023-02-01', value: 5200 },
                            { date: '2023-02-15', value: 5000 },
                            { date: '2023-03-01', value: 5500 },
                            { date: '2023-03-15', value: 6000 },
                            { date: '2023-04-01', value: 5800 },
                          ]}
                        />
                      </LineChart>
                    </ResponsiveContainer>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </>
      )}
      
      <WalletConnectModal 
        isOpen={isWalletModalOpen} 
        onClose={() => setIsWalletModalOpen(false)} 
      />
    </div>
  );
}