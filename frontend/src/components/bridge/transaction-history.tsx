import React, { useEffect, useState } from 'react';
import { BridgeService } from '@/services/bridge-service';
import { getChainName, getTxExplorerUrl } from '@/lib/utils/blockchain';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { ExternalLink } from 'lucide-react';

interface BridgeTransaction {
  sourceChainId: number;
  targetChainId: number;
  sourceAddress: string;
  targetAddress: string;
  amount: string;
  token: string;
  status: 'pending' | 'completed' | 'failed';
  txHash: string;
  timestamp: number;
}

const TransactionHistory: React.FC = () => {
  const [transactions, setTransactions] = useState<BridgeTransaction[]>([]);
  const bridgeService = BridgeService.getInstance();
  
  useEffect(() => {
    // Get transactions from the bridge service
    const txs = bridgeService.getTransactions();
    setTransactions(txs);
    
    // Set up an interval to refresh the transactions
    const interval = setInterval(() => {
      setTransactions(bridgeService.getTransactions());
    }, 5000);
    
    return () => clearInterval(interval);
  }, []);
  
  // Format date from timestamp
  const formatDate = (timestamp: number) => {
    return new Date(timestamp).toLocaleString();
  };
  
  // Truncate address for display
  const truncateAddress = (address: string) => {
    if (!address) return '';
    return `${address.slice(0, 6)}...${address.slice(-4)}`;
  };
  
  // Get status badge color
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'completed':
        return 'bg-green-100 text-green-800 hover:bg-green-200';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800 hover:bg-yellow-200';
      case 'failed':
        return 'bg-red-100 text-red-800 hover:bg-red-200';
      default:
        return 'bg-gray-100 text-gray-800 hover:bg-gray-200';
    }
  };
  
  return (
    <Card>
      <CardHeader>
        <CardTitle>Transaction History</CardTitle>
        <CardDescription>Recent bridge transactions</CardDescription>
      </CardHeader>
      <CardContent>
        {transactions.length === 0 ? (
          <div className="text-center py-8 text-gray-500">
            No transactions found. Bridge some assets to get started.
          </div>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Date</TableHead>
                <TableHead>From</TableHead>
                <TableHead>To</TableHead>
                <TableHead>Amount</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Transaction</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {transactions.map((tx) => (
                <TableRow key={tx.txHash}>
                  <TableCell className="font-medium">
                    {formatDate(tx.timestamp)}
                  </TableCell>
                  <TableCell>
                    {getChainName(tx.sourceChainId)}
                    <div className="text-xs text-gray-500">
                      {truncateAddress(tx.sourceAddress)}
                    </div>
                  </TableCell>
                  <TableCell>
                    {getChainName(tx.targetChainId)}
                    <div className="text-xs text-gray-500">
                      {truncateAddress(tx.targetAddress)}
                    </div>
                  </TableCell>
                  <TableCell>
                    {tx.amount} {tx.token}
                  </TableCell>
                  <TableCell>
                    <Badge className={getStatusColor(tx.status)}>
                      {tx.status.charAt(0).toUpperCase() + tx.status.slice(1)}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <a
                      href={getTxExplorerUrl(tx.sourceChainId, tx.txHash)}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="text-blue-600 hover:text-blue-800 flex items-center"
                    >
                      <span className="text-xs">View</span>
                      <ExternalLink className="h-3 w-3 ml-1" />
                    </a>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </CardContent>
    </Card>
  );
};

export default TransactionHistory;