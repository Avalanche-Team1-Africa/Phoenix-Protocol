"use client";

import React, { useState } from "react";
import { ArrowDown, AlertTriangle, CheckCircle, Loader2 } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";

interface TokenBalance {
  symbol: string;
  name: string;
  amount: string;
  value: string;
  icon?: string;
}

interface SimulationResult {
  success: boolean;
  balanceChanges: {
    before: TokenBalance[];
    after: TokenBalance[];
  };
  warnings: string[];
  gasUsed: string;
  executionTime: string;
  error?: string;
}

interface TransactionSimulationProps {
  transactionType: string;
  isSimulating: boolean;
  simulationResult: SimulationResult | null;
  onSimulate: () => void;
  onProceed: () => void;
  onCancel: () => void;
}

export function TransactionSimulation({
  transactionType,
  isSimulating,
  simulationResult,
  onSimulate,
  onProceed,
  onCancel,
}: TransactionSimulationProps) {
  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h3 className="text-lg font-semibold">Transaction Simulation</h3>
        
        {!isSimulating && !simulationResult && (
          <Button variant="outline" size="sm" onClick={onSimulate}>
            Simulate Transaction
          </Button>
        )}
      </div>
      
      {isSimulating && (
        <div className="flex flex-col items-center justify-center py-8">
          <Loader2 className="h-8 w-8 animate-spin text-orange-500" />
          <p className="mt-4 text-muted-foreground">
            Simulating {transactionType} transaction...
          </p>
        </div>
      )}
      
      {simulationResult && (
        <div className="space-y-6">
          {/* Simulation Status */}
          <div className={`rounded-lg p-4 ${
            simulationResult.success 
              ? "bg-green-500/10 text-green-500" 
              : "bg-red-500/10 text-red-500"
          }`}>
            <div className="flex items-center">
              {simulationResult.success ? (
                <CheckCircle className="mr-2 h-5 w-5" />
              ) : (
                <AlertTriangle className="mr-2 h-5 w-5" />
              )}
              <span className="font-medium">
                {simulationResult.success 
                  ? "Simulation successful" 
                  : "Simulation failed"}
              </span>
            </div>
            {!simulationResult.success && simulationResult.error && (
              <p className="mt-2 text-sm">{simulationResult.error}</p>
            )}
          </div>
          
          {/* Balance Changes */}
          {simulationResult.success && (
            <Card className="overflow-hidden">
              <div className="border-b p-4">
                <h4 className="font-medium">Expected Balance Changes</h4>
              </div>
              
              <div className="grid grid-cols-2 divide-x">
                <div className="p-4">
                  <h5 className="mb-3 text-sm text-muted-foreground">Before</h5>
                  <div className="space-y-3">
                    {simulationResult.balanceChanges.before.map((token, index) => (
                      <div key={`before-${index}`} className="flex items-center justify-between">
                        <div className="flex items-center">
                          {token.icon ? (
                            <img 
                              src={token.icon} 
                              alt={token.symbol} 
                              className="mr-2 h-5 w-5 rounded-full" 
                            />
                          ) : (
                            <div className="mr-2 h-5 w-5 rounded-full bg-muted" />
                          )}
                          <span>{token.symbol}</span>
                        </div>
                        <span className="font-medium">{token.amount}</span>
                      </div>
                    ))}
                  </div>
                </div>
                
                <div className="p-4">
                  <h5 className="mb-3 text-sm text-muted-foreground">After</h5>
                  <div className="space-y-3">
                    {simulationResult.balanceChanges.after.map((token, index) => {
                      const beforeToken = simulationResult.balanceChanges.before.find(
                        (t) => t.symbol === token.symbol
                      );
                      const isIncreased = beforeToken && 
                        parseFloat(token.amount) > parseFloat(beforeToken.amount);
                      const isDecreased = beforeToken && 
                        parseFloat(token.amount) < parseFloat(beforeToken.amount);
                      
                      return (
                        <div key={`after-${index}`} className="flex items-center justify-between">
                          <div className="flex items-center">
                            {token.icon ? (
                              <img 
                                src={token.icon} 
                                alt={token.symbol} 
                                className="mr-2 h-5 w-5 rounded-full" 
                              />
                            ) : (
                              <div className="mr-2 h-5 w-5 rounded-full bg-muted" />
                            )}
                            <span>{token.symbol}</span>
                          </div>
                          <span className={`font-medium ${
                            isIncreased 
                              ? "text-green-500" 
                              : isDecreased 
                              ? "text-red-500" 
                              : ""
                          }`}>
                            {token.amount}
                          </span>
                        </div>
                      );
                    })}
                  </div>
                </div>
              </div>
              
              <div className="border-t bg-muted/30 p-4">
                <div className="flex justify-between">
                  <span className="text-sm text-muted-foreground">Estimated Gas Used</span>
                  <span>{simulationResult.gasUsed}</span>
                </div>
                <div className="mt-2 flex justify-between">
                  <span className="text-sm text-muted-foreground">Execution Time</span>
                  <span>{simulationResult.executionTime}</span>
                </div>
              </div>
            </Card>
          )}
          
          {/* Warnings */}
          {simulationResult.warnings.length > 0 && (
            <div className="rounded-lg border-l-4 border-l-orange-500 bg-orange-500/10 p-4">
              <h4 className="mb-2 font-medium text-orange-500">Warnings</h4>
              <ul className="ml-5 list-disc space-y-1">
                {simulationResult.warnings.map((warning, index) => (
                  <li key={index} className="text-sm text-orange-500/90">
                    {warning}
                  </li>
                ))}
              </ul>
            </div>
          )}
          
          {/* Action Buttons */}
          <div className="flex justify-end space-x-3">
            <Button variant="outline" onClick={onCancel}>
              Cancel
            </Button>
            <Button 
              variant={simulationResult.success ? "gradient" : "default"}
              onClick={onProceed}
              disabled={!simulationResult.success}
            >
              {simulationResult.success ? "Proceed with Transaction" : "Modify Transaction"}
            </Button>
          </div>
        </div>
      )}
      
      {!isSimulating && !simulationResult && (
        <div className="rounded-lg border border-dashed border-muted-foreground/30 p-8 text-center">
          <div className="mx-auto mb-4 flex h-12 w-12 items-center justify-center rounded-full bg-muted">
            <ArrowDown className="h-6 w-6 text-muted-foreground" />
          </div>
          <h3 className="mb-2 text-lg font-medium">Simulate Before Executing</h3>
          <p className="mb-4 text-sm text-muted-foreground">
            Run a simulation to see the expected outcome of this transaction
            before sending it to the blockchain.
          </p>
          <Button onClick={onSimulate}>
            Simulate Transaction
          </Button>
        </div>
      )}
    </div>
  );
}