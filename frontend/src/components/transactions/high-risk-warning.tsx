"use client";

import React, { useState } from "react";
import { AlertTriangle, Shield, Info } from "lucide-react";
import { Button } from "@/components/ui/button";

interface RiskFactor {
  id: string;
  title: string;
  description: string;
  severity: "high" | "medium" | "low";
}

interface HighRiskWarningProps {
  transactionType: string;
  riskFactors: RiskFactor[];
  onProceed: () => void;
  onCancel: () => void;
}

export function HighRiskWarning({
  transactionType,
  riskFactors,
  onProceed,
  onCancel,
}: HighRiskWarningProps) {
  const [acknowledged, setAcknowledged] = useState(false);
  
  const highRiskCount = riskFactors.filter(r => r.severity === "high").length;
  const mediumRiskCount = riskFactors.filter(r => r.severity === "medium").length;
  
  return (
    <div className="rounded-lg border border-red-500 p-6">
      <div className="mb-4 flex items-center">
        <div className="mr-4 flex h-12 w-12 items-center justify-center rounded-full bg-red-500/10">
          <AlertTriangle className="h-6 w-6 text-red-500" />
        </div>
        <div>
          <h3 className="text-lg font-bold text-red-500">High Risk Transaction</h3>
          <p className="text-sm text-muted-foreground">
            This {transactionType} transaction contains potential risks
          </p>
        </div>
      </div>
      
      <div className="mb-6 space-y-4">
        {riskFactors.map((factor) => (
          <div 
            key={factor.id} 
            className={`rounded-lg p-4 ${
              factor.severity === "high" 
                ? "bg-red-500/10" 
                : factor.severity === "medium" 
                ? "bg-orange-500/10" 
                : "bg-yellow-500/10"
            }`}
          >
            <div className="flex items-start">
              <div className={`mr-3 mt-0.5 h-5 w-5 rounded-full ${
                factor.severity === "high" 
                  ? "bg-red-500" 
                  : factor.severity === "medium" 
                  ? "bg-orange-500" 
                  : "bg-yellow-500"
              } flex items-center justify-center`}>
                <span className="text-xs font-bold text-white">
                  {factor.severity === "high" ? "H" : factor.severity === "medium" ? "M" : "L"}
                </span>
              </div>
              <div>
                <h4 className={`font-medium ${
                  factor.severity === "high" 
                    ? "text-red-500" 
                    : factor.severity === "medium" 
                    ? "text-orange-500" 
                    : "text-yellow-500"
                }`}>
                  {factor.title}
                </h4>
                <p className="mt-1 text-sm text-muted-foreground">
                  {factor.description}
                </p>
              </div>
            </div>
          </div>
        ))}
      </div>
      
      <div className="mb-6 rounded-lg bg-muted/30 p-4">
        <div className="flex items-start">
          <Shield className="mr-3 h-5 w-5 text-green-500" />
          <div>
            <h4 className="font-medium text-green-500">Phoenix Protocol Protection</h4>
            <p className="mt-1 text-sm text-muted-foreground">
              This transaction is protected by Phoenix Protocol's recovery features. If something goes wrong,
              you may be able to recover your assets through our dispute resolution process.
            </p>
          </div>
        </div>
      </div>
      
      <div className="mb-6">
        <label className="flex cursor-pointer items-start">
          <input
            type="checkbox"
            checked={acknowledged}
            onChange={() => setAcknowledged(!acknowledged)}
            className="mt-1 h-4 w-4 rounded border-muted-foreground/30 text-orange-500 focus:ring-orange-500"
          />
          <span className="ml-2 text-sm">
            I understand the risks associated with this transaction and wish to proceed anyway.
          </span>
        </label>
      </div>
      
      <div className="flex justify-end space-x-3">
        <Button variant="outline" onClick={onCancel}>
          Cancel Transaction
        </Button>
        <Button
          variant="destructive"
          onClick={onProceed}
          disabled={!acknowledged}
        >
          Proceed Anyway
        </Button>
      </div>
    </div>
  );
}