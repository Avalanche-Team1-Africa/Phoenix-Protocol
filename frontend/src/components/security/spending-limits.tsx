"use client";

import React, { useState } from "react";
import { AlertTriangle, DollarSign, Clock, Shield, CheckCircle, Settings } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Slider } from "@/components/ui/slider";
import { Switch } from "@/components/ui/switch";
import { formatAddress } from "@/lib/utils/blockchain";

interface SpendingLimit {
  token: string;
  symbol: string;
  icon?: string;
  dailyLimit: string;
  dailyUsed: string;
  weeklyLimit: string;
  weeklyUsed: string;
  monthlyLimit: string;
  monthlyUsed: string;
}

interface TransactionApproval {
  from: string;
  to: string;
  token: string;
  symbol: string;
  amount: string;
  usdValue: string;
  exceedsLimit: boolean;
}

interface SpendingLimitsProps {
  limits: SpendingLimit[];
  onUpdateLimits: (limits: SpendingLimit[]) => Promise<void>;
  onClose: () => void;
}

export function SpendingLimits({
  limits,
  onUpdateLimits,
  onClose,
}: SpendingLimitsProps) {
  const [editedLimits, setEditedLimits] = useState<SpendingLimit[]>(limits);
  const [isUpdating, setIsUpdating] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleLimitChange = (
    index: number,
    period: "daily" | "weekly" | "monthly",
    value: string
  ) => {
    const newLimits = [...editedLimits];
    newLimits[index] = {
      ...newLimits[index],
      [`${period}Limit`]: value,
    };
    setEditedLimits(newLimits);
  };

  const handleSave = async () => {
    setIsUpdating(true);
    setError(null);

    try {
      await onUpdateLimits(editedLimits);
      onClose();
    } catch (err: any) {
      setError(err.message || "Failed to update spending limits");
    } finally {
      setIsUpdating(false);
    }
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center">
          <Settings className="mr-2 h-5 w-5 text-orange-500" />
          <h2 className="text-xl font-bold">Spending Limits</h2>
        </div>
      </div>

      <div className="rounded-lg border border-orange-500/30 bg-orange-500/5 p-4">
        <div className="flex items-start">
          <Shield className="mr-3 mt-0.5 h-5 w-5 text-orange-500" />
          <div>
            <h3 className="font-medium text-orange-500">Security Feature</h3>
            <p className="mt-1 text-sm text-orange-500/80">
              Set spending limits to protect your assets. Transactions exceeding these
              limits will require additional verification.
            </p>
          </div>
        </div>
      </div>

      {/* Spending Limits */}
      <div className="space-y-6">
        {editedLimits.map((limit, index) => (
          <div key={limit.token} className="rounded-lg border bg-muted/20 p-4">
            <div className="mb-4 flex items-center">
              {limit.icon ? (
                <img
                  src={limit.icon}
                  alt={limit.symbol}
                  className="mr-3 h-8 w-8 rounded-full"
                />
              ) : (
                <div className="mr-3 flex h-8 w-8 items-center justify-center rounded-full bg-muted">
                  <DollarSign className="h-4 w-4" />
                </div>
              )}
              <div>
                <h3 className="font-medium">{limit.symbol}</h3>
                <p className="text-xs text-muted-foreground">{limit.token}</p>
              </div>
            </div>

            <div className="space-y-4">
              {/* Daily Limit */}
              <div>
                <div className="mb-2 flex items-center justify-between">
                  <div className="flex items-center">
                    <Clock className="mr-2 h-4 w-4 text-muted-foreground" />
                    <span className="text-sm">Daily Limit</span>
                  </div>
                  <div className="flex items-center space-x-2">
                    <input
                      type="number"
                      value={parseFloat(limit.dailyLimit)}
                      onChange={(e) =>
                        handleLimitChange(index, "daily", e.target.value)
                      }
                      className="w-20 rounded-md border border-muted-foreground/30 bg-transparent px-2 py-1 text-right text-sm"
                    />
                    <span className="text-sm">{limit.symbol}</span>
                  </div>
                </div>
                <div className="flex items-center">
                  <div className="relative mr-2 h-2 flex-1 rounded-full bg-muted">
                    <div
                      className="absolute left-0 top-0 h-full rounded-full bg-orange-500"
                      style={{
                        width: `${
                          (parseFloat(limit.dailyUsed) /
                            parseFloat(limit.dailyLimit)) *
                          100
                        }%`,
                      }}
                    ></div>
                  </div>
                  <span className="text-xs text-muted-foreground">
                    {limit.dailyUsed}/{limit.dailyLimit} used
                  </span>
                </div>
              </div>

              {/* Weekly Limit */}
              <div>
                <div className="mb-2 flex items-center justify-between">
                  <div className="flex items-center">
                    <Clock className="mr-2 h-4 w-4 text-muted-foreground" />
                    <span className="text-sm">Weekly Limit</span>
                  </div>
                  <div className="flex items-center space-x-2">
                    <input
                      type="number"
                      value={parseFloat(limit.weeklyLimit)}
                      onChange={(e) =>
                        handleLimitChange(index, "weekly", e.target.value)
                      }
                      className="w-20 rounded-md border border-muted-foreground/30 bg-transparent px-2 py-1 text-right text-sm"
                    />
                    <span className="text-sm">{limit.symbol}</span>
                  </div>
                </div>
                <div className="flex items-center">
                  <div className="relative mr-2 h-2 flex-1 rounded-full bg-muted">
                    <div
                      className="absolute left-0 top-0 h-full rounded-full bg-orange-500"
                      style={{
                        width: `${
                          (parseFloat(limit.weeklyUsed) /
                            parseFloat(limit.weeklyLimit)) *
                          100
                        }%`,
                      }}
                    ></div>
                  </div>
                  <span className="text-xs text-muted-foreground">
                    {limit.weeklyUsed}/{limit.weeklyLimit} used
                  </span>
                </div>
              </div>

              {/* Monthly Limit */}
              <div>
                <div className="mb-2 flex items-center justify-between">
                  <div className="flex items-center">
                    <Clock className="mr-2 h-4 w-4 text-muted-foreground" />
                    <span className="text-sm">Monthly Limit</span>
                  </div>
                  <div className="flex items-center space-x-2">
                    <input
                      type="number"
                      value={parseFloat(limit.monthlyLimit)}
                      onChange={(e) =>
                        handleLimitChange(index, "monthly", e.target.value)
                      }
                      className="w-20 rounded-md border border-muted-foreground/30 bg-transparent px-2 py-1 text-right text-sm"
                    />
                    <span className="text-sm">{limit.symbol}</span>
                  </div>
                </div>
                <div className="flex items-center">
                  <div className="relative mr-2 h-2 flex-1 rounded-full bg-muted">
                    <div
                      className="absolute left-0 top-0 h-full rounded-full bg-orange-500"
                      style={{
                        width: `${
                          (parseFloat(limit.monthlyUsed) /
                            parseFloat(limit.monthlyLimit)) *
                          100
                        }%`,
                      }}
                    ></div>
                  </div>
                  <span className="text-xs text-muted-foreground">
                    {limit.monthlyUsed}/{limit.monthlyLimit} used
                  </span>
                </div>
              </div>
            </div>
          </div>
        ))}
      </div>

      {/* Error Message */}
      {error && (
        <div className="rounded-lg bg-red-500/10 p-3 text-sm text-red-500">
          <div className="flex items-start">
            <AlertTriangle className="mr-2 mt-0.5 h-4 w-4" />
            <span>{error}</span>
          </div>
        </div>
      )}

      {/* Action Buttons */}
      <div className="flex justify-end space-x-3">
        <Button variant="outline" onClick={onClose} disabled={isUpdating}>
          Cancel
        </Button>
        <Button
          variant="gradient"
          onClick={handleSave}
          disabled={isUpdating}
          className="min-w-[120px]"
        >
          {isUpdating ? (
            <div className="flex items-center">
              <div className="mr-2 h-4 w-4 animate-spin rounded-full border-2 border-current border-t-transparent"></div>
              Saving...
            </div>
          ) : (
            <div className="flex items-center">
              <CheckCircle className="mr-2 h-4 w-4" />
              Save Limits
            </div>
          )}
        </Button>
      </div>
    </div>
  );
}

// Transaction Approval Component
interface TransactionApprovalProps {
  transaction: TransactionApproval;
  onApprove: () => Promise<void>;
  onReject: () => void;
  onRequestIncrease: () => void;
}

export function TransactionApprovalModal({
  transaction,
  onApprove,
  onReject,
  onRequestIncrease,
}: TransactionApprovalProps) {
  const [isApproving, setIsApproving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleApprove = async () => {
    setIsApproving(true);
    setError(null);

    try {
      await onApprove();
    } catch (err: any) {
      setError(err.message || "Failed to approve transaction");
    } finally {
      setIsApproving(false);
    }
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-xl font-bold">Transaction Approval Required</h2>
      </div>

      {transaction.exceedsLimit && (
        <div className="rounded-lg border border-orange-500 bg-orange-500/10 p-4">
          <div className="flex items-start">
            <AlertTriangle className="mr-3 mt-0.5 h-5 w-5 text-orange-500" />
            <div>
              <h3 className="font-medium text-orange-500">Spending Limit Exceeded</h3>
              <p className="mt-1 text-sm text-orange-500/80">
                This transaction exceeds your daily spending limit for {transaction.symbol}.
                Additional verification is required.
              </p>
            </div>
          </div>
        </div>
      )}

      {/* Transaction Details */}
      <div className="rounded-lg border bg-muted/20 p-4">
        <h3 className="mb-4 font-medium">Transaction Details</h3>

        <div className="space-y-3">
          <div className="flex justify-between">
            <span className="text-sm text-muted-foreground">From</span>
            <span className="font-medium">{formatAddress(transaction.from, 8)}</span>
          </div>

          <div className="flex justify-between">
            <span className="text-sm text-muted-foreground">To</span>
            <span className="font-medium">{formatAddress(transaction.to, 8)}</span>
          </div>

          <div className="flex justify-between">
            <span className="text-sm text-muted-foreground">Amount</span>
            <span className="font-medium">
              {transaction.amount} {transaction.symbol}
            </span>
          </div>

          <div className="flex justify-between">
            <span className="text-sm text-muted-foreground">USD Value</span>
            <span className="font-medium">${transaction.usdValue}</span>
          </div>
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="rounded-lg bg-red-500/10 p-3 text-sm text-red-500">
          <div className="flex items-start">
            <AlertTriangle className="mr-2 mt-0.5 h-4 w-4" />
            <span>{error}</span>
          </div>
        </div>
      )}

      {/* Action Buttons */}
      <div className="flex justify-end space-x-3">
        <Button variant="outline" onClick={onReject} disabled={isApproving}>
          Reject
        </Button>
        {transaction.exceedsLimit && (
          <Button
            variant="outline"
            onClick={onRequestIncrease}
            disabled={isApproving}
            className="border-orange-500/30 text-orange-500"
          >
            Increase Limit
          </Button>
        )}
        <Button
          variant={transaction.exceedsLimit ? "default" : "gradient"}
          onClick={handleApprove}
          disabled={isApproving}
          className="min-w-[120px]"
        >
          {isApproving ? (
            <div className="flex items-center">
              <div className="mr-2 h-4 w-4 animate-spin rounded-full border-2 border-current border-t-transparent"></div>
              Approving...
            </div>
          ) : (
            <div className="flex items-center">
              <CheckCircle className="mr-2 h-4 w-4" />
              Approve Transaction
            </div>
          )}
        </Button>
      </div>
    </div>
  );
}