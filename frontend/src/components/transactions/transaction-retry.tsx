"use client";

import React, { useState } from "react";
import { AlertTriangle, ArrowRight, RefreshCw, Loader2 } from "lucide-react";
import { Button } from "@/components/ui/button";
import { AppError, ErrorSeverity } from "@/lib/utils/error-handler";

interface TransactionRetryProps {
  error: AppError;
  onRetry: () => Promise<void>;
  onCancel: () => void;
  maxRetries?: number;
}

export function TransactionRetry({
  error,
  onRetry,
  onCancel,
  maxRetries = 3,
}: TransactionRetryProps) {
  const [isRetrying, setIsRetrying] = useState(false);
  const [retryCount, setRetryCount] = useState(0);
  const [retryError, setRetryError] = useState<string | null>(null);

  const handleRetry = async () => {
    if (retryCount >= maxRetries) {
      setRetryError("Maximum retry attempts reached. Please try again later.");
      return;
    }

    setIsRetrying(true);
    setRetryError(null);

    try {
      await onRetry();
      // If successful, the parent component will handle the UI update
    } catch (err) {
      setRetryError("Retry failed. Please try again.");
      console.error("Retry failed:", err);
    } finally {
      setIsRetrying(false);
      setRetryCount((prev) => prev + 1);
    }
  };

  // Get appropriate color based on error severity
  const getSeverityColor = () => {
    switch (error.severity) {
      case ErrorSeverity.CRITICAL:
      case ErrorSeverity.HIGH:
        return "bg-red-500/10 text-red-500 border-red-500/30";
      case ErrorSeverity.MEDIUM:
        return "bg-orange-500/10 text-orange-500 border-orange-500/30";
      case ErrorSeverity.LOW:
      case ErrorSeverity.INFO:
        return "bg-yellow-500/10 text-yellow-500 border-yellow-500/30";
      default:
        return "bg-muted/50 text-muted-foreground border-muted-foreground/30";
    }
  };

  return (
    <div className="space-y-4">
      <div className={`rounded-lg border p-4 ${getSeverityColor()}`}>
        <div className="flex items-start">
          <AlertTriangle className="mr-3 mt-0.5 h-5 w-5 flex-shrink-0" />
          <div>
            <h3 className="font-medium">{error.message}</h3>
            {error.details && (
              <p className="mt-1 text-sm opacity-80">{error.details}</p>
            )}
          </div>
        </div>
      </div>

      {error.suggestedAction && (
        <div className="rounded-lg bg-muted/30 p-4">
          <h4 className="mb-2 text-sm font-medium">Suggested Action</h4>
          <p className="text-sm text-muted-foreground">{error.suggestedAction}</p>
        </div>
      )}

      {retryError && (
        <div className="rounded-lg bg-red-500/10 p-3 text-sm text-red-500">
          {retryError}
        </div>
      )}

      <div className="flex items-center justify-between">
        <div className="text-sm text-muted-foreground">
          {retryCount > 0 && (
            <span>
              Retry attempts: {retryCount}/{maxRetries}
            </span>
          )}
        </div>

        <div className="flex space-x-3">
          <Button variant="outline" onClick={onCancel}>
            Cancel
          </Button>
          {error.recoverable ? (
            <Button
              variant="default"
              onClick={handleRetry}
              disabled={isRetrying || retryCount >= maxRetries}
            >
              {isRetrying ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Retrying...
                </>
              ) : (
                <>
                  <RefreshCw className="mr-2 h-4 w-4" />
                  Retry
                </>
              )}
            </Button>
          ) : (
            <Button variant="default" onClick={onCancel}>
              <ArrowRight className="mr-2 h-4 w-4" />
              Continue
            </Button>
          )}
        </div>
      </div>
    </div>
  );
}