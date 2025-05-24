"use client";

import React, { createContext, useContext, useState, useCallback } from "react";
import { AppError, parseProviderError, createAppError, ErrorCodes } from "@/lib/utils/error-handler";
import { useToast } from "@/components/ui/use-toast";

interface ErrorContextType {
  error: AppError | null;
  setError: (error: AppError | null) => void;
  handleError: (error: any) => AppError;
  clearError: () => void;
  isError: boolean;
}

const ErrorContext = createContext<ErrorContextType | undefined>(undefined);

export function ErrorProvider({ children }: { children: React.ReactNode }) {
  const [error, setError] = useState<AppError | null>(null);
  const { toast } = useToast();

  const handleError = useCallback((err: any): AppError => {
    // Parse the error
    const parsedError = err instanceof Error 
      ? parseProviderError(err) 
      : typeof err === "string" 
        ? createAppError(ErrorCodes.UNKNOWN_ERROR, err) 
        : err as AppError;
    
    // Set the error in state
    setError(parsedError);
    
    // Show toast for non-user-rejected errors
    if (parsedError.code !== ErrorCodes.TX_REJECTED && 
        parsedError.code !== ErrorCodes.WALLET_CONNECTION_REJECTED) {
      toast({
        title: parsedError.message,
        description: parsedError.suggestedAction,
        variant: parsedError.severity === "critical" || parsedError.severity === "high" 
          ? "destructive" 
          : parsedError.severity === "medium" 
            ? "default" 
            : "secondary",
      });
    }
    
    // Log to console for debugging
    console.error("Error handled:", parsedError);
    
    return parsedError;
  }, [toast]);

  const clearError = useCallback(() => {
    setError(null);
  }, []);

  const value = {
    error,
    setError,
    handleError,
    clearError,
    isError: error !== null,
  };

  return <ErrorContext.Provider value={value}>{children}</ErrorContext.Provider>;
}

export function useError() {
  const context = useContext(ErrorContext);
  if (context === undefined) {
    throw new Error("useError must be used within an ErrorProvider");
  }
  return context;
}