"use client";

import React, { useState } from "react";
import { CheckCircle, AlertCircle, Loader2, ArrowRight } from "lucide-react";
import { Button } from "@/components/ui/button";

export type TransactionStep = {
  id: string;
  title: string;
  description: string;
  status: "pending" | "active" | "completed" | "error";
};

interface TransactionStepperProps {
  steps: TransactionStep[];
  currentStepIndex: number;
  onNext?: () => void;
  onPrevious?: () => void;
  onRetry?: () => void;
  isProcessing?: boolean;
  error?: string | null;
}

export function TransactionStepper({
  steps,
  currentStepIndex,
  onNext,
  onPrevious,
  onRetry,
  isProcessing = false,
  error = null,
}: TransactionStepperProps) {
  return (
    <div className="w-full">
      {/* Progress Bar */}
      <div className="relative mb-8">
        <div className="absolute top-1/2 left-0 right-0 h-1 -translate-y-1/2 bg-muted"></div>
        <div className="relative flex justify-between">
          {steps.map((step, index) => {
            const isCompleted = step.status === "completed";
            const isActive = step.status === "active";
            const isError = step.status === "error";
            
            return (
              <div key={step.id} className="flex flex-col items-center">
                <div
                  className={`relative z-10 flex h-8 w-8 items-center justify-center rounded-full border-2 ${
                    isCompleted
                      ? "border-green-500 bg-green-500 text-white"
                      : isError
                      ? "border-red-500 bg-red-500 text-white"
                      : isActive
                      ? "border-orange-500 bg-orange-500 text-white"
                      : "border-muted-foreground bg-background text-muted-foreground"
                  }`}
                >
                  {isCompleted ? (
                    <CheckCircle className="h-4 w-4" />
                  ) : isError ? (
                    <AlertCircle className="h-4 w-4" />
                  ) : isActive && isProcessing ? (
                    <Loader2 className="h-4 w-4 animate-spin" />
                  ) : (
                    <span className="text-xs font-medium">{index + 1}</span>
                  )}
                </div>
                <div className="mt-2 text-center">
                  <p
                    className={`text-sm font-medium ${
                      isActive
                        ? "text-foreground"
                        : isCompleted
                        ? "text-green-500"
                        : isError
                        ? "text-red-500"
                        : "text-muted-foreground"
                    }`}
                  >
                    {step.title}
                  </p>
                </div>
              </div>
            );
          })}
        </div>
      </div>

      {/* Current Step Content */}
      <div className="mb-6 rounded-lg border bg-card p-6">
        <h3 className="mb-2 text-lg font-semibold">
          {steps[currentStepIndex].title}
        </h3>
        <p className="text-muted-foreground">
          {steps[currentStepIndex].description}
        </p>
        
        {error && (
          <div className="mt-4 rounded-md bg-red-500/10 p-3 text-sm text-red-500">
            <div className="flex items-start">
              <AlertCircle className="mr-2 h-4 w-4 mt-0.5" />
              <span>{error}</span>
            </div>
          </div>
        )}
      </div>

      {/* Navigation Buttons */}
      <div className="flex justify-between">
        <Button
          variant="outline"
          onClick={onPrevious}
          disabled={currentStepIndex === 0 || isProcessing}
        >
          Back
        </Button>
        
        {steps[currentStepIndex].status === "error" ? (
          <Button
            variant="destructive"
            onClick={onRetry}
            disabled={isProcessing}
          >
            {isProcessing ? (
              <>
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                Retrying...
              </>
            ) : (
              "Retry"
            )}
          </Button>
        ) : (
          <Button
            variant={currentStepIndex === steps.length - 1 ? "gradient" : "default"}
            onClick={onNext}
            disabled={isProcessing || steps[currentStepIndex].status === "pending"}
          >
            {isProcessing ? (
              <>
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                Processing...
              </>
            ) : currentStepIndex === steps.length - 1 ? (
              "Complete"
            ) : (
              <>
                Next
                <ArrowRight className="ml-2 h-4 w-4" />
              </>
            )}
          </Button>
        )}
      </div>
    </div>
  );
}