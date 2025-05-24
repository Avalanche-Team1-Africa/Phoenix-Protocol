"use client";

import React from "react";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { AppError } from "@/lib/utils/error-handler";
import { ArrowRight, RefreshCw, HelpCircle, AlertTriangle, ExternalLink } from "lucide-react";

interface FallbackOption {
  id: string;
  title: string;
  description: string;
  icon: React.ReactNode;
  action: () => void;
  primary?: boolean;
}

interface FallbackOptionsProps {
  error: AppError;
  options: FallbackOption[];
  title?: string;
  description?: string;
}

export function FallbackOptions({
  error,
  options,
  title = "Something went wrong",
  description = "We encountered an issue, but don't worry! Here are some options to help you continue:",
}: FallbackOptionsProps) {
  return (
    <div className="space-y-6">
      <div className="rounded-lg border border-red-500/30 bg-red-500/5 p-4">
        <div className="flex items-start">
          <AlertTriangle className="mr-3 mt-0.5 h-5 w-5 text-red-500" />
          <div>
            <h3 className="font-medium text-red-500">{error.message}</h3>
            {error.details && (
              <p className="mt-1 text-sm text-red-500/80">{error.details}</p>
            )}
          </div>
        </div>
      </div>

      <div className="text-center">
        <h2 className="text-xl font-bold">{title}</h2>
        <p className="mt-2 text-muted-foreground">{description}</p>
      </div>

      <div className="grid grid-cols-1 gap-4 md:grid-cols-2">
        {options.map((option) => (
          <Card
            key={option.id}
            className={`cursor-pointer transition-all hover:shadow-md ${
              option.primary
                ? "border-orange-500/30 bg-orange-500/5"
                : "border-muted-foreground/20"
            }`}
            onClick={option.action}
          >
            <CardHeader className="pb-2">
              <div className="mb-2 flex h-8 w-8 items-center justify-center rounded-full bg-muted">
                {option.icon}
              </div>
              <CardTitle className="text-base">{option.title}</CardTitle>
            </CardHeader>
            <CardContent>
              <CardDescription>{option.description}</CardDescription>
            </CardContent>
            <CardFooter className="pt-0">
              <Button
                variant={option.primary ? "gradient" : "outline"}
                size="sm"
                className="w-full"
              >
                {option.title}
                <ArrowRight className="ml-2 h-4 w-4" />
              </Button>
            </CardFooter>
          </Card>
        ))}
      </div>
    </div>
  );
}

// Example usage:
export function createDefaultFallbackOptions(
  error: AppError,
  onRetry: () => void,
  onCancel: () => void
): FallbackOption[] {
  return [
    {
      id: "retry",
      title: "Try Again",
      description: "Attempt the operation again with the same parameters.",
      icon: <RefreshCw className="h-4 w-4" />,
      action: onRetry,
      primary: true,
    },
    {
      id: "support",
      title: "Get Help",
      description: "Contact our support team for assistance with this issue.",
      icon: <HelpCircle className="h-4 w-4" />,
      action: () => window.open("https://support.phoenixprotocol.io", "_blank"),
    },
    {
      id: "docs",
      title: "View Documentation",
      description: "Check our documentation for troubleshooting steps.",
      icon: <ExternalLink className="h-4 w-4" />,
      action: () => window.open("https://docs.phoenixprotocol.io/troubleshooting", "_blank"),
    },
    {
      id: "cancel",
      title: "Cancel Operation",
      description: "Return to the previous screen and try a different approach.",
      icon: <ArrowRight className="h-4 w-4" />,
      action: onCancel,
    },
  ];
}