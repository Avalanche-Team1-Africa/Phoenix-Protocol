"use client";

import React, { useState } from "react";
import Image from "next/image";
import { Button } from "@/components/ui/button";
import { CheckCircle, ChevronLeft, ChevronRight, X } from "lucide-react";

interface TutorialStep {
  title: string;
  description: string;
  image: string;
}

interface TutorialModalProps {
  isOpen: boolean;
  onClose: () => void;
  steps: TutorialStep[];
  onComplete: () => void;
}

export function TutorialModal({
  isOpen,
  onClose,
  steps,
  onComplete,
}: TutorialModalProps) {
  const [currentStep, setCurrentStep] = useState(0);
  
  if (!isOpen) return null;
  
  const handleNext = () => {
    if (currentStep < steps.length - 1) {
      setCurrentStep(currentStep + 1);
    } else {
      handleComplete();
    }
  };
  
  const handlePrevious = () => {
    if (currentStep > 0) {
      setCurrentStep(currentStep - 1);
    }
  };
  
  const handleComplete = () => {
    onComplete();
    onClose();
    setCurrentStep(0);
  };
  
  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="bg-background rounded-lg shadow-lg w-full max-w-3xl p-6 relative">
        <button
          onClick={onClose}
          className="absolute top-4 right-4 text-muted-foreground hover:text-foreground"
        >
          <X className="h-5 w-5" />
        </button>
        
        <div className="flex flex-col md:flex-row gap-6">
          <div className="w-full md:w-1/2 relative rounded-lg overflow-hidden bg-muted">
            <Image
              src={steps[currentStep].image}
              alt={steps[currentStep].title}
              width={500}
              height={300}
              className="object-cover"
            />
          </div>
          
          <div className="w-full md:w-1/2 flex flex-col">
            <div className="mb-2">
              <div className="flex items-center space-x-2 mb-4">
                {steps.map((_, index) => (
                  <div
                    key={index}
                    className={`h-2 rounded-full ${
                      index === currentStep
                        ? "w-8 bg-orange-500"
                        : "w-2 bg-muted-foreground/30"
                    }`}
                  />
                ))}
              </div>
              
              <h2 className="text-2xl font-bold mb-2">
                {steps[currentStep].title}
              </h2>
              
              <p className="text-muted-foreground mb-6">
                {steps[currentStep].description}
              </p>
            </div>
            
            <div className="mt-auto flex justify-between items-center">
              <Button
                variant="outline"
                onClick={handlePrevious}
                disabled={currentStep === 0}
              >
                <ChevronLeft className="h-4 w-4 mr-2" />
                Previous
              </Button>
              
              <Button variant="gradient" onClick={handleNext}>
                {currentStep < steps.length - 1 ? (
                  <>
                    Next
                    <ChevronRight className="h-4 w-4 ml-2" />
                  </>
                ) : (
                  <>
                    Complete
                    <CheckCircle className="h-4 w-4 ml-2" />
                  </>
                )}
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}