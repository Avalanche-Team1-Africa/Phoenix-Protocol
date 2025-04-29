"use client";

import React, { useState, useEffect, useRef } from "react";
import { AnimatePresence, motion } from "framer-motion";
import { X } from "lucide-react";

interface TooltipProps {
  id: string;
  content: React.ReactNode;
  position?: "top" | "right" | "bottom" | "left";
  children: React.ReactNode;
  delay?: number;
  width?: number;
  showArrow?: boolean;
  persistent?: boolean;
  onClose?: () => void;
  onComplete?: () => void;
}

export function OnboardingTooltip({
  id,
  content,
  position = "bottom",
  children,
  delay = 0,
  width = 250,
  showArrow = true,
  persistent = false,
  onClose,
  onComplete,
}: TooltipProps) {
  const [isVisible, setIsVisible] = useState(false);
  const [hasBeenSeen, setHasBeenSeen] = useState(false);
  const targetRef = useRef<HTMLDivElement>(null);
  const tooltipRef = useRef<HTMLDivElement>(null);
  
  // Check if tooltip has been seen before
  useEffect(() => {
    const seenTooltips = localStorage.getItem("phoenixSeenTooltips");
    const seenArray = seenTooltips ? JSON.parse(seenTooltips) : [];
    
    if (seenArray.includes(id)) {
      setHasBeenSeen(true);
    }
  }, [id]);
  
  // Show tooltip after delay
  useEffect(() => {
    if (hasBeenSeen && !persistent) return;
    
    const timer = setTimeout(() => {
      setIsVisible(true);
    }, delay);
    
    return () => clearTimeout(timer);
  }, [delay, hasBeenSeen, persistent]);
  
  // Handle click outside to close tooltip
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        tooltipRef.current &&
        !tooltipRef.current.contains(event.target as Node) &&
        targetRef.current &&
        !targetRef.current.contains(event.target as Node)
      ) {
        handleClose();
      }
    };
    
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, []);
  
  // Mark tooltip as seen and close it
  const handleClose = () => {
    setIsVisible(false);
    
    if (!hasBeenSeen) {
      const seenTooltips = localStorage.getItem("phoenixSeenTooltips");
      const seenArray = seenTooltips ? JSON.parse(seenTooltips) : [];
      
      if (!seenArray.includes(id)) {
        seenArray.push(id);
        localStorage.setItem("phoenixSeenTooltips", JSON.stringify(seenArray));
        setHasBeenSeen(true);
      }
    }
    
    if (onClose) {
      onClose();
    }
  };
  
  // Handle completion
  const handleComplete = () => {
    handleClose();
    
    if (onComplete) {
      onComplete();
    }
  };
  
  // Calculate position styles
  const getPositionStyles = () => {
    switch (position) {
      case "top":
        return {
          bottom: "calc(100% + 10px)",
          left: "50%",
          transform: "translateX(-50%)",
        };
      case "right":
        return {
          left: "calc(100% + 10px)",
          top: "50%",
          transform: "translateY(-50%)",
        };
      case "left":
        return {
          right: "calc(100% + 10px)",
          top: "50%",
          transform: "translateY(-50%)",
        };
      case "bottom":
      default:
        return {
          top: "calc(100% + 10px)",
          left: "50%",
          transform: "translateX(-50%)",
        };
    }
  };
  
  // Calculate arrow styles
  const getArrowStyles = () => {
    switch (position) {
      case "top":
        return {
          bottom: "-5px",
          left: "50%",
          transform: "translateX(-50%) rotate(45deg)",
          borderRight: "1px solid rgba(0, 0, 0, 0.1)",
          borderBottom: "1px solid rgba(0, 0, 0, 0.1)",
        };
      case "right":
        return {
          left: "-5px",
          top: "50%",
          transform: "translateY(-50%) rotate(45deg)",
          borderLeft: "1px solid rgba(0, 0, 0, 0.1)",
          borderBottom: "1px solid rgba(0, 0, 0, 0.1)",
        };
      case "left":
        return {
          right: "-5px",
          top: "50%",
          transform: "translateY(-50%) rotate(45deg)",
          borderRight: "1px solid rgba(0, 0, 0, 0.1)",
          borderTop: "1px solid rgba(0, 0, 0, 0.1)",
        };
      case "bottom":
      default:
        return {
          top: "-5px",
          left: "50%",
          transform: "translateX(-50%) rotate(45deg)",
          borderLeft: "1px solid rgba(0, 0, 0, 0.1)",
          borderTop: "1px solid rgba(0, 0, 0, 0.1)",
        };
    }
  };
  
  return (
    <div className="relative inline-block" ref={targetRef}>
      {children}
      
      <AnimatePresence>
        {isVisible && (
          <motion.div
            ref={tooltipRef}
            initial={{ opacity: 0, scale: 0.9 }}
            animate={{ opacity: 1, scale: 1 }}
            exit={{ opacity: 0, scale: 0.9 }}
            transition={{ duration: 0.2 }}
            style={{
              ...getPositionStyles(),
              width: `${width}px`,
              zIndex: 50,
            }}
            className="absolute bg-background rounded-lg shadow-lg border border-border p-4"
          >
            {showArrow && (
              <div
                className="absolute w-3 h-3 bg-background"
                style={getArrowStyles()}
              />
            )}
            
            <button
              onClick={handleClose}
              className="absolute top-2 right-2 text-muted-foreground hover:text-foreground"
            >
              <X className="h-4 w-4" />
            </button>
            
            <div className="pt-2">{content}</div>
            
            <div className="flex justify-end mt-4 space-x-2">
              <button
                onClick={handleComplete}
                className="px-3 py-1 text-xs font-medium bg-orange-500 text-white rounded-md hover:bg-orange-600"
              >
                Got it
              </button>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  );
}