"use client";

import React, { useState, useEffect, useRef } from "react";
import { X } from "lucide-react";

interface BottomSheetProps {
  isOpen: boolean;
  onClose: () => void;
  title?: string;
  children: React.ReactNode;
  showCloseButton?: boolean;
  height?: "auto" | "full" | "half";
}

export function BottomSheet({
  isOpen,
  onClose,
  title,
  children,
  showCloseButton = true,
  height = "auto",
}: BottomSheetProps) {
  const [isVisible, setIsVisible] = useState(false);
  const [startY, setStartY] = useState(0);
  const [currentY, setCurrentY] = useState(0);
  const [isDragging, setIsDragging] = useState(false);
  const sheetRef = useRef<HTMLDivElement>(null);

  // Handle visibility
  useEffect(() => {
    if (isOpen) {
      setIsVisible(true);
    } else {
      setIsVisible(false);
    }
  }, [isOpen]);

  // Handle touch start
  const handleTouchStart = (e: React.TouchEvent) => {
    const touch = e.touches[0];
    setStartY(touch.clientY);
    setIsDragging(true);
  };

  // Handle touch move
  const handleTouchMove = (e: React.TouchEvent) => {
    if (!isDragging) return;
    const touch = e.touches[0];
    const deltaY = touch.clientY - startY;
    if (deltaY > 0) {
      setCurrentY(deltaY);
    }
  };

  // Handle touch end
  const handleTouchEnd = () => {
    if (!isDragging) return;
    setIsDragging(false);
    
    // If dragged more than 100px down, close the sheet
    if (currentY > 100) {
      onClose();
    }
    
    setCurrentY(0);
  };

  // Close on backdrop click
  const handleBackdropClick = (e: React.MouseEvent) => {
    if (e.target === e.currentTarget) {
      onClose();
    }
  };

  if (!isOpen && !isVisible) return null;

  return (
    <div
      className={`fixed inset-0 z-50 flex items-end justify-center bg-black/50 transition-opacity duration-300 ${
        isVisible ? "opacity-100" : "opacity-0"
      }`}
      onClick={handleBackdropClick}
    >
      <div
        ref={sheetRef}
        className={`w-full rounded-t-xl bg-background shadow-lg transition-transform duration-300 ${
          isVisible ? "translate-y-0" : "translate-y-full"
        } ${
          height === "full" 
            ? "h-[90vh]" 
            : height === "half" 
            ? "h-[50vh]" 
            : "max-h-[90vh]"
        }`}
        style={{
          transform: isDragging ? `translateY(${currentY}px)` : undefined,
        }}
      >
        {/* Drag handle */}
        <div
          className="flex h-7 w-full items-center justify-center"
          onTouchStart={handleTouchStart}
          onTouchMove={handleTouchMove}
          onTouchEnd={handleTouchEnd}
        >
          <div className="h-1 w-16 rounded-full bg-muted-foreground/30"></div>
        </div>

        {/* Header */}
        {(title || showCloseButton) && (
          <div className="flex items-center justify-between border-b p-4">
            {title && <h3 className="text-lg font-semibold">{title}</h3>}
            {showCloseButton && (
              <button
                onClick={onClose}
                className="rounded-full p-1 text-muted-foreground hover:bg-muted hover:text-foreground"
              >
                <X className="h-5 w-5" />
              </button>
            )}
          </div>
        )}

        {/* Content */}
        <div className="overflow-auto p-4">{children}</div>
      </div>
    </div>
  );
}