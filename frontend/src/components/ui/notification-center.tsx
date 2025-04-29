"use client";

import * as React from "react";
import { Bell } from "lucide-react";
import { Button } from "@/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { useTranslation } from "react-i18next";

interface Notification {
  id: string;
  title: string;
  message: string;
  type: "info" | "success" | "warning" | "error";
  timestamp: number;
  read: boolean;
}

export function NotificationCenter() {
  const { t } = useTranslation();
  const [notifications, setNotifications] = React.useState<Notification[]>([]);
  const [unreadCount, setUnreadCount] = React.useState(0);

  // Mock notifications for demo
  React.useEffect(() => {
    const mockNotifications: Notification[] = [
      {
        id: "1",
        title: "Transaction Confirmed",
        message: "Your swap of 100 USDC to AVAX has been confirmed.",
        type: "success",
        timestamp: Date.now() - 1000 * 60 * 5, // 5 minutes ago
        read: false,
      },
      {
        id: "2",
        title: "Guardian Approval",
        message: "A guardian has approved your wallet recovery request.",
        type: "info",
        timestamp: Date.now() - 1000 * 60 * 30, // 30 minutes ago
        read: false,
      },
      {
        id: "3",
        title: "Dispute Resolution",
        message: "Your dispute case #823975 has been resolved in your favor.",
        type: "success",
        timestamp: Date.now() - 1000 * 60 * 60 * 2, // 2 hours ago
        read: true,
      },
    ];

    setNotifications(mockNotifications);
    setUnreadCount(mockNotifications.filter(n => !n.read).length);
  }, []);

  const markAsRead = (id: string) => {
    setNotifications(prev => 
      prev.map(notification => 
        notification.id === id 
          ? { ...notification, read: true } 
          : notification
      )
    );
    setUnreadCount(prev => Math.max(0, prev - 1));
  };

  const markAllAsRead = () => {
    setNotifications(prev => 
      prev.map(notification => ({ ...notification, read: true }))
    );
    setUnreadCount(0);
  };

  const formatTime = (timestamp: number) => {
    const now = Date.now();
    const diff = now - timestamp;
    
    if (diff < 1000 * 60) {
      return t("common.justNow");
    } else if (diff < 1000 * 60 * 60) {
      const minutes = Math.floor(diff / (1000 * 60));
      return `${minutes} ${minutes === 1 ? t("common.minute") : t("common.minutes")} ${t("common.ago")}`;
    } else if (diff < 1000 * 60 * 60 * 24) {
      const hours = Math.floor(diff / (1000 * 60 * 60));
      return `${hours} ${hours === 1 ? t("common.hour") : t("common.hours")} ${t("common.ago")}`;
    } else {
      const days = Math.floor(diff / (1000 * 60 * 60 * 24));
      return `${days} ${days === 1 ? t("common.day") : t("common.days")} ${t("common.ago")}`;
    }
  };

  const getNotificationIcon = (type: string) => {
    switch (type) {
      case "success":
        return "🟢";
      case "warning":
        return "🟠";
      case "error":
        return "🔴";
      default:
        return "🔵";
    }
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="ghost" size="icon" className="relative">
          <Bell className="h-5 w-5" />
          {unreadCount > 0 && (
            <span className="absolute -top-1 -right-1 flex h-5 w-5 items-center justify-center rounded-full bg-red-500 text-xs text-white">
              {unreadCount}
            </span>
          )}
          <span className="sr-only">Notifications</span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" className="w-80">
        <DropdownMenuLabel className="flex justify-between items-center">
          <span>Notifications</span>
          {unreadCount > 0 && (
            <Button 
              variant="ghost" 
              size="sm" 
              onClick={markAllAsRead}
              className="text-xs h-auto py-1"
            >
              Mark all as read
            </Button>
          )}
        </DropdownMenuLabel>
        <DropdownMenuSeparator />
        {notifications.length === 0 ? (
          <div className="py-4 text-center text-sm text-muted-foreground">
            No notifications
          </div>
        ) : (
          notifications.map((notification) => (
            <DropdownMenuItem
              key={notification.id}
              className={`flex flex-col items-start p-3 ${
                !notification.read ? "bg-muted/50" : ""
              }`}
              onClick={() => markAsRead(notification.id)}
            >
              <div className="flex w-full justify-between">
                <span className="font-medium flex items-center gap-2">
                  {getNotificationIcon(notification.type)} {notification.title}
                </span>
                <span className="text-xs text-muted-foreground">
                  {formatTime(notification.timestamp)}
                </span>
              </div>
              <p className="mt-1 text-sm text-muted-foreground">
                {notification.message}
              </p>
            </DropdownMenuItem>
          ))
        )}
        <DropdownMenuSeparator />
        <DropdownMenuItem className="justify-center text-sm font-medium">
          View all notifications
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}