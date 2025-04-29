"use client";

import { useEffect } from "react";
import { registerServiceWorker } from "@/lib/utils/service-worker";

export function ServiceWorkerInit() {
  useEffect(() => {
    registerServiceWorker();
  }, []);

  return null;
}