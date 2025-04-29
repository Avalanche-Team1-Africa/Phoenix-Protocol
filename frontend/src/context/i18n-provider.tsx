"use client";

import React, { useEffect } from 'react';
import { I18nextProvider } from 'react-i18next';
import i18n from '@/lib/i18n';
import { i18nConfig } from '@/app/i18n-config';

export function I18nProvider({ children }: { children: React.ReactNode }) {
  useEffect(() => {
    // This is just to ensure i18n is initialized on the client side
    // and to handle any client-side specific initialization
    if (typeof window !== 'undefined') {
      const savedLanguage = localStorage.getItem('phoenixLanguage');
      if (savedLanguage && i18nConfig.locales.includes(savedLanguage) && i18n.language !== savedLanguage) {
        i18n.changeLanguage(savedLanguage);
      }
    }
  }, []);

  return (
    <I18nextProvider i18n={i18n}>
      {children}
    </I18nextProvider>
  );
}