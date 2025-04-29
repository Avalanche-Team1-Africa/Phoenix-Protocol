import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import { i18nConfig } from '@/app/i18n-config';

// English translations
const enTranslations = {
  common: {
    justNow: 'just now',
    minute: 'minute',
    minutes: 'minutes',
    hour: 'hour',
    hours: 'hours',
    day: 'day',
    days: 'days',
    ago: 'ago',
  },
};

// Spanish translations
const esTranslations = {
  common: {
    justNow: 'ahora mismo',
    minute: 'minuto',
    minutes: 'minutos',
    hour: 'hora',
    hours: 'horas',
    day: 'día',
    days: 'días',
    ago: 'atrás',
  },
};

// French translations
const frTranslations = {
  common: {
    justNow: 'à l\'instant',
    minute: 'minute',
    minutes: 'minutes',
    hour: 'heure',
    hours: 'heures',
    day: 'jour',
    days: 'jours',
    ago: 'il y a',
  },
};

// Chinese translations
const zhTranslations = {
  common: {
    justNow: '刚刚',
    minute: '分钟',
    minutes: '分钟',
    hour: '小时',
    hours: '小时',
    day: '天',
    days: '天',
    ago: '前',
  },
};

// Create resources object from config
const resources = {
  en: { translation: enTranslations },
  es: { translation: esTranslations },
  fr: { translation: frTranslations },
  zh: { translation: zhTranslations },
};

// Initialize i18next
i18n
  .use(initReactI18next)
  .init({
    resources,
    lng: i18nConfig.defaultLocale, // Default language
    fallbackLng: i18nConfig.defaultLocale,
    supportedLngs: i18nConfig.locales,
    interpolation: {
      escapeValue: false, // React already escapes values
    },
  });

// Client-side language detection
// This code will only run on the client
if (typeof window !== 'undefined') {
  const savedLanguage = localStorage.getItem('phoenixLanguage');
  if (savedLanguage && i18nConfig.locales.includes(savedLanguage)) {
    i18n.changeLanguage(savedLanguage);
  }
}

export default i18n;