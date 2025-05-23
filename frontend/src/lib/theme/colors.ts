// Phoenix Protocol Color Palette

export const lightColors = {
  // Primary colors
  primary: '#2563eb', // Indigo-600 - Call-to-action, primary buttons
  primaryHover: '#1d4ed8', // Slightly darker for hover states
  primaryLight: '#dbeafe', // Light version for backgrounds
  
  // Secondary colors
  secondary: '#10b981', // Emerald-500 - Status highlights, confirmations
  secondaryHover: '#059669', // Slightly darker for hover states
  secondaryLight: '#d1fae5', // Light version for backgrounds
  
  // Background colors
  background: '#f9fafb', // Gray-50 - Main background
  surface: '#ffffff', // Cards, forms, modals
  
  // Border colors
  border: '#e5e7eb', // Gray-200 - Dividers, input borders
  borderFocus: '#93c5fd', // Blue-300 - Focused input borders
  
  // Text colors
  textPrimary: '#111827', // Gray-900 - Main text
  textSecondary: '#6b7280', // Gray-500 - Subtext, labels
  
  // Accent colors
  accent: '#6366f1', // Indigo-500 - Branding, focused elements
  
  // Status colors
  warning: '#f59e0b', // Amber-500 - Cautionary alerts
  warningLight: '#fef3c7', // Amber-100 - Warning backgrounds
  error: '#ef4444', // Red-500 - Errors, rejections
  errorLight: '#fee2e2', // Red-100 - Error backgrounds
  success: '#10b981', // Emerald-500 - Success states
  successLight: '#d1fae5', // Emerald-100 - Success backgrounds
  info: '#3b82f6', // Blue-500 - Information
  infoLight: '#dbeafe', // Blue-100 - Info backgrounds
};

export const darkColors = {
  // Primary colors
  primary: '#3b82f6', // Blue-500 - CTA buttons, links
  primaryHover: '#2563eb', // Slightly darker for hover states
  primaryLight: '#1e3a8a', // Dark blue for backgrounds
  
  // Secondary colors
  secondary: '#22c55e', // Green-500 - Confirmations, success states
  secondaryHover: '#16a34a', // Slightly darker for hover states
  secondaryLight: '#064e3b', // Dark green for backgrounds
  
  // Background colors
  background: '#0f172a', // Slate-900 - Main background
  surface: '#1e293b', // Slate-800 - Cards and modals
  
  // Border colors
  border: '#334155', // Slate-700 - Subtle dividers
  borderFocus: '#60a5fa', // Blue-400 - Focused input borders
  
  // Text colors
  textPrimary: '#f1f5f9', // Slate-100 - Light text on dark backgrounds
  textSecondary: '#94a3b8', // Slate-400 - Subdued text
  
  // Accent colors
  accent: '#8b5cf6', // Violet-500 - Focused elements, branding
  
  // Status colors
  warning: '#fbbf24', // Amber-400 - Warning banners
  warningLight: '#78350f', // Dark amber for backgrounds
  error: '#f87171', // Red-400 - Rejection states
  errorLight: '#7f1d1d', // Dark red for backgrounds
  success: '#22c55e', // Green-500 - Success states
  successLight: '#064e3b', // Dark green for backgrounds
  info: '#3b82f6', // Blue-500 - Information
  infoLight: '#1e3a8a', // Dark blue for backgrounds
};

// Helper function to get colors based on theme
export function getColors(isDarkMode: boolean) {
  return isDarkMode ? darkColors : lightColors;
}

// CSS variables for use in styled components or CSS
export const cssVariables = {
  light: `
    --color-primary: ${lightColors.primary};
    --color-primary-hover: ${lightColors.primaryHover};
    --color-primary-light: ${lightColors.primaryLight};
    --color-secondary: ${lightColors.secondary};
    --color-secondary-hover: ${lightColors.secondaryHover};
    --color-secondary-light: ${lightColors.secondaryLight};
    --color-background: ${lightColors.background};
    --color-surface: ${lightColors.surface};
    --color-border: ${lightColors.border};
    --color-border-focus: ${lightColors.borderFocus};
    --color-text-primary: ${lightColors.textPrimary};
    --color-text-secondary: ${lightColors.textSecondary};
    --color-accent: ${lightColors.accent};
    --color-warning: ${lightColors.warning};
    --color-warning-light: ${lightColors.warningLight};
    --color-error: ${lightColors.error};
    --color-error-light: ${lightColors.errorLight};
    --color-success: ${lightColors.success};
    --color-success-light: ${lightColors.successLight};
    --color-info: ${lightColors.info};
    --color-info-light: ${lightColors.infoLight};
  `,
  dark: `
    --color-primary: ${darkColors.primary};
    --color-primary-hover: ${darkColors.primaryHover};
    --color-primary-light: ${darkColors.primaryLight};
    --color-secondary: ${darkColors.secondary};
    --color-secondary-hover: ${darkColors.secondaryHover};
    --color-secondary-light: ${darkColors.secondaryLight};
    --color-background: ${darkColors.background};
    --color-surface: ${darkColors.surface};
    --color-border: ${darkColors.border};
    --color-border-focus: ${darkColors.borderFocus};
    --color-text-primary: ${darkColors.textPrimary};
    --color-text-secondary: ${darkColors.textSecondary};
    --color-accent: ${darkColors.accent};
    --color-warning: ${darkColors.warning};
    --color-warning-light: ${darkColors.warningLight};
    --color-error: ${darkColors.error};
    --color-error-light: ${darkColors.errorLight};
    --color-success: ${darkColors.success};
    --color-success-light: ${darkColors.successLight};
    --color-info: ${darkColors.info};
    --color-info-light: ${darkColors.infoLight};
  `
};