/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: ["class"],
  content: [
    './src/pages/**/*.{js,ts,jsx,tsx,mdx}',
    './src/components/**/*.{js,ts,jsx,tsx,mdx}',
    './src/app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    container: {
      center: true,
      padding: "2rem",
      screens: {
        "2xl": "1400px",
      },
    },
    extend: {
      colors: {
        // Phoenix Protocol Custom Colors
        phoenix: {
          // Light mode
          light: {
            primary: '#2563eb', // Indigo-600
            'primary-hover': '#1d4ed8',
            'primary-light': '#dbeafe',
            secondary: '#10b981', // Emerald-500
            'secondary-hover': '#059669',
            'secondary-light': '#d1fae5',
            background: '#f9fafb', // Gray-50
            surface: '#ffffff',
            border: '#e5e7eb', // Gray-200
            'border-focus': '#93c5fd',
            'text-primary': '#111827', // Gray-900
            'text-secondary': '#6b7280', // Gray-500
            accent: '#6366f1', // Indigo-500
            warning: '#f59e0b', // Amber-500
            'warning-light': '#fef3c7',
            error: '#ef4444', // Red-500
            'error-light': '#fee2e2',
            success: '#10b981', // Emerald-500
            'success-light': '#d1fae5',
            info: '#3b82f6', // Blue-500
            'info-light': '#dbeafe',
          },
          // Dark mode
          dark: {
            primary: '#3b82f6', // Blue-500
            'primary-hover': '#2563eb',
            'primary-light': '#1e3a8a',
            secondary: '#22c55e', // Green-500
            'secondary-hover': '#16a34a',
            'secondary-light': '#064e3b',
            background: '#0f172a', // Slate-900
            surface: '#1e293b', // Slate-800
            border: '#334155', // Slate-700
            'border-focus': '#60a5fa',
            'text-primary': '#f1f5f9', // Slate-100
            'text-secondary': '#94a3b8', // Slate-400
            accent: '#8b5cf6', // Violet-500
            warning: '#fbbf24', // Amber-400
            'warning-light': '#78350f',
            error: '#f87171', // Red-400
            'error-light': '#7f1d1d',
            success: '#22c55e', // Green-500
            'success-light': '#064e3b',
            info: '#3b82f6', // Blue-500
            'info-light': '#1e3a8a',
          }
        },
        
        // Default shadcn-ui variables
        border: "var(--border)",
        input: "var(--input)",
        ring: "var(--ring)",
        background: "var(--background)",
        foreground: "var(--foreground)",
        primary: {
          DEFAULT: "var(--primary)",
          foreground: "var(--primary-foreground)",
        },
        secondary: {
          DEFAULT: "var(--secondary)",
          foreground: "var(--secondary-foreground)",
        },
        destructive: {
          DEFAULT: "var(--destructive)",
          foreground: "var(--destructive-foreground)",
        },
        muted: {
          DEFAULT: "var(--muted)",
          foreground: "var(--muted-foreground)",
        },
        accent: {
          DEFAULT: "var(--accent)",
          foreground: "var(--accent-foreground)",
        },
        card: {
          DEFAULT: "var(--card-background)",
          foreground: "var(--card-foreground)",
        },
        popover: {
          DEFAULT: "var(--card-background)",
          foreground: "var(--card-foreground)",
        },
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      keyframes: {
        "accordion-down": {
          from: { height: 0 },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: 0 },
        },
        fadeIn: {
          from: { opacity: 0, transform: "translateY(10px)" },
          to: { opacity: 1, transform: "translateY(0)" },
        },
        pulse: {
          '0%, 100%': { opacity: 1 },
          '50%': { opacity: 0.5 },
        },
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
        "fadeIn": "fadeIn 0.3s ease-out forwards",
        "pulse": "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite",
      },
      backgroundImage: {
        'phoenix-gradient': 'linear-gradient(135deg, #2563eb 0%, #8b5cf6 100%)',
        'phoenix-gradient-dark': 'linear-gradient(135deg, #3b82f6 0%, #8b5cf6 100%)',
      },
    },
  },
  plugins: [require("tailwindcss-animate")],
}