import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import "./globals.css";
import { Header } from "@/components/layout/header";
import { Footer } from "@/components/layout/footer";
import { WalletProvider } from "@/context/wallet-context";
import { ThemeProvider } from "@/context/theme-provider";
import { I18nProvider } from "@/context/i18n-provider";
import { SecurityProvider } from "@/context/security-provider";
import { OnboardingProvider } from "@/context/onboarding-provider";
import { Toaster } from "@/components/ui/toaster";
import { ServiceWorkerInit } from "@/app/service-worker-init";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "Phoenix Protocol - Smart Contract Recovery & UX Protection",
  description: "Phoenix Protocol is a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems, enabling secure, verified rollback of smart contract operations.",
  keywords: "blockchain, smart contracts, DeFi, NFT, recovery, security, Avalanche, Cardano",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased`}
      >
        <ThemeProvider
          attribute="class"
          defaultTheme="system"
          enableSystem
          disableTransitionOnChange
        >
          <I18nProvider>
            <WalletProvider>
              <SecurityProvider>
                <OnboardingProvider>
                  <div className="flex min-h-screen flex-col">
                    <Header />
                    <main className="flex-1">{children}</main>
                    <Footer />
                  </div>
                  <Toaster />
                  <ServiceWorkerInit />
                </OnboardingProvider>
              </SecurityProvider>
            </WalletProvider>
          </I18nProvider>
        </ThemeProvider>
      </body>
    </html>
  );
}
