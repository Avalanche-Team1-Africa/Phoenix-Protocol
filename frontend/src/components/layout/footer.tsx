import React from "react";
import Link from "next/link";

export function Footer() {
  return (
    <footer className="border-t bg-background">
      <div className="container py-8 md:py-12">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
          <div className="space-y-3">
            <h3 className="text-lg font-semibold">Phoenix Protocol</h3>
            <p className="text-sm text-muted-foreground">
              Smart contract recovery and UX protection middleware for DeFi and NFT ecosystems.
            </p>
          </div>
          <div>
            <h3 className="text-lg font-semibold mb-3">Resources</h3>
            <ul className="space-y-2">
              <li>
                <Link href="/docs" className="text-sm text-muted-foreground hover:text-foreground">
                  Documentation
                </Link>
              </li>
              <li>
                <Link href="/developers" className="text-sm text-muted-foreground hover:text-foreground">
                  Developer API
                </Link>
              </li>
              <li>
                <Link href="/whitepaper" className="text-sm text-muted-foreground hover:text-foreground">
                  Whitepaper
                </Link>
              </li>
            </ul>
          </div>
          <div>
            <h3 className="text-lg font-semibold mb-3">Community</h3>
            <ul className="space-y-2">
              <li>
                <Link href="https://discord.gg/phoenix" className="text-sm text-muted-foreground hover:text-foreground">
                  Discord
                </Link>
              </li>
              <li>
                <Link href="https://twitter.com/phoenixprotocol" className="text-sm text-muted-foreground hover:text-foreground">
                  Twitter
                </Link>
              </li>
              <li>
                <Link href="https://github.com/phoenix-protocol" className="text-sm text-muted-foreground hover:text-foreground">
                  GitHub
                </Link>
              </li>
            </ul>
          </div>
          <div>
            <h3 className="text-lg font-semibold mb-3">Legal</h3>
            <ul className="space-y-2">
              <li>
                <Link href="/privacy" className="text-sm text-muted-foreground hover:text-foreground">
                  Privacy Policy
                </Link>
              </li>
              <li>
                <Link href="/terms" className="text-sm text-muted-foreground hover:text-foreground">
                  Terms of Service
                </Link>
              </li>
            </ul>
          </div>
        </div>
        <div className="mt-8 pt-6 border-t">
          <p className="text-sm text-muted-foreground text-center">
            © {new Date().getFullYear()} Phoenix Protocol. All rights reserved.
          </p>
        </div>
      </div>
    </footer>
  );
}