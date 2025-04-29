"use client";

import { useEffect, useState, useRef } from "react";
import Image from "next/image";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { ArrowRight, Shield, RefreshCw, Users, Lock, Zap, CheckCircle } from "lucide-react";

export default function Home() {
  // State for animation
  const [isVisible, setIsVisible] = useState({
    hero: false,
    features: false,
    cases: false,
    stats: false,
    cta: false
  });
  
  // Refs for sections
  const heroRef = useRef(null);
  const featuresRef = useRef(null);
  const casesRef = useRef(null);
  const statsRef = useRef(null);
  const ctaRef = useRef(null);
  
  // Observer for animations
  useEffect(() => {
    const observerOptions = {
      root: null,
      rootMargin: '0px',
      threshold: 0.1
    };
    
    const observerCallback = (entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          if (entry.target === heroRef.current) setIsVisible(prev => ({ ...prev, hero: true }));
          if (entry.target === featuresRef.current) setIsVisible(prev => ({ ...prev, features: true }));
          if (entry.target === casesRef.current) setIsVisible(prev => ({ ...prev, cases: true }));
          if (entry.target === statsRef.current) setIsVisible(prev => ({ ...prev, stats: true }));
          if (entry.target === ctaRef.current) setIsVisible(prev => ({ ...prev, cta: true }));
        }
      });
    };
    
    const observer = new IntersectionObserver(observerCallback, observerOptions);
    
    if (heroRef.current) observer.observe(heroRef.current);
    if (featuresRef.current) observer.observe(featuresRef.current);
    if (casesRef.current) observer.observe(casesRef.current);
    if (statsRef.current) observer.observe(statsRef.current);
    if (ctaRef.current) observer.observe(ctaRef.current);
    
    return () => {
      if (heroRef.current) observer.unobserve(heroRef.current);
      if (featuresRef.current) observer.unobserve(featuresRef.current);
      if (casesRef.current) observer.unobserve(casesRef.current);
      if (statsRef.current) observer.unobserve(statsRef.current);
      if (ctaRef.current) observer.unobserve(ctaRef.current);
    };
  }, []);
  
  // Mock data for recent disputes and rollback cases
  const recentCases = [
    {
      id: "case-001",
      type: "Dispute",
      title: "Token Swap Slippage Exceeded",
      description: "User intended 1% max slippage but received 15% slippage on DEX swap",
      status: "Resolved",
      date: "2 days ago"
    },
    {
      id: "case-002",
      type: "Rollback",
      title: "NFT Purchase Price Mismatch",
      description: "User was charged 2.5 ETH instead of advertised 0.25 ETH for NFT mint",
      status: "Completed",
      date: "3 days ago"
    },
    {
      id: "case-003",
      type: "Recovery",
      title: "Wallet Access Restored",
      description: "User regained access through social recovery after private key loss",
      status: "Completed",
      date: "1 week ago"
    }
  ];

  // Stats data
  const stats = [
    { label: "Transactions Protected", value: "1.2M+", icon: <Shield className="h-6 w-6 text-orange-500" /> },
    { label: "Successful Recoveries", value: "15,000+", icon: <RefreshCw className="h-6 w-6 text-orange-500" /> },
    { label: "Active Users", value: "50,000+", icon: <Users className="h-6 w-6 text-orange-500" /> },
    { label: "Chains Supported", value: "5+", icon: <Lock className="h-6 w-6 text-orange-500" /> }
  ];

  return (
    <>
      {/* Hero Section */}
      <section ref={heroRef} className="py-20 px-4 bg-gradient-to-b from-background to-muted overflow-hidden">
        <div className="container mx-auto max-w-6xl">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-12 items-center">
            <div className={`space-y-6 transition-all duration-1000 ${isVisible.hero ? 'opacity-100 translate-x-0' : 'opacity-0 -translate-x-10'}`}>
              <div className="inline-block px-3 py-1 bg-orange-500/10 text-orange-500 rounded-full text-sm font-medium mb-2">
                Blockchain Safety Reimagined
              </div>
              <h1 className="text-4xl md:text-6xl font-bold leading-tight">
                <span className="bg-gradient-to-r from-orange-500 to-red-500 bg-clip-text text-transparent">
                  Phoenix Protocol
                </span>
                <br />
                <span>Smart Contract Recovery</span>
              </h1>
              <p className="text-xl text-muted-foreground">
                Interact with smart contracts in a human-readable, intent-confirmed, and rollback-safe manner, preventing common transaction mishaps.
              </p>
              <div className="flex flex-col sm:flex-row gap-4 pt-4">
                <Button variant="gradient" size="lg" asChild>
                  <Link href="/transactions" className="group">
                    <span className="flex items-center">
                      Launch App
                      <ArrowRight className="ml-2 h-4 w-4 transition-transform group-hover:translate-x-1" />
                    </span>
                  </Link>
                </Button>
                <Button variant="outline" size="lg" asChild>
                  <Link href="/docs">
                    Read Documentation
                  </Link>
                </Button>
              </div>
            </div>
            <div className={`relative h-[450px] rounded-lg overflow-hidden shadow-xl transition-all duration-1000 ${isVisible.hero ? 'opacity-100 translate-x-0' : 'opacity-0 translate-x-10'}`}>
              <div className="absolute inset-0 bg-gradient-to-br from-orange-500/20 to-red-500/20 z-10"></div>
              <Image 
                src="/hero-blockchain.jpg" 
                alt="Blockchain visualization" 
                fill 
                className="object-cover"
                priority
              />
              <div className="absolute inset-0 flex items-center justify-center z-20">
                <div className="text-center p-6 bg-background/80 backdrop-blur-sm rounded-lg shadow-lg max-w-md">
                  <h3 className="text-2xl font-bold mb-2">Transaction Protection</h3>
                  <p className="mb-4">Secure your DeFi and NFT transactions with intent verification and rollback capability</p>
                  <div className="flex justify-center">
                    <span className="px-3 py-1 bg-green-500/10 text-green-500 rounded-full text-sm font-medium">
                      Active on Avalanche & Cardano
                    </span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Stats Section */}
      <section ref={statsRef} className="py-12 px-4 border-y">
        <div className="container mx-auto max-w-6xl">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-6">
            {stats.map((stat, index) => (
              <div 
                key={stat.label} 
                className={`text-center transition-all duration-700 delay-${index * 100} ${isVisible.stats ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}
                style={{ transitionDelay: `${index * 100}ms` }}
              >
                <div className="flex justify-center mb-2">
                  {stat.icon}
                </div>
                <div className="text-3xl font-bold mb-1 text-phoenix-gradient">{stat.value}</div>
                <div className="text-sm text-muted-foreground">{stat.label}</div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section ref={featuresRef} className="py-20 px-4">
        <div className="container mx-auto max-w-6xl">
          <div className="text-center mb-16">
            <h2 className={`text-4xl font-bold mb-4 transition-all duration-700 ${isVisible.features ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}>
              Core Features
            </h2>
            <p className={`text-xl text-muted-foreground max-w-2xl mx-auto transition-all duration-700 delay-100 ${isVisible.features ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}>
              Phoenix Protocol provides a comprehensive suite of tools to protect your blockchain transactions and assets.
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {[
              {
                title: "Intent Registry",
                icon: <Shield className="h-8 w-8 text-orange-500 mb-4" />,
                description: "Store your transaction intent in an immutable registry before execution, ensuring your actions are verified against your original intentions.",
                features: ["Parameter verification", "Intent locking", "Cross-chain support"]
              },
              {
                title: "Transaction Verification",
                icon: <CheckCircle className="h-8 w-8 text-orange-500 mb-4" />,
                description: "Automatically compare executed transactions against your intended parameters to detect mismatches in token amounts, recipients, or slippage.",
                features: ["Real-time monitoring", "Slippage protection", "MEV attack prevention"]
              },
              {
                title: "Recovery Controller",
                icon: <RefreshCw className="h-8 w-8 text-orange-500 mb-4" />,
                description: "Manage transaction rollbacks, wallet recovery, and asset restoration through our secure and verified recovery process.",
                features: ["Social recovery", "Time-locked rollbacks", "Guardian verification"]
              }
            ].map((feature, index) => (
              <div 
                key={feature.title}
                className={`card-hover-effect transition-all duration-700 ${isVisible.features ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-8'}`}
                style={{ transitionDelay: `${index * 150 + 200}ms` }}
              >
                <Card className="h-full border-2 hover:border-orange-500/50 transition-colors">
                  <CardHeader>
                    {feature.icon}
                    <CardTitle className="text-xl">{feature.title}</CardTitle>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <p>{feature.description}</p>
                    <ul className="space-y-2">
                      {feature.features.map(item => (
                        <li key={item} className="flex items-center">
                          <Zap className="h-4 w-4 text-orange-500 mr-2 flex-shrink-0" />
                          <span className="text-sm">{item}</span>
                        </li>
                      ))}
                    </ul>
                  </CardContent>
                </Card>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Recent Cases Section */}
      <section ref={casesRef} className="py-20 px-4 bg-muted">
        <div className="container mx-auto max-w-6xl">
          <div className="text-center mb-16">
            <h2 className={`text-4xl font-bold mb-4 transition-all duration-700 ${isVisible.cases ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}>
              Recent Disputes & Rollbacks
            </h2>
            <p className={`text-xl text-muted-foreground max-w-2xl mx-auto transition-all duration-700 delay-100 ${isVisible.cases ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}>
              See how Phoenix Protocol has helped users recover from transaction errors and smart contract issues.
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {recentCases.map((caseItem, index) => (
              <div 
                key={caseItem.id}
                className={`transition-all duration-700 ${isVisible.cases ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-8'}`}
                style={{ transitionDelay: `${index * 150}ms` }}
              >
                <Card className="h-full card-hover-effect">
                  <CardHeader>
                    <div className="flex justify-between items-center mb-2">
                      <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                        caseItem.type === 'Dispute' ? 'bg-yellow-500/10 text-yellow-500' : 
                        caseItem.type === 'Rollback' ? 'bg-blue-500/10 text-blue-500' : 
                        'bg-green-500/10 text-green-500'
                      }`}>
                        {caseItem.type}
                      </span>
                      <span className="text-xs text-muted-foreground">{caseItem.date}</span>
                    </div>
                    <CardTitle className="text-xl">{caseItem.title}</CardTitle>
                    <CardDescription>{caseItem.description}</CardDescription>
                  </CardHeader>
                  <CardFooter>
                    <div className="flex justify-between items-center w-full">
                      <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                        caseItem.status === 'Resolved' || caseItem.status === 'Completed' 
                          ? 'bg-green-500/10 text-green-500' 
                          : 'bg-orange-500/10 text-orange-500'
                      }`}>
                        {caseItem.status}
                      </span>
                      <Button variant="ghost" size="sm" asChild className="group">
                        <Link href={`/cases/${caseItem.id}`}>
                          <span className="flex items-center">
                            View Details
                            <ArrowRight className="ml-1 h-3 w-3 transition-transform group-hover:translate-x-1" />
                          </span>
                        </Link>
                      </Button>
                    </div>
                  </CardFooter>
                </Card>
              </div>
            ))}
          </div>
          
          <div className={`mt-12 text-center transition-all duration-700 delay-500 ${isVisible.cases ? 'opacity-100 translate-y-0' : 'opacity-0 translate-y-4'}`}>
            <Button variant="outline" size="lg" asChild className="group">
              <Link href="/cases">
                <span className="flex items-center">
                  View All Cases
                  <ArrowRight className="ml-2 h-4 w-4 transition-transform group-hover:translate-x-1" />
                </span>
              </Link>
            </Button>
          </div>
        </div>
      </section>

      {/* How It Works Section */}
      <section className="py-20 px-4">
        <div className="container mx-auto max-w-6xl">
          <div className="text-center mb-16">
            <h2 className="text-4xl font-bold mb-4">How Phoenix Protocol Works</h2>
            <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
              Our protocol provides a seamless layer of protection for all your blockchain interactions.
            </p>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-12 items-center">
            <div className="space-y-8">
              {[
                {
                  number: "01",
                  title: "Connect Your Wallet",
                  description: "Securely connect your wallet to Phoenix Protocol to enable transaction protection."
                },
                {
                  number: "02",
                  title: "Set Your Intent Parameters",
                  description: "Define your transaction parameters and intent before executing any blockchain operation."
                },
                {
                  number: "03",
                  title: "Execute with Confidence",
                  description: "Perform your transactions with the knowledge that Phoenix Protocol is verifying every step."
                },
                {
                  number: "04",
                  title: "Automatic Protection",
                  description: "If any discrepancy is detected, Phoenix Protocol will alert you and provide recovery options."
                }
              ].map(step => (
                <div key={step.number} className="flex gap-4 card-hover-effect p-4 rounded-lg">
                  <div className="flex-shrink-0 w-12 h-12 rounded-full bg-orange-500/10 flex items-center justify-center text-orange-500 font-bold">
                    {step.number}
                  </div>
                  <div>
                    <h3 className="text-xl font-semibold mb-2">{step.title}</h3>
                    <p className="text-muted-foreground">{step.description}</p>
                  </div>
                </div>
              ))}
            </div>
            
            <div className="relative h-[500px] rounded-lg overflow-hidden shadow-xl">
              <div className="absolute inset-0 bg-gradient-to-br from-orange-500/20 to-red-500/20 z-10"></div>
              <Image 
                src="/workflow-diagram.jpg" 
                alt="Phoenix Protocol workflow" 
                fill 
                className="object-cover"
              />
            </div>
          </div>
        </div>
      </section>

      {/* Call to Action */}
      <section ref={ctaRef} className="py-24 px-4 bg-gradient-to-br from-orange-500/10 to-red-500/10 rounded-lg mx-4 my-12">
        <div className="container mx-auto max-w-4xl text-center">
          <div className={`transition-all duration-1000 ${isVisible.cta ? 'opacity-100 scale-100' : 'opacity-0 scale-95'}`}>
            <h2 className="text-3xl md:text-5xl font-bold mb-6">Ready to Secure Your Blockchain Transactions?</h2>
            <p className="text-xl text-muted-foreground mb-8 max-w-2xl mx-auto">
              Join Phoenix Protocol today and experience worry-free DeFi and NFT interactions with built-in recovery mechanisms.
            </p>
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Button variant="gradient" size="lg" asChild className="group">
                <Link href="/transactions">
                  <span className="flex items-center">
                    Get Started
                    <ArrowRight className="ml-2 h-5 w-5 transition-transform group-hover:translate-x-1" />
                  </span>
                </Link>
              </Button>
              <Button variant="outline" size="lg" asChild>
                <Link href="/developers">For Developers</Link>
              </Button>
            </div>
          </div>
        </div>
      </section>
    </>
  );
}
