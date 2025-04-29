import React from "react";

export default function TermsOfService() {
  return (
    <div className="container py-12 max-w-4xl">
      <h1 className="text-4xl font-bold mb-8">Terms of Service</h1>
      
      <div className="prose prose-lg dark:prose-invert">
        <p className="text-lg mb-6">
          Last Updated: {new Date().toLocaleDateString()}
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">1. Acceptance of Terms</h2>
        <p>
          By accessing or using Phoenix Protocol's website, services, or applications, you agree to be bound by these Terms of Service. 
          If you do not agree to all the terms and conditions, then you may not access the service.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">2. Description of Service</h2>
        <p>
          Phoenix Protocol is a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems. 
          Our service enables secure, verified rollback of smart contract operations. The service includes, but is not limited to:
        </p>
        <ul className="list-disc pl-6 my-4 space-y-2">
          <li>Transaction recovery mechanisms</li>
          <li>Dispute resolution services</li>
          <li>Wallet recovery options</li>
          <li>Smart contract interaction protection</li>
        </ul>

        <h2 className="text-2xl font-semibold mt-8 mb-4">3. User Accounts</h2>
        <p>
          To use certain features of the service, you may be required to connect a blockchain wallet. You are responsible for 
          safeguarding your wallet and for all activities that occur through your wallet connection.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">4. User Conduct</h2>
        <p>
          You agree not to use the service to:
        </p>
        <ul className="list-disc pl-6 my-4 space-y-2">
          <li>Violate any applicable laws or regulations.</li>
          <li>Infringe the rights of others.</li>
          <li>Attempt to interfere with or disrupt the service or servers.</li>
          <li>Attempt to gain unauthorized access to any part of the service.</li>
          <li>Use the service for any illegal or unauthorized purpose.</li>
        </ul>

        <h2 className="text-2xl font-semibold mt-8 mb-4">5. Intellectual Property</h2>
        <p>
          The service and its original content, features, and functionality are and will remain the exclusive property of 
          Phoenix Protocol and its licensors. The service is protected by copyright, trademark, and other laws.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">6. Disclaimer of Warranties</h2>
        <p>
          The service is provided on an "AS IS" and "AS AVAILABLE" basis. Phoenix Protocol expressly disclaims all warranties 
          of any kind, whether express or implied, including but not limited to the implied warranties of merchantability, 
          fitness for a particular purpose, and non-infringement.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">7. Limitation of Liability</h2>
        <p>
          In no event shall Phoenix Protocol, nor its directors, employees, partners, agents, suppliers, or affiliates, 
          be liable for any indirect, incidental, special, consequential or punitive damages, including without limitation, 
          loss of profits, data, use, goodwill, or other intangible losses, resulting from your access to or use of or 
          inability to access or use the service.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">8. Blockchain Risks</h2>
        <p>
          You understand that blockchain technology and cryptocurrencies involve risks, including but not limited to:
        </p>
        <ul className="list-disc pl-6 my-4 space-y-2">
          <li>Volatility in cryptocurrency prices</li>
          <li>Potential for smart contract vulnerabilities</li>
          <li>Regulatory uncertainty</li>
          <li>Network congestion and high transaction fees</li>
          <li>Risk of loss due to loss of private keys</li>
        </ul>
        <p>
          Phoenix Protocol does not guarantee protection against all risks associated with blockchain technology.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">9. Modifications to Terms</h2>
        <p>
          Phoenix Protocol reserves the right, at its sole discretion, to modify or replace these Terms at any time. 
          If a revision is material, we will provide at least 30 days' notice prior to any new terms taking effect.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">10. Governing Law</h2>
        <p>
          These Terms shall be governed by and construed in accordance with the laws of the jurisdiction in which 
          Phoenix Protocol is established, without regard to its conflict of law provisions.
        </p>

        <h2 className="text-2xl font-semibold mt-8 mb-4">11. Contact Us</h2>
        <p>
          If you have any questions about these Terms, please contact us at:
        </p>
        <p className="mt-2">
          <strong>Email:</strong> legal@phoenixprotocol.com
        </p>
      </div>
    </div>
  );
}