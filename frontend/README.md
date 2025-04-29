# Phoenix Protocol Frontend

This is the frontend application for Phoenix Protocol, a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems. Built with [Next.js](https://nextjs.org).

## Features

- **Intent-Based Transactions**: Create and confirm human-readable transaction intents before execution
- **Transaction Verification**: Automatically verify executed transactions against intended parameters
- **Dispute Resolution**: Submit and track disputes for mismatched transactions
- **Social Recovery**: Set up guardians for wallet recovery in case of lost private keys
- **Multi-Wallet Support**: Connect with MetaMask, Core Wallet, Phantom, and other popular wallets

## Getting Started

### Prerequisites

- Node.js 18.x or later
- npm or yarn

### Installation

1. Clone the repository
   ```bash
   git clone https://github.com/your-username/phoenix-protocol.git
   cd phoenix-protocol/frontend
   ```

2. Install dependencies
   ```bash
   npm install
   # or
   yarn install
   ```

3. Start the development server
   ```bash
   npm run dev
   # or
   yarn dev
   ```

4. Open [http://localhost:3000](http://localhost:3000) in your browser

## Project Structure

```
frontend/
├── public/            # Static assets
├── src/
│   ├── app/           # Next.js app router pages
│   ├── components/    # React components
│   │   ├── forms/     # Form components
│   │   ├── layout/    # Layout components
│   │   └── ui/        # UI components
│   ├── context/       # React context providers
│   ├── hooks/         # Custom React hooks
│   └── lib/           # Utility functions
│       └── utils/     # Helper utilities
├── tailwind.config.js # Tailwind CSS configuration
└── next.config.js     # Next.js configuration
```

## Key Components

- **WalletProvider**: Context for wallet connection and management
- **IntentModal**: Modal for confirming transaction intents
- **TransactionPage**: Main interface for creating and managing transactions
- **DisputesPage**: Dashboard for viewing and managing disputes
- **RecoveryPage**: Interface for setting up and using social recovery

## Technologies Used

- **Next.js**: React framework for server-rendered applications
- **TypeScript**: Type-safe JavaScript
- **Tailwind CSS**: Utility-first CSS framework
- **ethers.js**: Ethereum library for wallet interactions

## Blockchain Integration

The frontend integrates with multiple blockchain networks:

- **Avalanche C-Chain**: Primary network for smart contracts (Solidity)
- **Cardano**: Secondary network for UTXO-based transactions (Plutus)

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

## Deployment

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out the [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Joseph Mwangi - Project Author
