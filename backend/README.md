# Phoenix Protocol Backend API

This is the backend API for Phoenix Protocol, a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems.

## Features

- User authentication with wallet signatures
- Transaction intent creation and verification
- Transaction recovery system
- Two-factor authentication
- User preferences and settings

## Getting Started

### Prerequisites

- Node.js (v18 or higher)
- MongoDB
- npm or yarn

### Installation

1. Clone the repository
2. Navigate to the backend directory
3. Install dependencies

```bash
cd backend
npm install
```

4. Create a `.env` file based on `.env.example`
5. Start the development server

```bash
npm run dev
```

## API Endpoints

### Authentication

- `POST /api/auth/nonce` - Get a nonce for wallet signature
- `POST /api/auth/wallet` - Authenticate with wallet signature
- `POST /api/auth/login` - Login with email and password
- `POST /api/auth/register` - Register a new user
- `POST /api/auth/logout` - Logout user
- `POST /api/auth/refresh` - Refresh access token
- `POST /api/auth/2fa/setup` - Setup two-factor authentication
- `POST /api/auth/2fa/verify` - Verify two-factor authentication code
- `POST /api/auth/2fa/disable` - Disable two-factor authentication

### Users

- `GET /api/users/me` - Get current user profile
- `PUT /api/users/me` - Update current user profile
- `PUT /api/users/me/preferences` - Update user preferences
- `PUT /api/users/me/password` - Update user password
- `PUT /api/users/me/email` - Update user email
- `GET /api/users` - Get all users (admin only)
- `GET /api/users/:id` - Get user by ID (admin only)
- `PUT /api/users/:id` - Update user by ID (admin only)
- `DELETE /api/users/:id` - Delete user by ID (admin only)

### Transactions

- `GET /api/transactions` - Get user transactions
- `GET /api/transactions/:id` - Get transaction by ID
- `POST /api/transactions` - Create a new transaction record
- `PUT /api/transactions/:id` - Update transaction status
- `GET /api/transactions/address/:address` - Get transactions by wallet address
- `GET /api/transactions/hash/:hash` - Get transaction by hash
- `POST /api/transactions/verify` - Verify transaction against intent

### Intents

- `GET /api/intents` - Get user intents
- `GET /api/intents/:id` - Get intent by ID
- `POST /api/intents` - Create a new intent
- `PUT /api/intents/:id` - Update intent status
- `DELETE /api/intents/:id` - Cancel an intent
- `GET /api/intents/address/:address` - Get intents by wallet address
- `POST /api/intents/:id/execute` - Execute an intent
- `POST /api/intents/:id/sign` - Sign an intent

### Recovery

- `POST /api/recovery/request` - Request transaction recovery
- `GET /api/recovery/requests` - Get user recovery requests
- `GET /api/recovery/requests/:id` - Get recovery request by ID
- `PUT /api/recovery/requests/:id` - Update recovery request status
- `POST /api/recovery/simulate` - Simulate transaction recovery
- `POST /api/recovery/execute/:id` - Execute transaction recovery

## Architecture

The backend follows a modular architecture:

- `controllers/` - Request handlers
- `models/` - Database models
- `routes/` - API routes
- `middleware/` - Express middleware
- `config/` - Configuration files
- `utils/` - Utility functions
- `services/` - Business logic

## Security

- JWT authentication
- Two-factor authentication
- Rate limiting
- Helmet security headers
- Input validation
- Error handling

## License

This project is licensed under the MIT License - see the LICENSE file for details.