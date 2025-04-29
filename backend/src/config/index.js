require('dotenv').config();

module.exports = {
  NODE_ENV: process.env.NODE_ENV || 'development',
  PORT: process.env.PORT || 3001,
  MONGODB_URI: process.env.MONGODB_URI || 'mongodb://localhost:27017/phoenix-protocol',
  JWT_SECRET: process.env.JWT_SECRET || 'your-secret-key',
  JWT_EXPIRATION: process.env.JWT_EXPIRATION || '1d',
  CORS_ORIGIN: process.env.CORS_ORIGIN || 'http://localhost:3000',
  LOG_LEVEL: process.env.LOG_LEVEL || 'info',
  BLOCKCHAIN_PROVIDER_URL: process.env.BLOCKCHAIN_PROVIDER_URL || 'https://api.avax-test.network/ext/bc/C/rpc',
  CHAIN_ID: process.env.CHAIN_ID || '43113', // Avalanche Fuji Testnet
};