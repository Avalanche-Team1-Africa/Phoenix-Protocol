import axios from 'axios';

const API_URL = process.env.REACT_APP_API_URL || 'http://localhost:3001/api';

// Create axios instance
const api = axios.create({
  baseURL: API_URL,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Add request interceptor to add auth token
api.interceptors.request.use(
  (config) => {
    const token = localStorage.getItem('token');
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => Promise.reject(error)
);

// Auth services
const authService = {
  login: async (walletAddress, signature) => {
    const response = await api.post('/auth/login', { walletAddress, signature });
    return response.data;
  },
  register: async (walletAddress, signature) => {
    const response = await api.post('/auth/register', { walletAddress, signature });
    return response.data;
  },
  getProfile: async () => {
    const response = await api.get('/auth/profile');
    return response.data;
  },
  updateProfile: async (data) => {
    const response = await api.put('/auth/profile', data);
    return response.data;
  },
};

// Intent services
const intentService = {
  getUserIntents: async (page = 1, limit = 10, status = null, type = null) => {
    const response = await api.get('/intents', {
      params: { page, limit, status, type },
    });
    return response.data;
  },
  getIntentById: async (id) => {
    const response = await api.get(`/intents/${id}`);
    return response.data;
  },
  createIntent: async (data) => {
    const response = await api.post('/intents', data);
    return response.data;
  },
  updateIntent: async (id, status) => {
    const response = await api.put(`/intents/${id}`, { status });
    return response.data;
  },
  cancelIntent: async (id) => {
    const response = await api.post(`/intents/${id}/cancel`);
    return response.data;
  },
  executeIntent: async (id, transactionHash) => {
    const response = await api.post(`/intents/${id}/execute`, { transactionHash });
    return response.data;
  },
  signIntent: async (id, signature) => {
    const response = await api.post(`/intents/${id}/sign`, { signature });
    return response.data;
  },
};

// Transaction services
const transactionService = {
  getUserTransactions: async (page = 1, limit = 10) => {
    const response = await api.get('/transactions', {
      params: { page, limit },
    });
    return response.data;
  },
  getTransactionById: async (id) => {
    const response = await api.get(`/transactions/${id}`);
    return response.data;
  },
  createTransaction: async (data) => {
    const response = await api.post('/transactions', data);
    return response.data;
  },
};

// Recovery services
const recoveryService = {
  getUserRecoveries: async (page = 1, limit = 10) => {
    const response = await api.get('/recoveries', {
      params: { page, limit },
    });
    return response.data;
  },
  getRecoveryById: async (id) => {
    const response = await api.get(`/recoveries/${id}`);
    return response.data;
  },
  requestRecovery: async (data) => {
    const response = await api.post('/recoveries', data);
    return response.data;
  },
  updateRecovery: async (id, status) => {
    const response = await api.put(`/recoveries/${id}`, { status });
    return response.data;
  },
};

// Export all services
export {
  api,
  authService,
  intentService,
  transactionService,
  recoveryService,
};