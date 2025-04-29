// Service Worker for Phoenix Protocol
const CACHE_NAME = 'phoenix-protocol-cache-v1';
const urlsToCache = [
  '/',
  '/index.html',
  '/PhoenixProtocol.png',
  '/hero-blockchain.jpg',
  '/manifest.json',
  '/favicon.ico'
];

// Install event - cache assets
self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => {
        console.log('Opened cache');
        return cache.addAll(urlsToCache);
      })
  );
});

// Activate event - clean up old caches
self.addEventListener('activate', event => {
  const cacheWhitelist = [CACHE_NAME];
  event.waitUntil(
    caches.keys().then(cacheNames => {
      return Promise.all(
        cacheNames.map(cacheName => {
          if (cacheWhitelist.indexOf(cacheName) === -1) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});

// Fetch event - serve from cache, fall back to network
self.addEventListener('fetch', event => {
  event.respondWith(
    caches.match(event.request)
      .then(response => {
        // Cache hit - return response
        if (response) {
          return response;
        }
        
        // Clone the request
        const fetchRequest = event.request.clone();
        
        return fetch(fetchRequest).then(
          response => {
            // Check if valid response
            if (!response || response.status !== 200 || response.type !== 'basic') {
              return response;
            }
            
            // Clone the response
            const responseToCache = response.clone();
            
            // Don't cache API calls or dynamic content
            if (!event.request.url.includes('/api/')) {
              caches.open(CACHE_NAME)
                .then(cache => {
                  cache.put(event.request, responseToCache);
                });
            }
            
            return response;
          }
        );
      })
  );
});

// Handle offline fallback
self.addEventListener('fetch', event => {
  // Skip cross-origin requests
  if (event.request.mode === 'navigate') {
    event.respondWith(
      fetch(event.request)
        .catch(() => {
          return caches.match('/offline.html');
        })
    );
  }
});

// Background sync for offline transactions
self.addEventListener('sync', event => {
  if (event.tag === 'sync-transactions') {
    event.waitUntil(syncTransactions());
  }
});

// Function to sync transactions when back online
async function syncTransactions() {
  try {
    const pendingTransactions = await getPendingTransactions();
    
    for (const tx of pendingTransactions) {
      await sendTransaction(tx);
      await markTransactionSynced(tx.id);
    }
    
    return true;
  } catch (error) {
    console.error('Error syncing transactions:', error);
    return false;
  }
}

// Mock functions for transaction handling
async function getPendingTransactions() {
  // In a real app, this would get pending transactions from IndexedDB
  return [];
}

async function sendTransaction(tx) {
  // In a real app, this would send the transaction to the server/blockchain
  console.log('Sending transaction:', tx);
  return true;
}

async function markTransactionSynced(txId) {
  // In a real app, this would update the transaction status in IndexedDB
  console.log('Transaction synced:', txId);
  return true;
}