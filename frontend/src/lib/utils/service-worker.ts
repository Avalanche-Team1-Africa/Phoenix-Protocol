// Extend the ServiceWorkerRegistration interface to include the sync property
interface SyncManager {
  register(tag: string): Promise<void>;
}

interface ExtendedServiceWorkerRegistration extends ServiceWorkerRegistration {
  sync?: SyncManager;
}

export function registerServiceWorker() {
  if (typeof window !== 'undefined' && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
      navigator.serviceWorker
        .register('/sw.js')
        .then(registration => {
          console.log('Service Worker registered: ', registration);
        })
        .catch(registrationError => {
          console.log('Service Worker registration failed: ', registrationError);
        });
    });
  }
}

export function unregisterServiceWorker() {
  if (typeof window !== 'undefined' && 'serviceWorker' in navigator) {
    navigator.serviceWorker.ready
      .then(registration => {
        registration.unregister();
      })
      .catch(error => {
        console.error(error.message);
      });
  }
}

// Check if app can work offline
export function checkOfflineCapability(): boolean {
  if (typeof window !== 'undefined' && 'serviceWorker' in navigator) {
    return true;
  }
  return false;
}

// Request background sync for transactions
export async function requestBackgroundSync() {
  if (typeof window !== 'undefined' && 'serviceWorker' in navigator && 'SyncManager' in window) {
    try {
      const registration = await navigator.serviceWorker.ready as ExtendedServiceWorkerRegistration;
      if (registration.sync) {
        await registration.sync.register('sync-transactions');
        return true;
      } else {
        console.warn('Sync API is not available in this browser');
        return false;
      }
    } catch (error) {
      console.error('Background sync could not be registered:', error);
      return false;
    }
  }
  return false;
}