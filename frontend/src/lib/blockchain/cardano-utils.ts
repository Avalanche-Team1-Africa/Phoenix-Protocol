/**
 * Utility functions for Cardano blockchain interactions
 */

import { CHAIN_IDS } from './providers';

// Define types for Cardano transactions
export interface CardanoTxInput {
  txHash: string;
  index: number;
  amount: {
    lovelace: string;
    assets?: Record<string, string>;
  };
}

export interface CardanoTxOutput {
  address: string;
  amount: {
    lovelace: string;
    assets?: Record<string, string>;
  };
}

export interface CardanoTxMetadata {
  [key: string]: any;
}

export interface CardanoTx {
  inputs: CardanoTxInput[];
  outputs: CardanoTxOutput[];
  fee: string;
  ttl?: number;
  metadata?: CardanoTxMetadata;
}

/**
 * Create a transaction for registering an intent
 */
export async function createRegisterIntentTx(
  walletApi: any,
  intentRegistryAddress: string,
  action: string,
  parameters: string,
  expiryTimestamp: number
): Promise<string> {
  try {
    // Get wallet UTXOs
    const utxos = await walletApi.getUtxos();
    
    // Get change address
    const changeAddress = await walletApi.getChangeAddress();
    
    // Create metadata for the intent
    const metadata = {
      674: { // Custom metadata label for Phoenix Protocol
        action,
        parameters,
        expiry: expiryTimestamp.toString(),
        type: 'register-intent'
      }
    };
    
    // Build transaction
    const txHash = await walletApi.submitTx({
      tx: {
        inputs: utxos.slice(0, 1), // Use first UTXO
        outputs: [
          {
            address: intentRegistryAddress,
            amount: {
              lovelace: '2000000' // 2 ADA as collateral
            }
          },
          {
            address: changeAddress,
            amount: {
              lovelace: '0' // Will be calculated by the wallet
            }
          }
        ],
        metadata
      }
    });
    
    return txHash;
  } catch (error) {
    console.error('Error creating register intent transaction:', error);
    throw new Error('Failed to create register intent transaction');
  }
}

/**
 * Create a transaction for executing an intent
 */
export async function createExecuteIntentTx(
  walletApi: any,
  intentRegistryAddress: string,
  intentId: string
): Promise<string> {
  try {
    // Get wallet UTXOs
    const utxos = await walletApi.getUtxos();
    
    // Get change address
    const changeAddress = await walletApi.getChangeAddress();
    
    // Create metadata for executing the intent
    const metadata = {
      674: { // Custom metadata label for Phoenix Protocol
        intentId,
        type: 'execute-intent'
      }
    };
    
    // Build transaction
    const txHash = await walletApi.submitTx({
      tx: {
        inputs: utxos.slice(0, 1), // Use first UTXO
        outputs: [
          {
            address: intentRegistryAddress,
            amount: {
              lovelace: '2000000' // 2 ADA as collateral
            }
          },
          {
            address: changeAddress,
            amount: {
              lovelace: '0' // Will be calculated by the wallet
            }
          }
        ],
        metadata
      }
    });
    
    return txHash;
  } catch (error) {
    console.error('Error creating execute intent transaction:', error);
    throw new Error('Failed to create execute intent transaction');
  }
}

/**
 * Create a transaction for requesting recovery
 */
export async function createRequestRecoveryTx(
  walletApi: any,
  recoveryModuleAddress: string,
  transactionId: string,
  reason: string,
  amount: string,
  token: string
): Promise<string> {
  try {
    // Get wallet UTXOs
    const utxos = await walletApi.getUtxos();
    
    // Get change address
    const changeAddress = await walletApi.getChangeAddress();
    
    // Create metadata for the recovery request
    const metadata = {
      674: { // Custom metadata label for Phoenix Protocol
        transactionId,
        reason,
        amount,
        token,
        type: 'request-recovery'
      }
    };
    
    // Build transaction
    const txHash = await walletApi.submitTx({
      tx: {
        inputs: utxos.slice(0, 1), // Use first UTXO
        outputs: [
          {
            address: recoveryModuleAddress,
            amount: {
              lovelace: '2000000' // 2 ADA as collateral
            }
          },
          {
            address: changeAddress,
            amount: {
              lovelace: '0' // Will be calculated by the wallet
            }
          }
        ],
        metadata
      }
    });
    
    return txHash;
  } catch (error) {
    console.error('Error creating request recovery transaction:', error);
    throw new Error('Failed to create request recovery transaction');
  }
}

/**
 * Create a transaction for approving a recovery request
 */
export async function createApproveRecoveryTx(
  walletApi: any,
  recoveryModuleAddress: string,
  recoveryId: string
): Promise<string> {
  try {
    // Get wallet UTXOs
    const utxos = await walletApi.getUtxos();
    
    // Get change address
    const changeAddress = await walletApi.getChangeAddress();
    
    // Create metadata for approving the recovery
    const metadata = {
      674: { // Custom metadata label for Phoenix Protocol
        recoveryId,
        type: 'approve-recovery'
      }
    };
    
    // Build transaction
    const txHash = await walletApi.submitTx({
      tx: {
        inputs: utxos.slice(0, 1), // Use first UTXO
        outputs: [
          {
            address: recoveryModuleAddress,
            amount: {
              lovelace: '2000000' // 2 ADA as collateral
            }
          },
          {
            address: changeAddress,
            amount: {
              lovelace: '0' // Will be calculated by the wallet
            }
          }
        ],
        metadata
      }
    });
    
    return txHash;
  } catch (error) {
    console.error('Error creating approve recovery transaction:', error);
    throw new Error('Failed to create approve recovery transaction');
  }
}

/**
 * Get contract addresses for the current network
 */
export function getCardanoContractAddresses(networkId: number): { intentRegistry: string; recoveryModule: string } {
  // In a real app, these would be loaded from environment variables or a config file
  if (networkId === CHAIN_IDS.CARDANO_MAINNET) {
    return {
      intentRegistry: 'addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx',
      recoveryModule: 'addr1w9lzn5ygc6qvlsz4n0tzmw8kv4ym6yssvjyqx0f3qfln6tcyjy7wx'
    };
  } else {
    // Testnet addresses
    return {
      intentRegistry: 'addr_test1wphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx',
      recoveryModule: 'addr_test1w9lzn5ygc6qvlsz4n0tzmw8kv4ym6yssvjyqx0f3qfln6tcyjy7wx'
    };
  }
}