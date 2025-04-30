/**
 * Script to update contract addresses in all .env files
 * 
 * Usage: node scripts/update-addresses.js <phoenix-protocol-address> <intent-registry-address> <recovery-module-address> <token-vault-address>
 */

const fs = require('fs');
const path = require('path');

// Get contract addresses from command line arguments
const [, , phoenixProtocolAddress, intentRegistryAddress, recoveryModuleAddress, tokenVaultAddress] = process.argv;

// Validate arguments
if (!phoenixProtocolAddress || !intentRegistryAddress || !recoveryModuleAddress || !tokenVaultAddress) {
  console.error('Missing contract addresses. Usage:');
  console.error('node scripts/update-addresses.js <phoenix-protocol-address> <intent-registry-address> <recovery-module-address> <token-vault-address>');
  process.exit(1);
}

// Define paths to .env files
const rootEnvPath = path.join(__dirname, '..', '.env');
const frontendEnvPath = path.join(__dirname, '..', 'frontend', '.env');
const backendEnvPath = path.join(__dirname, '..', 'backend', '.env');

// Function to update .env file
const updateEnvFile = (filePath, prefix = '') => {
  try {
    if (!fs.existsSync(filePath)) {
      console.error(`File not found: ${filePath}`);
      return false;
    }

    let content = fs.readFileSync(filePath, 'utf8');

    // Update contract addresses
    content = content.replace(new RegExp(`${prefix}PHOENIX_PROTOCOL_ADDRESS=.*`, 'g'), `${prefix}PHOENIX_PROTOCOL_ADDRESS=${phoenixProtocolAddress}`);
    content = content.replace(new RegExp(`${prefix}INTENT_REGISTRY_ADDRESS=.*`, 'g'), `${prefix}INTENT_REGISTRY_ADDRESS=${intentRegistryAddress}`);
    content = content.replace(new RegExp(`${prefix}RECOVERY_MODULE_ADDRESS=.*`, 'g'), `${prefix}RECOVERY_MODULE_ADDRESS=${recoveryModuleAddress}`);
    content = content.replace(new RegExp(`${prefix}TOKEN_VAULT_ADDRESS=.*`, 'g'), `${prefix}TOKEN_VAULT_ADDRESS=${tokenVaultAddress}`);

    // Write updated content back to file
    fs.writeFileSync(filePath, content, 'utf8');
    console.log(`Updated ${filePath}`);
    return true;
  } catch (error) {
    console.error(`Error updating ${filePath}:`, error.message);
    return false;
  }
};

// Update all .env files
console.log('Updating contract addresses in .env files...');
updateEnvFile(rootEnvPath);
updateEnvFile(frontendEnvPath, 'REACT_APP_');
updateEnvFile(backendEnvPath);

console.log('\nContract addresses updated successfully!');
console.log('Phoenix Protocol:', phoenixProtocolAddress);
console.log('Intent Registry:', intentRegistryAddress);
console.log('Recovery Module:', recoveryModuleAddress);
console.log('Token Vault:', tokenVaultAddress);