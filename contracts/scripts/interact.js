// Interaction script for Phoenix Protocol contracts
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  console.log("Phoenix Protocol Contract Interaction");

  // Get contract addresses from .env
  const phoenixProtocolAddress = process.env.PHOENIX_PROTOCOL_ADDRESS;
  const intentRegistryAddress = process.env.INTENT_REGISTRY_ADDRESS;
  const recoveryModuleAddress = process.env.RECOVERY_MODULE_ADDRESS;
  const tokenVaultAddress = process.env.TOKEN_VAULT_ADDRESS;

  if (!phoenixProtocolAddress || !intentRegistryAddress || !recoveryModuleAddress || !tokenVaultAddress) {
    throw new Error("Contract addresses not set in .env file");
  }

  // Get the signer
  const [signer] = await hre.ethers.getSigners();
  console.log(`Using account: ${signer.address}`);

  // Get contract instances
  const phoenixProtocol = await hre.ethers.getContractAt("PhoenixProtocol", phoenixProtocolAddress);
  const intentRegistry = await hre.ethers.getContractAt("IntentRegistry", intentRegistryAddress);
  const recoveryModule = await hre.ethers.getContractAt("RecoveryModule", recoveryModuleAddress);
  const tokenVault = await hre.ethers.getContractAt("TokenVault", tokenVaultAddress);

  // Display contract information
  console.log("\nContract Information:");
  console.log(`- PhoenixProtocol: ${phoenixProtocolAddress}`);
  console.log(`- IntentRegistry: ${intentRegistryAddress}`);
  console.log(`- RecoveryModule: ${recoveryModuleAddress}`);
  console.log(`- TokenVault: ${tokenVaultAddress}`);

  // Display protocol configuration
  const protocolFeePercentage = await phoenixProtocol.protocolFeePercentage();
  const feeCollector = await phoenixProtocol.feeCollector();
  console.log("\nProtocol Configuration:");
  console.log(`- Protocol Fee: ${protocolFeePercentage / 100}%`);
  console.log(`- Fee Collector: ${feeCollector}`);

  // Display recovery module configuration
  const recoveryFeePercentage = await recoveryModule.recoveryFeePercentage();
  const recoveryCooldown = await recoveryModule.recoveryCooldown();
  console.log("\nRecovery Module Configuration:");
  console.log(`- Recovery Fee: ${recoveryFeePercentage / 100}%`);
  console.log(`- Recovery Cooldown: ${recoveryCooldown / 3600} hours`);

  // Check if the contracts are properly connected
  const registeredIntentRegistry = await phoenixProtocol.intentRegistry();
  const registeredRecoveryModule = await phoenixProtocol.recoveryModule();
  const registeredTokenVault = await phoenixProtocol.tokenVault();

  console.log("\nContract Connections:");
  console.log(`- PhoenixProtocol -> IntentRegistry: ${registeredIntentRegistry === intentRegistryAddress ? "✓" : "✗"}`);
  console.log(`- PhoenixProtocol -> RecoveryModule: ${registeredRecoveryModule === recoveryModuleAddress ? "✓" : "✗"}`);
  console.log(`- PhoenixProtocol -> TokenVault: ${registeredTokenVault === tokenVaultAddress ? "✓" : "✗"}`);

  // Check native token balance in TokenVault
  const nativeBalance = await hre.ethers.provider.getBalance(tokenVaultAddress);
  console.log("\nToken Vault Balances:");
  console.log(`- Native Token: ${hre.ethers.formatEther(nativeBalance)} ETH/AVAX`);

  console.log("\nInteraction completed!");
}

// Execute the interaction
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });