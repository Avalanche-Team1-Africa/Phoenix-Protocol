// Deployment script for Phoenix Protocol contracts
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  console.log("Deploying Phoenix Protocol contracts...");

  // Get the deployer account
  const [deployer] = await hre.ethers.getSigners();
  console.log(`Deploying contracts with the account: ${deployer.address}`);

  // Get the admin wallet address from .env
  const adminWalletAddress = process.env.ADMIN_WALLET_ADDRESS;
  if (!adminWalletAddress) {
    throw new Error("ADMIN_WALLET_ADDRESS not set in .env file");
  }
  console.log(`Admin wallet address: ${adminWalletAddress}`);

  // Deploy IntentRegistry
  console.log("Deploying IntentRegistry...");
  const IntentRegistry = await hre.ethers.getContractFactory("IntentRegistry");
  const intentRegistry = await IntentRegistry.deploy(adminWalletAddress, deployer.address);
  await intentRegistry.waitForDeployment();
  const intentRegistryAddress = await intentRegistry.getAddress();
  console.log(`IntentRegistry deployed to: ${intentRegistryAddress}`);

  // Deploy TokenVault
  console.log("Deploying TokenVault...");
  const TokenVault = await hre.ethers.getContractFactory("TokenVault");
  const tokenVault = await TokenVault.deploy(adminWalletAddress, deployer.address);
  await tokenVault.waitForDeployment();
  const tokenVaultAddress = await tokenVault.getAddress();
  console.log(`TokenVault deployed to: ${tokenVaultAddress}`);

  // Deploy RecoveryModule
  console.log("Deploying RecoveryModule...");
  const recoveryFeePercentage = 500; // 5%
  const recoveryCooldown = 86400; // 24 hours in seconds
  const RecoveryModule = await hre.ethers.getContractFactory("RecoveryModule");
  const recoveryModule = await RecoveryModule.deploy(
    adminWalletAddress,
    adminWalletAddress, // Fee collector is the admin
    recoveryFeePercentage,
    recoveryCooldown
  );
  await recoveryModule.waitForDeployment();
  const recoveryModuleAddress = await recoveryModule.getAddress();
  console.log(`RecoveryModule deployed to: ${recoveryModuleAddress}`);

  // Deploy PhoenixProtocol
  console.log("Deploying PhoenixProtocol...");
  const protocolFeePercentage = 100; // 1%
  const PhoenixProtocol = await hre.ethers.getContractFactory("PhoenixProtocol");
  const phoenixProtocol = await PhoenixProtocol.deploy(
    adminWalletAddress,
    adminWalletAddress, // Fee collector is the admin
    protocolFeePercentage
  );
  await phoenixProtocol.waitForDeployment();
  const phoenixProtocolAddress = await phoenixProtocol.getAddress();
  console.log(`PhoenixProtocol deployed to: ${phoenixProtocolAddress}`);

  // Set up contract connections
  console.log("Setting up contract connections...");

  // Set up PhoenixProtocol
  console.log("Setting up PhoenixProtocol connections...");
  await phoenixProtocol.setIntentRegistry(intentRegistryAddress);
  await phoenixProtocol.setRecoveryModule(recoveryModuleAddress);
  await phoenixProtocol.setTokenVault(tokenVaultAddress);

  // Set up RecoveryModule
  console.log("Setting up RecoveryModule connections...");
  await recoveryModule.setTokenVault(tokenVaultAddress);

  // Grant roles
  console.log("Granting roles...");
  
  // Grant PROTOCOL_ROLE to PhoenixProtocol in TokenVault
  const PROTOCOL_ROLE = hre.ethers.keccak256(hre.ethers.toUtf8Bytes("PROTOCOL_ROLE"));
  await tokenVault.grantRole(PROTOCOL_ROLE, phoenixProtocolAddress);
  
  // Grant EXECUTOR_ROLE to PhoenixProtocol in IntentRegistry
  const EXECUTOR_ROLE = hre.ethers.keccak256(hre.ethers.toUtf8Bytes("EXECUTOR_ROLE"));
  await intentRegistry.grantRole(EXECUTOR_ROLE, phoenixProtocolAddress);
  
  // Grant PROTOCOL_ROLE to PhoenixProtocol in RecoveryModule
  await recoveryModule.grantRole(PROTOCOL_ROLE, phoenixProtocolAddress);

  console.log("Deployment completed successfully!");
  console.log("Contract addresses:");
  console.log(`- PhoenixProtocol: ${phoenixProtocolAddress}`);
  console.log(`- IntentRegistry: ${intentRegistryAddress}`);
  console.log(`- RecoveryModule: ${recoveryModuleAddress}`);
  console.log(`- TokenVault: ${tokenVaultAddress}`);

  // Update .env file with contract addresses
  console.log("Don't forget to update your .env file with the following values:");
  console.log(`PHOENIX_PROTOCOL_ADDRESS=${phoenixProtocolAddress}`);
  console.log(`INTENT_REGISTRY_ADDRESS=${intentRegistryAddress}`);
  console.log(`RECOVERY_MODULE_ADDRESS=${recoveryModuleAddress}`);
  console.log(`TOKEN_VAULT_ADDRESS=${tokenVaultAddress}`);
}

// Execute the deployment
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });