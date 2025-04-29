// Verification script for Phoenix Protocol contracts
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  console.log("Verifying Phoenix Protocol contracts...");

  // Get contract addresses from .env
  const phoenixProtocolAddress = process.env.PHOENIX_PROTOCOL_ADDRESS;
  const intentRegistryAddress = process.env.INTENT_REGISTRY_ADDRESS;
  const recoveryModuleAddress = process.env.RECOVERY_MODULE_ADDRESS;
  const tokenVaultAddress = process.env.TOKEN_VAULT_ADDRESS;
  const adminWalletAddress = process.env.ADMIN_WALLET_ADDRESS;

  if (!phoenixProtocolAddress || !intentRegistryAddress || !recoveryModuleAddress || !tokenVaultAddress || !adminWalletAddress) {
    throw new Error("Contract addresses or admin wallet address not set in .env file");
  }

  console.log("Contract addresses:");
  console.log(`- PhoenixProtocol: ${phoenixProtocolAddress}`);
  console.log(`- IntentRegistry: ${intentRegistryAddress}`);
  console.log(`- RecoveryModule: ${recoveryModuleAddress}`);
  console.log(`- TokenVault: ${tokenVaultAddress}`);
  console.log(`- Admin Wallet: ${adminWalletAddress}`);

  // Verify PhoenixProtocol
  console.log("Verifying PhoenixProtocol...");
  try {
    await hre.run("verify:verify", {
      address: phoenixProtocolAddress,
      constructorArguments: [
        adminWalletAddress,
        adminWalletAddress, // Fee collector is the admin
        100 // 1% protocol fee
      ],
    });
    console.log("PhoenixProtocol verified successfully!");
  } catch (error) {
    console.error("Error verifying PhoenixProtocol:", error.message);
  }

  // Verify IntentRegistry
  console.log("Verifying IntentRegistry...");
  try {
    await hre.run("verify:verify", {
      address: intentRegistryAddress,
      constructorArguments: [
        adminWalletAddress,
        phoenixProtocolAddress
      ],
    });
    console.log("IntentRegistry verified successfully!");
  } catch (error) {
    console.error("Error verifying IntentRegistry:", error.message);
  }

  // Verify RecoveryModule
  console.log("Verifying RecoveryModule...");
  try {
    await hre.run("verify:verify", {
      address: recoveryModuleAddress,
      constructorArguments: [
        adminWalletAddress,
        adminWalletAddress, // Fee collector is the admin
        500, // 5% recovery fee
        86400 // 24 hours cooldown
      ],
    });
    console.log("RecoveryModule verified successfully!");
  } catch (error) {
    console.error("Error verifying RecoveryModule:", error.message);
  }

  // Verify TokenVault
  console.log("Verifying TokenVault...");
  try {
    await hre.run("verify:verify", {
      address: tokenVaultAddress,
      constructorArguments: [
        adminWalletAddress,
        phoenixProtocolAddress
      ],
    });
    console.log("TokenVault verified successfully!");
  } catch (error) {
    console.error("Error verifying TokenVault:", error.message);
  }

  console.log("Verification process completed!");
}

// Execute the verification
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });