// Script to approve and set details for a recovery request in the Phoenix Protocol
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  // Get command line arguments
  const args = process.argv.slice(2);
  if (args.length < 4) {
    console.log("Usage: npx hardhat run scripts/approve-recovery.js --network <network> <recovery-id> <token-address> <amount> <recipient>");
    console.log("Example: npx hardhat run scripts/approve-recovery.js --network fuji 0x1234... 0x0000000000000000000000000000000000000000 0.1 0x5678...");
    process.exit(1);
  }

  const recoveryId = args[0];
  const tokenAddress = args[1];
  const amount = args[2];
  const recipient = args[3];

  // Get contract addresses from .env
  const phoenixProtocolAddress = process.env.PHOENIX_PROTOCOL_ADDRESS;
  const recoveryModuleAddress = process.env.RECOVERY_MODULE_ADDRESS;
  
  if (!phoenixProtocolAddress || !recoveryModuleAddress) {
    throw new Error("Contract addresses not set in .env file");
  }

  // Get the signer
  const [signer] = await hre.ethers.getSigners();
  console.log(`Using account: ${signer.address}`);

  // Get contract instances
  const phoenixProtocol = await hre.ethers.getContractAt("PhoenixProtocol", phoenixProtocolAddress);
  const recoveryModule = await hre.ethers.getContractAt("RecoveryModule", recoveryModuleAddress);

  // Get recovery details
  console.log(`\nFetching recovery details for ID: ${recoveryId}`);
  const recoveryRequest = await recoveryModule.getRecoveryRequest(recoveryId);
  
  if (recoveryRequest.requester === "0x0000000000000000000000000000000000000000") {
    console.error("Recovery request not found");
    process.exit(1);
  }

  console.log("\nRecovery Request Details:");
  console.log(`- Requester: ${recoveryRequest.requester}`);
  console.log(`- Transaction ID: ${recoveryRequest.transactionId}`);
  console.log(`- Status: ${["Requested", "Approved", "Rejected", "Completed"][recoveryRequest.status]}`);
  console.log(`- Reason: ${recoveryRequest.reason}`);
  console.log(`- Requested At: ${new Date(Number(recoveryRequest.requestedAt) * 1000).toLocaleString()}`);

  // Check if recovery is in the right state
  if (recoveryRequest.status !== 0) { // 0 = Requested
    console.error(`\nRecovery request is not in 'Requested' state. Current state: ${["Requested", "Approved", "Rejected", "Completed"][recoveryRequest.status]}`);
    process.exit(1);
  }

  // Parse amount to wei
  const amountInWei = hre.ethers.parseEther(amount);

  console.log("\nApproving Recovery:");
  console.log(`- Token: ${tokenAddress === "0x0000000000000000000000000000000000000000" ? "Native Token (ETH/AVAX)" : tokenAddress}`);
  console.log(`- Amount: ${amount} (${amountInWei} wei)`);
  console.log(`- Recipient: ${recipient}`);

  // Approve the recovery
  console.log("\nApproving recovery request...");
  const approveTx = await phoenixProtocol.approveRecovery(recoveryId);
  
  console.log(`Approval transaction submitted: ${approveTx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const approveReceipt = await approveTx.wait();
  console.log(`Approval transaction confirmed in block ${approveReceipt.blockNumber}`);

  // Set recovery details
  console.log("\nSetting recovery details...");
  const setDetailsTx = await recoveryModule.setRecoveryDetails(
    recoveryId,
    tokenAddress,
    amountInWei,
    recipient
  );
  
  console.log(`Set details transaction submitted: ${setDetailsTx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const setDetailsReceipt = await setDetailsTx.wait();
  console.log(`Set details transaction confirmed in block ${setDetailsReceipt.blockNumber}`);

  // Get updated recovery details
  const updatedRecoveryRequest = await recoveryModule.getRecoveryRequest(recoveryId);
  
  console.log("\nUpdated Recovery Request Details:");
  console.log(`- Status: ${["Requested", "Approved", "Rejected", "Completed"][updatedRecoveryRequest.status]}`);
  console.log(`- Token: ${updatedRecoveryRequest.tokenAddress}`);
  console.log(`- Amount: ${hre.ethers.formatEther(updatedRecoveryRequest.amount)} (${updatedRecoveryRequest.amount} wei)`);
  console.log(`- Recipient: ${updatedRecoveryRequest.recipient}`);
  console.log(`- Updated At: ${new Date(Number(updatedRecoveryRequest.updatedAt) * 1000).toLocaleString()}`);

  // Calculate when the recovery can be executed
  const recoveryCooldown = await recoveryModule.recoveryCooldown();
  const executeAfter = Number(updatedRecoveryRequest.updatedAt) + Number(recoveryCooldown);
  
  console.log("\nRecovery Execution Information:");
  console.log(`- Cooldown Period: ${Number(recoveryCooldown) / 3600} hours`);
  console.log(`- Can Execute After: ${new Date(executeAfter * 1000).toLocaleString()}`);
  console.log(`- Recovery ID: ${recoveryId}`);
  
  console.log("\nNext Steps:");
  console.log("After the cooldown period has elapsed, execute the recovery using:");
  console.log(`npx hardhat run scripts/execute-recovery.js --network <network> ${recoveryId}`);
}

// Execute the script
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });