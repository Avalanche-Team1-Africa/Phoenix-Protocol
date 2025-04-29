// Script to execute an approved recovery in the Phoenix Protocol
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  // Get command line arguments
  const args = process.argv.slice(2);
  if (args.length < 1) {
    console.log("Usage: npx hardhat run scripts/execute-recovery.js --network <network> <recovery-id>");
    console.log("Example: npx hardhat run scripts/execute-recovery.js --network fuji 0x1234...");
    process.exit(1);
  }

  const recoveryId = args[0];

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
  console.log(`- Token: ${recoveryRequest.tokenAddress === "0x0000000000000000000000000000000000000000" ? "Native Token (ETH/AVAX)" : recoveryRequest.tokenAddress}`);
  console.log(`- Amount: ${hre.ethers.formatEther(recoveryRequest.amount)} (${recoveryRequest.amount} wei)`);
  console.log(`- Recipient: ${recoveryRequest.recipient}`);
  console.log(`- Requested At: ${new Date(Number(recoveryRequest.requestedAt) * 1000).toLocaleString()}`);
  console.log(`- Updated At: ${new Date(Number(recoveryRequest.updatedAt) * 1000).toLocaleString()}`);

  // Check if recovery is in the right state
  if (recoveryRequest.status !== 1) { // 1 = Approved
    console.error(`\nRecovery request is not in 'Approved' state. Current state: ${["Requested", "Approved", "Rejected", "Completed"][recoveryRequest.status]}`);
    process.exit(1);
  }

  // Check if cooldown period has elapsed
  const recoveryCooldown = await recoveryModule.recoveryCooldown();
  const executeAfter = Number(recoveryRequest.updatedAt) + Number(recoveryCooldown);
  const currentTime = Math.floor(Date.now() / 1000);
  
  if (currentTime < executeAfter) {
    console.error(`\nCooldown period has not elapsed yet. Can execute after: ${new Date(executeAfter * 1000).toLocaleString()}`);
    console.error(`Current time: ${new Date(currentTime * 1000).toLocaleString()}`);
    console.error(`Time remaining: ${Math.ceil((executeAfter - currentTime) / 60)} minutes`);
    process.exit(1);
  }

  // Execute the recovery
  console.log("\nExecuting recovery...");
  const tx = await phoenixProtocol.executeRecovery(recoveryId);
  
  console.log(`Transaction submitted: ${tx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const receipt = await tx.wait();
  console.log(`Transaction confirmed in block ${receipt.blockNumber}`);

  // Get updated recovery details
  const updatedRecoveryRequest = await recoveryModule.getRecoveryRequest(recoveryId);
  
  if (updatedRecoveryRequest.status === 3) { // 3 = Completed
    console.log("\nRecovery executed successfully!");
    console.log(`- Executed At: ${new Date(Number(updatedRecoveryRequest.executedAt) * 1000).toLocaleString()}`);
    
    // Calculate recovery fee
    const recoveryFeePercentage = await recoveryModule.recoveryFeePercentage();
    const recoveryFee = (Number(recoveryRequest.amount) * Number(recoveryFeePercentage)) / 10000;
    const amountAfterFee = Number(recoveryRequest.amount) - recoveryFee;
    
    console.log("\nRecovery Summary:");
    console.log(`- Total Amount: ${hre.ethers.formatEther(recoveryRequest.amount)}`);
    console.log(`- Recovery Fee (${Number(recoveryFeePercentage) / 100}%): ${hre.ethers.formatEther(recoveryFee.toString())}`);
    console.log(`- Amount Received: ${hre.ethers.formatEther(amountAfterFee.toString())}`);
    console.log(`- Recipient: ${recoveryRequest.recipient}`);
  } else {
    console.log("\nRecovery execution failed. Current status: ${["Requested", "Approved", "Rejected", "Completed"][updatedRecoveryRequest.status]}");
  }
}

// Execute the script
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });