// Script to request recovery in the Phoenix Protocol
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  // Get command line arguments
  const args = process.argv.slice(2);
  if (args.length < 2) {
    console.log("Usage: npx hardhat run scripts/request-recovery.js --network <network> <transaction-id> <reason>");
    console.log("Example: npx hardhat run scripts/request-recovery.js --network fuji 0x1234... \"Transaction sent to wrong address\"");
    process.exit(1);
  }

  const transactionId = args[0];
  const reason = args[1];

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

  console.log("\nRequesting Recovery:");
  console.log(`- Transaction ID: ${transactionId}`);
  console.log(`- Reason: ${reason}`);

  // Request recovery
  console.log("\nSubmitting recovery request...");
  const tx = await phoenixProtocol.requestRecovery(transactionId, reason);
  
  console.log(`Transaction submitted: ${tx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const receipt = await tx.wait();
  console.log(`Transaction confirmed in block ${receipt.blockNumber}`);

  // Get the recovery ID from the event logs
  const recoveryRequestedEvent = receipt.logs
    .filter(log => {
      // Check if this is from the recovery module
      if (log.address.toLowerCase() !== recoveryModuleAddress.toLowerCase()) {
        return false;
      }
      // Check if this is a RecoveryRequested event
      return log.topics[0] === hre.ethers.id("RecoveryRequested(bytes32,address,bytes32,string)");
    })
    .map(log => {
      const decodedLog = recoveryModule.interface.parseLog({
        topics: log.topics,
        data: log.data
      });
      return decodedLog.args;
    })[0];

  if (recoveryRequestedEvent) {
    const recoveryId = recoveryRequestedEvent[0];
    console.log(`\nRecovery request submitted successfully!`);
    console.log(`Recovery ID: ${recoveryId}`);
    
    // Get recovery details
    const recoveryRequest = await recoveryModule.getRecoveryRequest(recoveryId);
    console.log("\nRecovery Request Details:");
    console.log(`- Requester: ${recoveryRequest.requester}`);
    console.log(`- Transaction ID: ${recoveryRequest.transactionId}`);
    console.log(`- Status: ${["Requested", "Approved", "Rejected", "Completed"][recoveryRequest.status]}`);
    console.log(`- Reason: ${recoveryRequest.reason}`);
    console.log(`- Requested At: ${new Date(Number(recoveryRequest.requestedAt) * 1000).toLocaleString()}`);
    
    console.log("\nNext Steps:");
    console.log("1. Wait for an admin to review and approve your recovery request");
    console.log("2. Once approved and after the cooldown period, execute the recovery using the recovery ID");
  } else {
    console.log("Could not find RecoveryRequested event in the logs");
  }
}

// Execute the script
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });