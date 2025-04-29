// Script to execute an intent in the Phoenix Protocol
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  // Get command line arguments
  const args = process.argv.slice(2);
  if (args.length < 1) {
    console.log("Usage: npx hardhat run scripts/execute-intent.js --network <network> <intent-id>");
    console.log("Example: npx hardhat run scripts/execute-intent.js --network fuji 0x1234...");
    process.exit(1);
  }

  const intentId = args[0];

  // Get contract addresses from .env
  const phoenixProtocolAddress = process.env.PHOENIX_PROTOCOL_ADDRESS;
  const intentRegistryAddress = process.env.INTENT_REGISTRY_ADDRESS;
  
  if (!phoenixProtocolAddress || !intentRegistryAddress) {
    throw new Error("Contract addresses not set in .env file");
  }

  // Get the signer
  const [signer] = await hre.ethers.getSigners();
  console.log(`Using account: ${signer.address}`);

  // Get contract instances
  const phoenixProtocol = await hre.ethers.getContractAt("PhoenixProtocol", phoenixProtocolAddress);
  const intentRegistry = await hre.ethers.getContractAt("IntentRegistry", intentRegistryAddress);

  // Get intent details
  console.log(`\nFetching intent details for ID: ${intentId}`);
  const intentDetails = await intentRegistry.getIntentDetails(intentId);
  
  if (intentDetails.user === "0x0000000000000000000000000000000000000000") {
    console.error("Intent not found");
    process.exit(1);
  }

  console.log("\nIntent Details:");
  console.log(`- User: ${intentDetails.user}`);
  console.log(`- Token: ${intentDetails.tokenAddress === "0x0000000000000000000000000000000000000000" ? "Native Token (ETH/AVAX)" : intentDetails.tokenAddress}`);
  console.log(`- Amount: ${hre.ethers.formatEther(intentDetails.amount)} (${intentDetails.amount} wei)`);
  console.log(`- Recipient: ${intentDetails.recipient}`);
  console.log(`- Expires: ${new Date(Number(intentDetails.expiresAt) * 1000).toLocaleString()}`);
  console.log(`- Type: ${["Send", "Swap", "Stake", "Approve", "Other"][intentDetails.intentType]}`);
  console.log(`- Status: ${intentDetails.isExecuted ? "Executed" : intentDetails.isCancelled ? "Cancelled" : "Pending"}`);
  console.log(`- Created: ${new Date(Number(intentDetails.createdAt) * 1000).toLocaleString()}`);

  // Check if intent is valid
  const [isValid, reason] = await intentRegistry.isIntentValid(intentId);
  if (!isValid) {
    console.error(`\nIntent is not valid: ${reason}`);
    process.exit(1);
  }

  // Create message to sign
  const messageHash = hre.ethers.keccak256(
    hre.ethers.solidityPacked(
      ["bytes32", "address", "address", "uint256", "address", "uint256"],
      [
        intentId,
        intentDetails.user,
        intentDetails.tokenAddress,
        intentDetails.amount,
        intentDetails.recipient,
        intentDetails.expiresAt
      ]
    )
  );

  console.log("\nSigning message...");
  const signature = await signer.signMessage(hre.ethers.getBytes(messageHash));
  console.log(`Signature: ${signature}`);

  // Execute the intent
  console.log("\nExecuting intent...");
  const tx = await phoenixProtocol.executeIntent(intentId, signature);
  
  console.log(`Transaction submitted: ${tx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const receipt = await tx.wait();
  console.log(`Transaction confirmed in block ${receipt.blockNumber}`);

  // Check if intent was executed
  const updatedIntentDetails = await intentRegistry.getIntentDetails(intentId);
  if (updatedIntentDetails.isExecuted) {
    console.log("\nIntent executed successfully!");
  } else {
    console.log("\nIntent execution failed. Please check the transaction details.");
  }
}

// Execute the script
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });