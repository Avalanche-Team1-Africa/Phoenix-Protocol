// Script to create an intent in the Phoenix Protocol
const hre = require("hardhat");
require("dotenv").config({ path: "../.env" });

async function main() {
  // Get command line arguments
  const args = process.argv.slice(2);
  if (args.length < 4) {
    console.log("Usage: npx hardhat run scripts/create-intent.js --network <network> <token-address> <amount> <recipient> <expires-in-hours>");
    console.log("Example: npx hardhat run scripts/create-intent.js --network fuji 0x0000000000000000000000000000000000000000 0.1 0x1234... 24");
    process.exit(1);
  }

  const tokenAddress = args[0];
  const amount = args[1];
  const recipient = args[2];
  const expiresInHours = parseInt(args[3]);

  // Get the intent registry address from .env
  const intentRegistryAddress = process.env.INTENT_REGISTRY_ADDRESS;
  if (!intentRegistryAddress) {
    throw new Error("INTENT_REGISTRY_ADDRESS not set in .env file");
  }

  // Get the signer
  const [signer] = await hre.ethers.getSigners();
  console.log(`Using account: ${signer.address}`);

  // Get the intent registry contract
  const intentRegistry = await hre.ethers.getContractAt("IntentRegistry", intentRegistryAddress);

  // Calculate expiration time
  const expiresAt = Math.floor(Date.now() / 1000) + (expiresInHours * 3600);

  // Parse amount to wei
  const amountInWei = hre.ethers.parseEther(amount);

  // Intent type (0 = Send)
  const intentType = 0;

  // Additional parameters (empty for now)
  const additionalParams = "0x";

  console.log("\nCreating Intent:");
  console.log(`- Token: ${tokenAddress === "0x0000000000000000000000000000000000000000" ? "Native Token (ETH/AVAX)" : tokenAddress}`);
  console.log(`- Amount: ${amount} (${amountInWei} wei)`);
  console.log(`- Recipient: ${recipient}`);
  console.log(`- Expires: in ${expiresInHours} hours (${new Date(expiresAt * 1000).toLocaleString()})`);
  console.log(`- Type: Send`);

  // Create the intent
  console.log("\nSubmitting transaction...");
  const tx = await intentRegistry.createIntent(
    tokenAddress,
    amountInWei,
    recipient,
    expiresAt,
    intentType,
    additionalParams
  );

  console.log(`Transaction submitted: ${tx.hash}`);
  console.log("Waiting for confirmation...");

  // Wait for the transaction to be mined
  const receipt = await tx.wait();
  console.log(`Transaction confirmed in block ${receipt.blockNumber}`);

  // Get the intent ID from the event logs
  const intentCreatedEvent = receipt.logs
    .filter(log => log.topics[0] === hre.ethers.id("IntentCreated(bytes32,address,address,uint256,address,uint256,uint8)"))
    .map(log => {
      const decodedLog = intentRegistry.interface.parseLog({
        topics: log.topics,
        data: log.data
      });
      return decodedLog.args;
    })[0];

  if (intentCreatedEvent) {
    const intentId = intentCreatedEvent[0];
    console.log(`\nIntent created successfully!`);
    console.log(`Intent ID: ${intentId}`);
    
    // Get the current nonce
    const nonce = await intentRegistry.getCurrentNonce(signer.address);
    console.log(`Current nonce: ${nonce}`);
  } else {
    console.log("Could not find IntentCreated event in the logs");
  }
}

// Execute the script
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });