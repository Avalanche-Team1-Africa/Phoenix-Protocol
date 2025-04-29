const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Phoenix Protocol", function () {
  let phoenixProtocol;
  let intentRegistry;
  let recoveryModule;
  let tokenVault;
  let mockToken;
  let owner;
  let user1;
  let user2;
  let admin;
  let feeCollector;

  const protocolFeePercentage = 100; // 1%
  const recoveryFeePercentage = 500; // 5%
  const recoveryCooldown = 86400; // 24 hours in seconds

  beforeEach(async function () {
    // Get signers
    [owner, user1, user2, admin, feeCollector] = await ethers.getSigners();

    // Deploy mock ERC20 token for testing
    const MockToken = await ethers.getContractFactory("MockERC20");
    mockToken = await MockToken.deploy("Mock Token", "MTK");
    await mockToken.waitForDeployment();

    // Deploy IntentRegistry
    const IntentRegistry = await ethers.getContractFactory("IntentRegistry");
    intentRegistry = await IntentRegistry.deploy(admin.address, owner.address);
    await intentRegistry.waitForDeployment();

    // Deploy TokenVault
    const TokenVault = await ethers.getContractFactory("TokenVault");
    tokenVault = await TokenVault.deploy(admin.address, owner.address);
    await tokenVault.waitForDeployment();

    // Deploy RecoveryModule
    const RecoveryModule = await ethers.getContractFactory("RecoveryModule");
    recoveryModule = await RecoveryModule.deploy(
      admin.address,
      feeCollector.address,
      recoveryFeePercentage,
      recoveryCooldown
    );
    await recoveryModule.waitForDeployment();

    // Deploy PhoenixProtocol
    const PhoenixProtocol = await ethers.getContractFactory("PhoenixProtocol");
    phoenixProtocol = await PhoenixProtocol.deploy(
      admin.address,
      feeCollector.address,
      protocolFeePercentage
    );
    await phoenixProtocol.waitForDeployment();

    // Set up contract connections
    await phoenixProtocol.setIntentRegistry(await intentRegistry.getAddress());
    await phoenixProtocol.setRecoveryModule(await recoveryModule.getAddress());
    await phoenixProtocol.setTokenVault(await tokenVault.getAddress());
    await recoveryModule.setTokenVault(await tokenVault.getAddress());

    // Grant roles
    const PROTOCOL_ROLE = ethers.keccak256(ethers.toUtf8Bytes("PROTOCOL_ROLE"));
    await tokenVault.grantRole(PROTOCOL_ROLE, await phoenixProtocol.getAddress());
    
    const EXECUTOR_ROLE = ethers.keccak256(ethers.toUtf8Bytes("EXECUTOR_ROLE"));
    await intentRegistry.grantRole(EXECUTOR_ROLE, await phoenixProtocol.getAddress());
    
    await recoveryModule.grantRole(PROTOCOL_ROLE, await phoenixProtocol.getAddress());
  });

  describe("Basic Setup", function () {
    it("Should set the correct admin", async function () {
      const ADMIN_ROLE = ethers.keccak256(ethers.toUtf8Bytes("ADMIN_ROLE"));
      expect(await phoenixProtocol.hasRole(ADMIN_ROLE, admin.address)).to.be.true;
    });

    it("Should set the correct fee collector", async function () {
      expect(await phoenixProtocol.feeCollector()).to.equal(feeCollector.address);
    });

    it("Should set the correct protocol fee percentage", async function () {
      expect(await phoenixProtocol.protocolFeePercentage()).to.equal(protocolFeePercentage);
    });

    it("Should set the correct module addresses", async function () {
      expect(await phoenixProtocol.intentRegistry()).to.equal(await intentRegistry.getAddress());
      expect(await phoenixProtocol.recoveryModule()).to.equal(await recoveryModule.getAddress());
      expect(await phoenixProtocol.tokenVault()).to.equal(await tokenVault.getAddress());
    });
  });

  describe("Intent Management", function () {
    it("Should create an intent", async function () {
      const tokenAddress = await mockToken.getAddress();
      const amount = ethers.parseEther("1");
      const recipient = user2.address;
      const expiresAt = Math.floor(Date.now() / 1000) + 3600; // 1 hour from now
      const intentType = 0; // Send
      const additionalParams = "0x";

      await intentRegistry.connect(user1).createIntent(
        tokenAddress,
        amount,
        recipient,
        expiresAt,
        intentType,
        additionalParams
      );

      const nonce = await intentRegistry.getCurrentNonce(user1.address);
      expect(nonce).to.equal(1);
    });

    // Add more intent tests here
  });

  describe("Recovery Process", function () {
    it("Should request recovery", async function () {
      const transactionId = ethers.keccak256(ethers.toUtf8Bytes("test-transaction"));
      const reason = "Transaction failed";

      await phoenixProtocol.connect(user1).requestRecovery(transactionId, reason);

      // Add verification logic here
    });

    // Add more recovery tests here
  });

  // Add more test suites for other functionality
});

// Mock ERC20 token for testing
const MockERC20 = `
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract MockERC20 is ERC20 {
    constructor(string memory name, string memory symbol) ERC20(name, symbol) {
        _mint(msg.sender, 1000000 * 10 ** decimals());
    }

    function mint(address to, uint256 amount) external {
        _mint(to, amount);
    }
}
`;

// Export the mock contract for the test
module.exports = { MockERC20 };