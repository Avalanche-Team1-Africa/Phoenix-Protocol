# Phoenix Protocol Documentation

### Version: 1.0.0
### Last Updated: April 23, 2025
### Authors: Joseph Mwangi 

---

## 1. Project Overview

### 1.1 What is Phoenix Protocol?
Phoenix Protocol is a smart contract recovery and UX protection middleware built for the decentralized finance (DeFi) and non-fungible token (NFT) ecosystems. It enables users to interact with smart contracts in a human-readable, intent-confirmed, and rollback-safe manner, preventing common transaction mishaps. It is built to operate across both Avalanche and Cardano blockchains.

### 1.2 Core Objectives
- Prevent user loss due to contract errors, front-end exploits, or transaction misconfigurations
- Facilitate secure, verified rollback of smart contract operations
- Enable social-based wallet recovery and intent delegation
- Bridge UX protection across Avalanche (C-Chain) and Cardano (Plutus/UTXO)
- Establish a decentralized dispute resolution system for blockchain transactions

---

## 2. Architectural Overview

### 2.1 Core Components
| Component               | Description                                                                 |
|------------------------|-----------------------------------------------------------------------------|
| Intent Registry        | Immutable storage of user-signed intent prior to contract execution         |
| Transaction Verifier   | Compares executed vs intended transactions for mismatch detection            |
| Recovery Controller    | Manages transaction rollbacks, wallet recovery, and asset restoration        |
| Dispute Resolution DAO | Arbitrates disputes with on-chain/off-chain logic and community governance   |
| Frontend SDK           | dApp interface layer to prompt intent, undo, dispute, or approve actions     |
| Social Guardian Module | Allows trusted contacts to assist in wallet or transaction recovery          |
| Cross-Chain Bridge     | Synchronizes user intents and asset states between Avalanche and Cardano     |

---

## 3. Transaction Workflow and Data Flow

### 3.1 Full Lifecycle (Intent-Based Execution)

#### Step 1: Intent Logging
1. User initiates transaction on Phoenix-integrated dApp (e.g., token swap).
2. SDK prompts user to confirm a human-readable intent.
3. Intent is hashed, signed, and stored in both:
   - On-chain Intent Registry (Avalanche/Cardano)
   - Off-chain IPFS (for auditability)

#### Step 2: Smart Contract Execution
4. User signs transaction with wallet.
5. dApp executes operation (swap, mint, transfer, stake).

#### Step 3: Transaction Validation
6. Transaction Verifier checks intent against executed transaction data:
   - Token types, amounts, recipient, slippage, timestamp.
7. If mismatch:
   - Trigger rollback logic (auto or manual).
   - Notify user and store discrepancy on-chain.

#### Step 4: Rollback & Dispute
- **If automatic rollback:**
   - Contract reverses transaction.
- **If dispute arises:**
   - Submit claim to Dispute DAO.
   - Evidence provided by both dApp and user.
   - DAO votes on fair outcome.

#### Step 5: Recovery and Governance
- Users can appoint guardians for social wallet recovery.
- If private key is lost:
   - Guardians verify identity.
   - Smart contract reassigns ownership.

---

## 4. Cross-Chain Interoperability

| Feature                         | Avalanche                                | Cardano                                 |
|---------------------------------|------------------------------------------|------------------------------------------|
| Contract Layer                  | Solidity (C-Chain)                       | Plutus (UTXO)                            |
| Intent Storage                  | Smart Contract + IPFS                    | Script Context + Metadata                |
| Execution Validation            | Chain Event Listener                     | Transaction Validator                    |
| Bridge Mechanism                | LayerZero/Wormhole                       | Milkomeda or Hydra-enabled scripts       |
| Asset Recovery                  | ERC20/ERC721 restoration logic           | ADA/Native Tokens rollback handler       |

---

## 5. Governance and Dispute Mechanism

### 5.1 Dispute Flow
1. Mismatch or malicious action detected
2. User submits dispute through dApp interface
3. Arbitration DAO collects evidence from both parties
4. AI + human jurors analyze:
   - Was user tricked?
   - Did smart contract behave maliciously?
5. Result:
   - Enforced rollback
   - Fund freeze
   - Dispute rejection (if invalid)

### 5.2 DAO Structure
- **Jury Pool**: Validators or reputation-holding users
- **AI Jury**: Verifies signatures, timestamps, market data
- **Outcome Execution**: Smart contract-triggered based on final vote

---

## 6. UI/UX Visualization

### 6.1 Screens and Elements

#### Home Page
- Overview of protocol
- Recent disputes and rollback cases
- Call-to-action for developers and users

#### dApp Transaction Screen
- Input: Transaction Type (swap, stake, transfer)
- Modal: Intent summary (e.g., "Swap 100 USDC to AVAX with 2% slippage")
- Buttons: [Confirm Intent] [Reject Intent]

#### Post-Transaction Screen
- Intent ID: #823989
- Status: MATCHED or DISPUTED
- Actions:
   - [Undo Transaction]
   - [Submit Dispute]
   - [Call Recovery Guardian]

#### Dispute Dashboard
- Open Cases
- Evidence Viewer (on-chain/off-chain)
- Voting Panel for DAO members

#### Recovery Screen
- Guardian Login
- Threshold Signatures (2-of-3 required)
- Wallet reassignment confirmation

---

## 7. Security Design

- **Intent Signatures**: ECDSA with timestamped payload
- **Rollback Authenticity**: Only executable within 5-minute intent validity
- **Guardian Recovery**: M-of-N approval required from designated addresses
- **Dispute Logs**: Immutable trail stored on IPFS and chain
- **Slippage Protection**: Hardcoded thresholds in smart contracts
- **Front-End Protection**: SDK intercepts and validates UI manipulations

---

## 8. Developer Integration

### 8.1 Phoenix SDK Features
- `initPhoenix(dAppId)` – Initializes middleware
- `createIntent(txType, payload)` – Logs intent pre-execution
- `verifyExecution(intentHash, txReceipt)` – Verifies actual transaction
- `triggerRollback(txId)` – Calls contract rollback
- `submitDispute(txId, evidence)` – Sends case to DAO
- `registerGuardian(address)` – Assigns recovery wallet

### 8.2 Example Integration (Solidity)
```solidity
function swapTokens(address tokenIn, address tokenOut, uint amount) public {
    require(verifyIntent(msg.sender, tokenIn, tokenOut, amount), "Invalid intent");
    _swap(tokenIn, tokenOut, amount);
    logExecution(msg.sender, tx.origin, tokenIn, tokenOut, amount);
}
```

---

## 9. Scalability & Roadmap

### Q2 2025
- [x] Phoenix SDK JS/TS
- [x] Intent Registry (Avalanche)
- [x] Dispute DAO MVP

### Q3 2025
- [ ] Cardano UTXO Integration
- [ ] Bridge using LayerZero + Milkomeda
- [ ] Full social recovery contract

### Q4 2025
- [ ] Institutional adoption with DeFi protocols
- [ ] DAO expansion with token-weighted jurors
- [ ] AI juror automation via GPT + Zero-Knowledge Proofs

---

## 10. Summary

Phoenix Protocol is designed to solve one of the most persistent issues in Web3: **transaction safety and smart contract recovery**. By focusing on human-readable intent, AI-assisted validation, DAO-based resolution, and chain-agnostic architecture, Phoenix Protocol ensures that users no longer suffer from irreversible blockchain errors. It bridges the **usability gap** between technical blockchains and non-technical users, offering a safer, reversible, and trust-enforced Web3 future.

