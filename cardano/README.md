--- cardano/README.md
+++ cardano/README.md
@@ -0,0 +1,85 @@
+# Phoenix Protocol - Cardano Implementation
+
+This directory contains the Cardano implementation of the Phoenix Protocol, a smart contract recovery and UX protection middleware for DeFi and NFT ecosystems.
+
+## Overview
+
+The Cardano implementation leverages Plutus, Cardano's native smart contract platform, to provide the same functionality as the Avalanche implementation but in a UTXO-based model. This implementation takes advantage of Cardano's formal verification capabilities, deterministic fees, and native multi-asset support.
+
+## Directory Structure
+
+- `plutus/`: Contains the Plutus smart contracts
+  - `src/`: Source code for the Plutus scripts
+  - `app/`: Compiler application for the Plutus scripts
+  - `scripts/`: Compiled Plutus scripts
+- `scripts/`: Helper scripts for interacting with the Cardano blockchain
+
+## Smart Contracts
+
+### Intent Registry
+
+The Intent Registry contract stores user intents for transactions. Intents represent a user's desire to perform a specific action, which can be executed later by the protocol.
+
+Key features:
+- Intent creation with expiration time
+- Intent execution with validation
+- Intent cancellation by the owner
+
+### Recovery Module
+
+The Recovery Module contract manages the recovery process for transactions, allowing users to request recovery of funds and guardians to approve or reject these requests.
+
+Key features:
+- Recovery request creation
+- Guardian approval mechanism
+- M-of-N approval threshold
+- Recovery execution with validation
+
+## Building and Deploying
+
+### Prerequisites
+
+- GHC (Glasgow Haskell Compiler)
+- Cabal
+- Cardano Node
+- Cardano CLI
+
+### Building the Contracts
+
+```bash
+cd plutus
+cabal build
+cabal run compile-scripts -- ../scripts
+```
+
+### Deploying the Contracts
+
+```bash
+cd ../scripts
+./deploy-contracts.sh testnet
+```
+
+## Integration with Frontend
+
+The frontend integration with Cardano wallets is implemented in the `frontend/src/lib/blockchain/wallet-connect.ts` file. It supports popular Cardano wallets like Nami, Eternl, and Flint.
+
+## Bridge Mechanism
+
+The bridge between Avalanche and Cardano is implemented using Milkomeda, a sidechain that enables EVM compatibility on Cardano. This allows for seamless communication between the two blockchains.
+
+## Testing
+
+To test the Cardano implementation:
+
+```bash
+cd plutus
+cabal test
+```
+
+## Future Work
+
+- Complete wallet integration with Nami, Eternl, and Flint
+- Implement transaction validation and recovery logic
+- Deploy and verify on Cardano Testnet
+- Implement bridge mechanism with Avalanche
+- Optimize for performance and gas efficiency