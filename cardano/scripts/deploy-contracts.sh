#!/bin/bash

# Script to deploy Plutus contracts to the Cardano blockchain
# Usage: ./deploy-contracts.sh [testnet|mainnet]

# Set default network to testnet
NETWORK=${1:-testnet}

# Set network parameters
if [ "$NETWORK" = "testnet" ]; then
    NETWORK_PARAM="--testnet-magic 1097911063"
    NETWORK_NAME="testnet"
elif [ "$NETWORK" = "mainnet" ]; then
    NETWORK_PARAM="--mainnet"
    NETWORK_NAME="mainnet"
else
    echo "Invalid network. Use 'testnet' or 'mainnet'."
    exit 1
fi

# Set directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
PLUTUS_DIR="$ROOT_DIR/plutus"
OUTPUT_DIR="$PLUTUS_DIR/scripts"

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Compile Plutus scripts
echo "Compiling Plutus scripts..."
cd "$PLUTUS_DIR"
cabal run compile-scripts -- "$OUTPUT_DIR"

# Check if compilation was successful
if [ ! -f "$OUTPUT_DIR/intent-registry.plutus" ] || [ ! -f "$OUTPUT_DIR/recovery-module.plutus" ]; then
    echo "Error: Compilation failed. Plutus scripts not found."
    exit 1
fi

echo "Plutus scripts compiled successfully."

# Generate protocol parameters
echo "Generating protocol parameters..."
cardano-cli query protocol-parameters \
    $NETWORK_PARAM \
    --out-file "$OUTPUT_DIR/protocol-parameters.json"

# Generate a new key pair for the contracts if they don't exist
if [ ! -f "$OUTPUT_DIR/payment.skey" ]; then
    echo "Generating new key pair..."
    cardano-cli address key-gen \
        --verification-key-file "$OUTPUT_DIR/payment.vkey" \
        --signing-key-file "$OUTPUT_DIR/payment.skey"
    
    cardano-cli address build \
        --payment-verification-key-file "$OUTPUT_DIR/payment.vkey" \
        $NETWORK_PARAM \
        --out-file "$OUTPUT_DIR/payment.addr"
    
    echo "New key pair generated. Please fund the address:"
    cat "$OUTPUT_DIR/payment.addr"
    echo "Press Enter after funding the address..."
    read
fi

# Calculate script addresses
echo "Calculating script addresses..."
cardano-cli address build \
    --payment-script-file "$OUTPUT_DIR/intent-registry.plutus" \
    $NETWORK_PARAM \
    --out-file "$OUTPUT_DIR/intent-registry.addr"

cardano-cli address build \
    --payment-script-file "$OUTPUT_DIR/recovery-module.plutus" \
    $NETWORK_PARAM \
    --out-file "$OUTPUT_DIR/recovery-module.addr"

# Display script addresses
echo "Intent Registry address: $(cat "$OUTPUT_DIR/intent-registry.addr")"
echo "Recovery Module address: $(cat "$OUTPUT_DIR/recovery-module.addr")"

# Create a JSON file with contract addresses
cat > "$OUTPUT_DIR/contract-addresses.json" << EOF
{
    "network": "$NETWORK_NAME",
    "intentRegistry": "$(cat "$OUTPUT_DIR/intent-registry.addr")",
    "recoveryModule": "$(cat "$OUTPUT_DIR/recovery-module.addr")"
}
EOF

echo "Contract addresses saved to $OUTPUT_DIR/contract-addresses.json"
echo "Deployment completed successfully!"