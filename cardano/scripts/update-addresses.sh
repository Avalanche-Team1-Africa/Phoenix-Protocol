#!/bin/bash

# Script to update contract addresses in the frontend
# Usage: ./update-addresses.sh

# Set directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
PLUTUS_DIR="$ROOT_DIR/plutus"
OUTPUT_DIR="$PLUTUS_DIR/scripts"
FRONTEND_DIR="$ROOT_DIR/../frontend"

# Check if contract addresses exist
if [ ! -f "$OUTPUT_DIR/contract-addresses.json" ]; then
    echo "Error: Contract addresses not found. Please deploy contracts first."
    exit 1
fi

# Load contract addresses
NETWORK=$(jq -r '.network' "$OUTPUT_DIR/contract-addresses.json")
INTENT_REGISTRY_ADDR=$(jq -r '.intentRegistry' "$OUTPUT_DIR/contract-addresses.json")
RECOVERY_MODULE_ADDR=$(jq -r '.recoveryModule' "$OUTPUT_DIR/contract-addresses.json")

# Create or update .env.local file in frontend directory
ENV_FILE="$FRONTEND_DIR/.env.local"

# Check if .env.local exists
if [ -f "$ENV_FILE" ]; then
    # Update existing file
    sed -i "s|^NEXT_PUBLIC_CARDANO_INTENT_REGISTRY_ADDR=.*|NEXT_PUBLIC_CARDANO_INTENT_REGISTRY_ADDR=$INTENT_REGISTRY_ADDR|" "$ENV_FILE"
    sed -i "s|^NEXT_PUBLIC_CARDANO_RECOVERY_MODULE_ADDR=.*|NEXT_PUBLIC_CARDANO_RECOVERY_MODULE_ADDR=$RECOVERY_MODULE_ADDR|" "$ENV_FILE"
    sed -i "s|^NEXT_PUBLIC_CARDANO_NETWORK=.*|NEXT_PUBLIC_CARDANO_NETWORK=$NETWORK|" "$ENV_FILE"
else
    # Create new file
    cat > "$ENV_FILE" << EOF
# Cardano contract addresses
NEXT_PUBLIC_CARDANO_INTENT_REGISTRY_ADDR=$INTENT_REGISTRY_ADDR
NEXT_PUBLIC_CARDANO_RECOVERY_MODULE_ADDR=$RECOVERY_MODULE_ADDR
NEXT_PUBLIC_CARDANO_NETWORK=$NETWORK

# Add other environment variables here
EOF
fi

echo "Contract addresses updated in $ENV_FILE"

# Update contract addresses in the frontend constants file
CONSTANTS_FILE="$FRONTEND_DIR/src/lib/blockchain/constants.ts"

# Check if constants file exists
if [ -f "$CONSTANTS_FILE" ]; then
    # Create a temporary file
    TMP_FILE=$(mktemp)
    
    # Update the file
    cat "$CONSTANTS_FILE" | awk -v intent="$INTENT_REGISTRY_ADDR" -v recovery="$RECOVERY_MODULE_ADDR" -v network="$NETWORK" '
    /CARDANO_CONTRACT_ADDRESSES = {/ {
        print "export const CARDANO_CONTRACT_ADDRESSES = {"
        print "  " network ": {"
        print "    intentRegistry: \"" intent "\","
        print "    recoveryModule: \"" recovery "\""
        print "  }"
        print "};"
        skip = 1
        next
    }
    /};/ {
        if (skip) {
            skip = 0
            next
        }
    }
    {
        if (!skip) print
    }
    ' > "$TMP_FILE"
    
    # Replace the original file
    mv "$TMP_FILE" "$CONSTANTS_FILE"
    
    echo "Contract addresses updated in $CONSTANTS_FILE"
else
    echo "Warning: Constants file not found at $CONSTANTS_FILE"
fi

echo "Address update completed successfully!"