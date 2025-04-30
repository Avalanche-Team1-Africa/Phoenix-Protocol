#!/bin/bash

# Script to interact with Plutus contracts on the Cardano blockchain
# Usage: ./interact-with-contracts.sh [testnet|mainnet] [command] [args...]

# Set default network to testnet
NETWORK=${1:-testnet}
COMMAND=${2:-help}

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

# Check if contract addresses exist
if [ ! -f "$OUTPUT_DIR/contract-addresses.json" ]; then
    echo "Error: Contract addresses not found. Please deploy contracts first."
    exit 1
fi

# Load contract addresses
INTENT_REGISTRY_ADDR=$(jq -r '.intentRegistry' "$OUTPUT_DIR/contract-addresses.json")
RECOVERY_MODULE_ADDR=$(jq -r '.recoveryModule' "$OUTPUT_DIR/contract-addresses.json")

# Function to display help
function show_help {
    echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] [command] [args...]"
    echo ""
    echo "Commands:"
    echo "  register-intent [action] [parameters] [expiry]  - Register a new intent"
    echo "  execute-intent [intent-id]                      - Execute an existing intent"
    echo "  cancel-intent [intent-id]                       - Cancel an intent"
    echo "  request-recovery [tx-id] [reason] [amount] [token] - Request recovery for a transaction"
    echo "  approve-recovery [recovery-id]                  - Approve a recovery request"
    echo "  execute-recovery [recovery-id]                  - Execute an approved recovery"
    echo "  help                                            - Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./interact-with-contracts.sh testnet register-intent \"swap\" \"token1,token2,100\" \"1717171717\""
    echo "  ./interact-with-contracts.sh testnet execute-intent \"intent-id\""
    echo "  ./interact-with-contracts.sh testnet request-recovery \"tx-id\" \"wrong-amount\" \"100\" \"ADA\""
}

# Function to register a new intent
function register_intent {
    ACTION=$1
    PARAMETERS=$2
    EXPIRY=$3
    
    if [ -z "$ACTION" ] || [ -z "$PARAMETERS" ] || [ -z "$EXPIRY" ]; then
        echo "Error: Missing arguments for register-intent."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] register-intent [action] [parameters] [expiry]"
        exit 1
    fi
    
    echo "Registering intent..."
    echo "Action: $ACTION"
    echo "Parameters: $PARAMETERS"
    echo "Expiry: $EXPIRY"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    INTENT_ID=$(echo "$ACTION-$PARAMETERS-$EXPIRY" | md5sum | cut -d' ' -f1)
    echo "Intent registered with ID: $INTENT_ID"
}

# Function to execute an intent
function execute_intent {
    INTENT_ID=$1
    
    if [ -z "$INTENT_ID" ]; then
        echo "Error: Missing intent ID for execute-intent."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] execute-intent [intent-id]"
        exit 1
    fi
    
    echo "Executing intent with ID: $INTENT_ID"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    echo "Intent executed successfully."
}

# Function to cancel an intent
function cancel_intent {
    INTENT_ID=$1
    
    if [ -z "$INTENT_ID" ]; then
        echo "Error: Missing intent ID for cancel-intent."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] cancel-intent [intent-id]"
        exit 1
    fi
    
    echo "Cancelling intent with ID: $INTENT_ID"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    echo "Intent cancelled successfully."
}

# Function to request recovery
function request_recovery {
    TX_ID=$1
    REASON=$2
    AMOUNT=$3
    TOKEN=$4
    
    if [ -z "$TX_ID" ] || [ -z "$REASON" ] || [ -z "$AMOUNT" ] || [ -z "$TOKEN" ]; then
        echo "Error: Missing arguments for request-recovery."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] request-recovery [tx-id] [reason] [amount] [token]"
        exit 1
    fi
    
    echo "Requesting recovery..."
    echo "Transaction ID: $TX_ID"
    echo "Reason: $REASON"
    echo "Amount: $AMOUNT"
    echo "Token: $TOKEN"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    RECOVERY_ID=$(echo "$TX_ID-$REASON-$AMOUNT-$TOKEN" | md5sum | cut -d' ' -f1)
    echo "Recovery requested with ID: $RECOVERY_ID"
}

# Function to approve a recovery request
function approve_recovery {
    RECOVERY_ID=$1
    
    if [ -z "$RECOVERY_ID" ]; then
        echo "Error: Missing recovery ID for approve-recovery."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] approve-recovery [recovery-id]"
        exit 1
    fi
    
    echo "Approving recovery with ID: $RECOVERY_ID"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    echo "Recovery approved successfully."
}

# Function to execute a recovery
function execute_recovery {
    RECOVERY_ID=$1
    
    if [ -z "$RECOVERY_ID" ]; then
        echo "Error: Missing recovery ID for execute-recovery."
        echo "Usage: ./interact-with-contracts.sh [testnet|mainnet] execute-recovery [recovery-id]"
        exit 1
    fi
    
    echo "Executing recovery with ID: $RECOVERY_ID"
    
    # In a real implementation, this would construct and submit a transaction
    # For now, we'll just simulate it
    echo "Recovery executed successfully."
}

# Execute the requested command
case $COMMAND in
    register-intent)
        register_intent "$3" "$4" "$5"
        ;;
    execute-intent)
        execute_intent "$3"
        ;;
    cancel-intent)
        cancel_intent "$3"
        ;;
    request-recovery)
        request_recovery "$3" "$4" "$5" "$6"
        ;;
    approve-recovery)
        approve_recovery "$3"
        ;;
    execute-recovery)
        execute_recovery "$3"
        ;;
    help|*)
        show_help
        ;;
esac