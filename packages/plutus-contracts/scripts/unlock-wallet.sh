#!/bin/bash

# Exit on error
set -e

# Display a message with a divider
function section() {
  echo "===================================================="
  echo "$1"
  echo "===================================================="
}

# Set variables
TESTNET_MAGIC=1097911063  # Preprod testnet magic
OUTPUT_DIR="output"
SCRIPTS_DIR="$OUTPUT_DIR/scripts"
CONFIG_DIR="$OUTPUT_DIR/config"
WALLETS_DIR="$OUTPUT_DIR/wallets"
GAMES_DIR="$OUTPUT_DIR/games"
PROTOCOL_FILE="$OUTPUT_DIR/protocol-params.json"

# Check for required arguments
if [ $# -lt 3 ]; then
  echo "Usage: $0 GAME_NAME TARGET_WALLET SECRET_PHRASE [USER_WALLET]"
  echo "  GAME_NAME      - Name of the deployed game"
  echo "  TARGET_WALLET  - Name of the wallet you want to unlock"
  echo "  SECRET_PHRASE  - The secret phrase to unlock the wallet"
  echo "  USER_WALLET    - (Optional) Your wallet to receive funds if successful"
  exit 1
fi

GAME_NAME="$1"
TARGET_WALLET="$2"
SECRET_PHRASE="$3"
USER_WALLET="${4:-}"

# Check if cardano-cli is available
if ! command -v cardano-cli &> /dev/null; then
    echo "cardano-cli could not be found. Please install it first."
    exit 1
fi

# Check if game exists and is deployed
GAME_DIR="$GAMES_DIR/$GAME_NAME"
CONF_FILE="$GAME_DIR/game.conf"

if [ ! -d "$GAME_DIR" ]; then
  echo "Error: Game '$GAME_NAME' does not exist."
  exit 1
fi

if [ ! -f "$CONF_FILE" ]; then
  echo "Error: Configuration file not found for game '$GAME_NAME'."
  exit 1
fi

# Load game config
source $CONF_FILE

# Check if game is deployed
if [ "$STATUS" != "deployed" ]; then
  echo "Error: Game '$GAME_NAME' is not deployed yet."
  exit 1
fi

# Check if target wallet exists in the game
TARGET_WALLET_LINK="$GAME_DIR/wallets/$TARGET_WALLET"
if [ ! -L "$TARGET_WALLET_LINK" ]; then
  echo "Error: Wallet '$TARGET_WALLET' is not part of game '$GAME_NAME'."
  exit 1
fi

TARGET_WALLET_DIR=$(readlink -f "$TARGET_WALLET_LINK")
TARGET_WALLET_TYPE=$(cat "$TARGET_WALLET_DIR/type" 2>/dev/null || echo "Unknown")
TARGET_ADDR_FILE="$TARGET_WALLET_DIR/address"

if [ ! -f "$TARGET_ADDR_FILE" ]; then
  echo "Error: Address file not found for wallet '$TARGET_WALLET'."
  exit 1
fi

TARGET_ADDRESS=$(cat $TARGET_ADDR_FILE)

section "Attempting to unlock wallet: $TARGET_WALLET"
echo "Game: $GAME_NAME"
echo "Wallet: $TARGET_WALLET ($TARGET_WALLET_TYPE)"
echo "Address: $TARGET_ADDRESS"

# If no user wallet is provided, create a temporary one
if [ -z "$USER_WALLET" ]; then
  section "Creating temporary user wallet"
  TEMP_DIR=$(mktemp -d)
  USER_WALLET_DIR="$TEMP_DIR/wallet"
  mkdir -p "$USER_WALLET_DIR"
  
  # Generate payment keys
  cardano-cli address key-gen \
    --verification-key-file "$USER_WALLET_DIR/payment.vkey" \
    --signing-key-file "$USER_WALLET_DIR/payment.skey"
  
  # Build address
  cardano-cli address build \
    --payment-verification-key-file "$USER_WALLET_DIR/payment.vkey" \
    --testnet-magic $TESTNET_MAGIC \
    --out-file "$USER_WALLET_DIR/address"
  
  USER_ADDRESS=$(cat "$USER_WALLET_DIR/address")
  USER_SKEY="$USER_WALLET_DIR/payment.skey"
  
  echo "Temporary wallet created with address: $USER_ADDRESS"
  echo "Note: This wallet has no funds, so transactions will fail if the unlock fails."
else
  USER_WALLET_DIR="$WALLETS_DIR/$USER_WALLET"
  
  if [ ! -d "$USER_WALLET_DIR" ]; then
    echo "Error: User wallet '$USER_WALLET' does not exist."
    exit 1
  fi
  
  USER_ADDR_FILE="$USER_WALLET_DIR/address"
  USER_SKEY="$USER_WALLET_DIR/payment.skey"
  
  if [ ! -f "$USER_ADDR_FILE" ] || [ ! -f "$USER_SKEY" ]; then
    echo "Error: Missing key files for user wallet '$USER_WALLET'."
    exit 1
  fi
  
  USER_ADDRESS=$(cat $USER_ADDR_FILE)
  echo "Using your wallet: $USER_WALLET"
  echo "Address: $USER_ADDRESS"
  
  # Check user wallet balance
  UTXO_DATA=$(cardano-cli query utxo \
    --address $USER_ADDRESS \
    --testnet-magic $TESTNET_MAGIC \
    --out-file /dev/stdout)
  
  LOVELACE_BALANCE=$(echo $UTXO_DATA | jq -r 'if type=="object" then to_entries | map(.value.value.lovelace) | add else 0 end' 2>/dev/null || echo "0")
  
  if [ $LOVELACE_BALANCE -lt 3000000 ]; then
    echo "Warning: User wallet has low funds (${LOVELACE_BALANCE} lovelace). Need at least 3 ADA for transaction fees."
  fi
fi

# Check if target wallet has funds
TARGET_UTXO_DATA=$(cardano-cli query utxo \
  --address $TARGET_ADDRESS \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /dev/stdout)

TARGET_LOVELACE_BALANCE=$(echo $TARGET_UTXO_DATA | jq -r 'if type=="object" then to_entries | map(.value.value.lovelace) | add else 0 end' 2>/dev/null || echo "0")

if [ $TARGET_LOVELACE_BALANCE -eq 0 ]; then
  echo "Error: Target wallet has no funds."
  exit 1
fi

echo "Target wallet balance: $(echo "scale=6; $TARGET_LOVELACE_BALANCE / 1000000" | bc) ADA"

# Calculate secret hash
CALCULATED_HASH=$(echo -n "$SECRET_PHRASE" | sha256sum | cut -d ' ' -f 1)
echo "Provided secret hash: $CALCULATED_HASH"

# Verify the real wallet's secret
if [ "$TARGET_WALLET_TYPE" == "RealWallet" ]; then
  if [ "$CALCULATED_HASH" == "$SECRET_HASH" ]; then
    echo "✅ The secret phrase is correct for the real wallet!"
  else
    echo "❌ Incorrect secret phrase for the real wallet."
    exit 1
  fi
fi

# Load deployment info
DEPLOYMENT_INFO_FILE="$GAME_DIR/deployment-info.json"
if [ ! -f "$DEPLOYMENT_INFO_FILE" ]; then
  echo "Error: Deployment info file not found."
  exit 1
fi

VALIDATOR_FILE=$(jq -r '.validatorFile' $DEPLOYMENT_INFO_FILE)
POLICY_ID=$(jq -r '.policyId' $DEPLOYMENT_INFO_FILE)

if [ ! -f "$VALIDATOR_FILE" ]; then
  echo "Error: Validator file not found: $VALIDATOR_FILE"
  exit 1
fi

# Create transaction directory
TX_DIR="$OUTPUT_DIR/tx-$(date +%s)"
mkdir -p $TX_DIR

# Select a suitable UTxO from the target wallet
echo "Selecting a suitable UTxO from the target wallet..."
TARGET_UTXO=$(echo $TARGET_UTXO_DATA | jq -r 'to_entries | sort_by(.value.value.lovelace) | last | "\(.key)#\(.value.index)"')
TARGET_UTXO_AMOUNT=$(echo $TARGET_UTXO_DATA | jq -r 'to_entries | sort_by(.value.value.lovelace) | last | .value.value.lovelace')

if [ -z "$TARGET_UTXO" ] || [ "$TARGET_UTXO" == "#" ]; then
  echo "Error: No suitable UTxO found in target wallet."
  exit 1
fi

echo "Selected UTxO: $TARGET_UTXO with amount: $TARGET_UTXO_AMOUNT lovelace"

# Select a suitable UTxO from the user wallet (if available)
USER_UTXO=""
USER_UTXO_AMOUNT=0

if [ ! -z "$USER_WALLET" ]; then
  echo "Selecting a suitable UTxO from your wallet for fees..."
  USER_UTXO=$(echo $UTXO_DATA | jq -r 'to_entries | sort_by(.value.value.lovelace) | first | "\(.key)#\(.value.index)"')
  USER_UTXO_AMOUNT=$(echo $UTXO_DATA | jq -r 'to_entries | sort_by(.value.value.lovelace) | first | .value.value.lovelace')
  
  if [ -z "$USER_UTXO" ] || [ "$USER_UTXO" == "#" ]; then
    echo "Warning: No suitable UTxO found in user wallet. Proceeding without user input UTxO."
    USER_UTXO=""
    USER_UTXO_AMOUNT=0
  else
    echo "Selected user UTxO: $USER_UTXO with amount: $USER_UTXO_AMOUNT lovelace"
  fi
fi

# Create datum file with secret
DATUM_FILE="$TX_DIR/datum.json"
cat > $DATUM_FILE << EOF
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$(echo -n "$SECRET_PHRASE" | xxd -p -c 1000)"
    }
  ]
}
EOF

# Create redeemer file
REDEEMER_FILE="$TX_DIR/redeemer.json"
cat > $REDEEMER_FILE << EOF
{
  "constructor": 0,
  "fields": []
}
EOF

# Build the transaction
section "Building unlock transaction"

# Calculate fees
FEE=250000 # Initial fee estimate

# Create draft transaction
TX_DRAFT="$TX_DIR/tx.draft"
TX_SIGNED="$TX_DIR/tx.signed"

if [ -z "$USER_UTXO" ]; then
  # Only using target wallet UTxO
  cardano-cli transaction build \
    --babbage-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $TARGET_UTXO \
    --tx-in-script-file $VALIDATOR_FILE \
    --tx-in-datum-file $DATUM_FILE \
    --tx-in-redeemer-file $REDEEMER_FILE \
    --tx-in-collateral $TARGET_UTXO \
    --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file "$USER_WALLET_DIR/payment.vkey") \
    --change-address $USER_ADDRESS \
    --protocol-params-file $PROTOCOL_FILE \
    --out-file $TX_DRAFT
else
  # Using both target and user wallet UTxOs
  cardano-cli transaction build \
    --babbage-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $TARGET_UTXO \
    --tx-in-script-file $VALIDATOR_FILE \
    --tx-in-datum-file $DATUM_FILE \
    --tx-in-redeemer-file $REDEEMER_FILE \
    --tx-in $USER_UTXO \
    --tx-in-collateral $USER_UTXO \
    --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file "$USER_WALLET_DIR/payment.vkey") \
    --change-address $USER_ADDRESS \
    --protocol-params-file $PROTOCOL_FILE \
    --out-file $TX_DRAFT
fi

# Sign transaction
cardano-cli transaction sign \
  --tx-body-file $TX_DRAFT \
  --signing-key-file $USER_SKEY \
  --testnet-magic $TESTNET_MAGIC \
  --out-file $TX_SIGNED

# Submit transaction
section "Submitting transaction"
cardano-cli transaction submit \
  --testnet-magic $TESTNET_MAGIC \
  --tx-file $TX_SIGNED

# Get transaction hash
TX_HASH=$(cardano-cli transaction txid --tx-file $TX_SIGNED)

echo "Transaction submitted successfully!"
echo "Transaction hash: $TX_HASH"
echo "You can view it at https://preprod.cardanoscan.io/transaction/$TX_HASH"

if [ "$TARGET_WALLET_TYPE" == "RealWallet" ]; then
  echo ""
  echo "🎉 Congratulations! You've successfully unlocked the real wallet and claimed the funds!"
  
  # Record the successful attempt in the game
  ATTEMPTS_DIR="$GAME_DIR/attempts"
  mkdir -p $ATTEMPTS_DIR
  
  ATTEMPT_FILE="$ATTEMPTS_DIR/$(date +%s).json"
  cat > $ATTEMPT_FILE << EOF
{
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "wallet": "$TARGET_WALLET",
  "walletType": "$TARGET_WALLET_TYPE",
  "txHash": "$TX_HASH",
  "secretProvided": "$SECRET_PHRASE",
  "userAddress": "$USER_ADDRESS",
  "success": true
}
EOF
else
  echo ""
  echo "❌ Sorry, you unlocked a decoy wallet. This was not the real wallet with the prize."
  
  # Get clue from wallet
  CLUE_FILE="$TARGET_WALLET_DIR/clue"
  if [ -f "$CLUE_FILE" ]; then
    CLUE=$(cat $CLUE_FILE)
    echo "Clue from this wallet: \"$CLUE\""
  fi
  
  # Record the failed attempt in the game
  ATTEMPTS_DIR="$GAME_DIR/attempts"
  mkdir -p $ATTEMPTS_DIR
  
  ATTEMPT_FILE="$ATTEMPTS_DIR/$(date +%s).json"
  cat > $ATTEMPT_FILE << EOF
{
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "wallet": "$TARGET_WALLET",
  "walletType": "$TARGET_WALLET_TYPE",
  "txHash": "$TX_HASH",
  "secretProvided": "$SECRET_PHRASE",
  "userAddress": "$USER_ADDRESS",
  "success": false
}
EOF
fi

# Clean up temporary files if using a temporary wallet
if [ -z "$USER_WALLET" ] && [ ! -z "$TEMP_DIR" ]; then
  rm -rf $TEMP_DIR
fi 