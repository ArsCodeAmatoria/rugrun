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
WALLET_NAME=${1:-"deployer"}
OUTPUT_DIR="output"
SCRIPTS_DIR="$OUTPUT_DIR/scripts"
ADDR_FILE="$OUTPUT_DIR/$WALLET_NAME.addr"
PAYMENT_SKEY="$OUTPUT_DIR/$WALLET_NAME.payment.skey"
PAYMENT_VKEY="$OUTPUT_DIR/$WALLET_NAME.payment.vkey"
PROTOCOL_FILE="$OUTPUT_DIR/protocol-params.json"

# Check if cardano-cli is available
if ! command -v cardano-cli &> /dev/null; then
    echo "cardano-cli could not be found. Please install it first."
    echo "You can use the Cardano Docker container as described in the README."
    exit 1
fi

# First build the contracts
section "Building contracts"
./scripts/build.sh

# Get protocol parameters
section "Fetching protocol parameters"
cardano-cli query protocol-parameters \
  --testnet-magic $TESTNET_MAGIC \
  --out-file $PROTOCOL_FILE

# Create wallet if it doesn't exist
if [ ! -f "$PAYMENT_SKEY" ]; then
  section "Creating wallet keys for $WALLET_NAME"
  cardano-cli address key-gen \
    --verification-key-file $PAYMENT_VKEY \
    --signing-key-file $PAYMENT_SKEY

  cardano-cli address build \
    --payment-verification-key-file $PAYMENT_VKEY \
    --testnet-magic $TESTNET_MAGIC \
    --out-file $ADDR_FILE

  echo "Wallet address: $(cat $ADDR_FILE)"
  echo "Fund this address with tADA before proceeding."
  echo "You can get test ADA from the faucet: https://docs.cardano.org/cardano-testnet/tools/faucet"
  exit 0
else
  echo "Using existing wallet: $WALLET_NAME"
  echo "Address: $(cat $ADDR_FILE)"
fi

# Create directories
mkdir -p $OUTPUT_DIR/tx

# Check wallet balance
section "Checking wallet balance"
cardano-cli query utxo \
  --address $(cat $ADDR_FILE) \
  --testnet-magic $TESTNET_MAGIC \
  --out-file $OUTPUT_DIR/utxo.json

LOVELACE_BALANCE=$(jq -r 'to_entries | map(.value.value.lovelace) | add' $OUTPUT_DIR/utxo.json)
ADA_BALANCE=$(echo "scale=6; $LOVELACE_BALANCE / 1000000" | bc)

echo "Wallet balance: $ADA_BALANCE ADA ($LOVELACE_BALANCE lovelace)"

if [ "$LOVELACE_BALANCE" -lt 10000000 ]; then
  echo "Wallet balance too low. Please fund your wallet with at least 10 ADA."
  exit 1
fi

# Generate on-chain tokens for the game
section "Generating RugRun tokens"
POLICY_FOLDER="$OUTPUT_DIR/policy"
mkdir -p $POLICY_FOLDER

# Create token policy
if [ ! -f "$POLICY_FOLDER/policy.script" ]; then
  cardano-cli address key-gen \
    --verification-key-file $POLICY_FOLDER/policy.vkey \
    --signing-key-file $POLICY_FOLDER/policy.skey

  KEYHASH=$(cardano-cli address key-hash --payment-verification-key-file $POLICY_FOLDER/policy.vkey)

  # Create a simple policy script
  cat > $POLICY_FOLDER/policy.script << EOF
{
  "type": "all",
  "scripts": [
    {
      "type": "sig",
      "keyHash": "$KEYHASH"
    }
  ]
}
EOF

  # Calculate the policy ID
  POLICY_ID=$(cardano-cli transaction policyid --script-file $POLICY_FOLDER/policy.script)
  echo $POLICY_ID > $POLICY_FOLDER/policyID

  echo "Created new token policy with ID: $POLICY_ID"
else
  POLICY_ID=$(cat $POLICY_FOLDER/policyID)
  echo "Using existing token policy with ID: $POLICY_ID"
fi

# Deploy the RugWallet and DecoyWallet contracts
section "Deploying RugWallet contract"

# Get UTxO details
UTXO_DATA=$(cardano-cli query utxo \
  --address $(cat $ADDR_FILE) \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /dev/stdout)

TX_IN=$(jq -r 'to_entries | [.[0].key] | .[]' <<< "$UTXO_DATA")
TX_IX=$(jq -r ".[\"$TX_IN\"].indecomposable_keys[0]" <<< "$UTXO_DATA")
AMOUNT=$(jq -r ".[\"$TX_IN\"].value.lovelace" <<< "$UTXO_DATA")

# Datum for the RugWallet
SECRET_PHRASE="haskell_loves_monads"  # You should change this for production
SECRET_HASH=$(./scripts/build.sh hash "$SECRET_PHRASE" | grep "Hash:" | cut -d ' ' -f 2)

# Create datum file
cat > $OUTPUT_DIR/rug-datum.json << EOF
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$SECRET_HASH"
    },
    {
      "int": 0
    },
    {
      "bytes": "$KEYHASH"
    }
  ]
}
EOF

# Calculate fee
cardano-cli transaction build-raw \
  --tx-in "$TX_IN#$TX_IX" \
  --tx-out "$(cat $ADDR_FILE)+0" \
  --tx-out-datum-hash-file $OUTPUT_DIR/rug-datum.json \
  --fee 0 \
  --out-file $OUTPUT_DIR/tx/tx.draft

FEE=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file $OUTPUT_DIR/tx/tx.draft \
  --tx-in-count 1 \
  --tx-out-count 1 \
  --witness-count 1 \
  --testnet-magic $TESTNET_MAGIC \
  --protocol-params-file $PROTOCOL_FILE | cut -d ' ' -f 1)

CHANGE=$((AMOUNT - FEE))

# Build the transaction
cardano-cli transaction build \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in "$TX_IN#$TX_IX" \
  --tx-out "$(cat $ADDR_FILE)+$CHANGE" \
  --change-address $(cat $ADDR_FILE) \
  --metadata-json-file $OUTPUT_DIR/contracts.json \
  --out-file $OUTPUT_DIR/tx/tx.raw

# Sign the transaction
cardano-cli transaction sign \
  --tx-body-file $OUTPUT_DIR/tx/tx.raw \
  --signing-key-file $PAYMENT_SKEY \
  --testnet-magic $TESTNET_MAGIC \
  --out-file $OUTPUT_DIR/tx/tx.signed

# Submit the transaction
cardano-cli transaction submit \
  --testnet-magic $TESTNET_MAGIC \
  --tx-file $OUTPUT_DIR/tx/tx.signed

section "RugRun contracts deployed successfully!"
echo "Contract parameters have been saved to $OUTPUT_DIR/contracts.json"
echo "Secret phrase for the real wallet: $SECRET_PHRASE"
echo "Secret hash: $SECRET_HASH"
echo "Token policy ID: $POLICY_ID"

# Save deployment info
cat > $OUTPUT_DIR/deployment-info.json << EOF
{
  "network": "testnet",
  "testnetMagic": $TESTNET_MAGIC,
  "deployedAt": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "policyId": "$POLICY_ID",
  "rugWalletHash": "$(cat $SCRIPTS_DIR/rugWalletHash.txt)",
  "decoyWalletHash": "$(cat $SCRIPTS_DIR/decoyWalletHash.txt)",
  "secretHash": "$SECRET_HASH"
}
EOF

echo "Deployment information saved to $OUTPUT_DIR/deployment-info.json" 