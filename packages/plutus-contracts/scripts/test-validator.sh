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
OUTPUT_DIR="output/test"
HASH_FILE="$OUTPUT_DIR/secret-hash.txt"
SECRET="haskellftw"

# Create output directory
mkdir -p "$OUTPUT_DIR"

section "Testing Plutus Wallet Validators"

# Step 1: Generate the hash for our secret
echo "Generating hash for secret: $SECRET"
cabal run plutus-contracts -- generate-hash "$SECRET" > "$HASH_FILE"
HASH=$(grep -oP "Generated hash for secret .+: \K.+" "$HASH_FILE")
echo "Hash: $HASH"

# Step 2: Compile the validators
echo "Compiling validators..."
cabal run plutus-contracts -- compile-validators "$OUTPUT_DIR"
echo "Validators compiled successfully"

# Step 3: Create a test game
echo "Creating a test game with 5 wallets..."
cabal run plutus-contracts -- generate-game "$OUTPUT_DIR" "$SECRET" 5 "$OUTPUT_DIR/game-wallets.json"

# Step 4: Test validation with correct secret
section "Testing with correct secret"
cabal run plutus-contracts -- test-validator "$HASH" "$SECRET"

# Step 5: Test validation with incorrect secret
section "Testing with incorrect secret"
cabal run plutus-contracts -- test-validator "$HASH" "wrongsecret"

section "Test completed"
echo "The real wallet's secret is: $SECRET"
echo "Its hash is: $HASH"
echo
echo "Game wallet data is in: $OUTPUT_DIR/game-wallets.json"
echo "You can use these to test the TypeScript simulation" 