#!/bin/bash

# Exit on error
set -e

# Display a message with a divider
function section() {
  echo "===================================================="
  echo "$1"
  echo "===================================================="
}

# Build the contracts
section "Building RugRun Plutus contracts"
cabal build

# Clean any previous outputs
section "Cleaning previous build outputs"
rm -rf output
mkdir -p output/scripts

# Export the compiled contracts
section "Exporting compiled contracts"
cabal run rugrun -- export

# Run a quick validation test
section "Running validation tests"
TEST_SECRET="haskell_loves_monads"
TEST_HASH=$(cabal run rugrun -- hash "$TEST_SECRET" | grep "Hash:" | cut -d ' ' -f 2)
echo "Generated hash: $TEST_HASH"
cabal run rugrun -- verify "$TEST_SECRET" "$TEST_HASH"

section "Build and export completed successfully!"
echo "Compiled scripts and related files are in the output directory."
echo "Use these files to interact with the Cardano blockchain." 