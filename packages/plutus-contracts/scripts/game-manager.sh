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

# Create necessary directories
mkdir -p $SCRIPTS_DIR $CONFIG_DIR $WALLETS_DIR $GAMES_DIR

# Check if cardano-cli is available
if ! command -v cardano-cli &> /dev/null; then
    echo "cardano-cli could not be found. Please install it first."
    echo "You can use the Cardano Docker container as described in the README."
    exit 1
fi

# Command menu
function show_help() {
  echo "RugRun Game Manager"
  echo "Available commands:"
  echo "  setup                  - Setup initial environment"
  echo "  wallet create NAME     - Create a new wallet"
  echo "  wallet list            - List all wallets"
  echo "  wallet balance NAME    - Show wallet balance"
  echo "  game create NAME       - Create a new game"
  echo "  game deploy NAME       - Deploy a game to the blockchain"
  echo "  game status NAME       - Show game status"
  echo "  game list              - List all games"
  echo "  wallet add-to WALLET GAME - Add a wallet to a game"
  echo "  help                   - Show this help"
}

# Setup initial environment
function setup_environment() {
  section "Setting up environment"
  
  # Get protocol parameters
  section "Fetching protocol parameters"
  cardano-cli query protocol-parameters \
    --testnet-magic $TESTNET_MAGIC \
    --out-file $PROTOCOL_FILE
  
  # Build contracts
  section "Building contracts"
  if [ -f "./scripts/build.sh" ]; then
    ./scripts/build.sh
  else
    echo "Build script not found. Please make sure you're in the right directory."
    exit 1
  fi
  
  echo "Environment setup complete!"
}

# Create a new wallet
function create_wallet() {
  local WALLET_NAME=$1
  
  if [ -z "$WALLET_NAME" ]; then
    echo "Error: Wallet name is required."
    exit 1
  fi
  
  local WALLET_DIR="$WALLETS_DIR/$WALLET_NAME"
  local PAYMENT_SKEY="$WALLET_DIR/payment.skey"
  local PAYMENT_VKEY="$WALLET_DIR/payment.vkey"
  local ADDR_FILE="$WALLET_DIR/address"
  
  if [ -d "$WALLET_DIR" ]; then
    echo "Error: Wallet '$WALLET_NAME' already exists."
    exit 1
  fi
  
  section "Creating wallet: $WALLET_NAME"
  mkdir -p "$WALLET_DIR"
  
  # Generate payment keys
  cardano-cli address key-gen \
    --verification-key-file $PAYMENT_VKEY \
    --signing-key-file $PAYMENT_SKEY
  
  # Build address
  cardano-cli address build \
    --payment-verification-key-file $PAYMENT_VKEY \
    --testnet-magic $TESTNET_MAGIC \
    --out-file $ADDR_FILE
  
  echo "Wallet created successfully!"
  echo "Address: $(cat $ADDR_FILE)"
  echo "Please fund this address with tADA before using it in games."
}

# List all wallets
function list_wallets() {
  section "Available Wallets"
  
  if [ -z "$(ls -A $WALLETS_DIR 2>/dev/null)" ]; then
    echo "No wallets found. Create one with 'wallet create NAME'."
    return
  fi
  
  echo "NAME | ADDRESS | BALANCE"
  echo "---------------------"
  
  for WALLET_DIR in $WALLETS_DIR/*; do
    if [ -d "$WALLET_DIR" ]; then
      WALLET_NAME=$(basename $WALLET_DIR)
      ADDR_FILE="$WALLET_DIR/address"
      
      if [ -f "$ADDR_FILE" ]; then
        ADDRESS=$(cat $ADDR_FILE)
        
        # Get balance if possible
        BALANCE="unknown"
        if command -v cardano-cli &> /dev/null; then
          UTXO_DATA=$(cardano-cli query utxo \
            --address $ADDRESS \
            --testnet-magic $TESTNET_MAGIC \
            --out-file /dev/stdout 2>/dev/null || echo '{}')
          
          LOVELACE_BALANCE=$(echo $UTXO_DATA | jq -r 'if type=="object" then to_entries | map(.value.value.lovelace) | add else 0 end' 2>/dev/null || echo "N/A")
          if [ "$LOVELACE_BALANCE" != "N/A" ] && [ "$LOVELACE_BALANCE" != "null" ]; then
            ADA_BALANCE=$(echo "scale=6; $LOVELACE_BALANCE / 1000000" | bc)
            BALANCE="${ADA_BALANCE} ADA"
          fi
        fi
        
        echo "$WALLET_NAME | ${ADDRESS:0:12}... | $BALANCE"
      else
        echo "$WALLET_NAME | No address file found"
      fi
    fi
  done
}

# Show wallet balance
function wallet_balance() {
  local WALLET_NAME=$1
  
  if [ -z "$WALLET_NAME" ]; then
    echo "Error: Wallet name is required."
    exit 1
  fi
  
  local WALLET_DIR="$WALLETS_DIR/$WALLET_NAME"
  local ADDR_FILE="$WALLET_DIR/address"
  
  if [ ! -d "$WALLET_DIR" ]; then
    echo "Error: Wallet '$WALLET_NAME' does not exist."
    exit 1
  fi
  
  if [ ! -f "$ADDR_FILE" ]; then
    echo "Error: Address file not found for wallet '$WALLET_NAME'."
    exit 1
  fi
  
  section "Wallet Balance: $WALLET_NAME"
  local ADDRESS=$(cat $ADDR_FILE)
  echo "Address: $ADDRESS"
  
  # Query UTxOs
  cardano-cli query utxo \
    --address $ADDRESS \
    --testnet-magic $TESTNET_MAGIC
  
  # Calculate total balance
  local UTXO_DATA=$(cardano-cli query utxo \
    --address $ADDRESS \
    --testnet-magic $TESTNET_MAGIC \
    --out-file /dev/stdout)
  
  local LOVELACE_BALANCE=$(echo $UTXO_DATA | jq -r 'if type=="object" then to_entries | map(.value.value.lovelace) | add else 0 end' 2>/dev/null || echo "0")
  local ADA_BALANCE=$(echo "scale=6; $LOVELACE_BALANCE / 1000000" | bc)
  
  echo ""
  echo "Total Balance: $ADA_BALANCE ADA ($LOVELACE_BALANCE lovelace)"
}

# Create a new game
function create_game() {
  local GAME_NAME=$1
  
  if [ -z "$GAME_NAME" ]; then
    echo "Error: Game name is required."
    exit 1
  fi
  
  local GAME_DIR="$GAMES_DIR/$GAME_NAME"
  
  if [ -d "$GAME_DIR" ]; then
    echo "Error: Game '$GAME_NAME' already exists."
    exit 1
  fi
  
  section "Creating game: $GAME_NAME"
  mkdir -p "$GAME_DIR"
  mkdir -p "$GAME_DIR/wallets"
  
  # Ask for game parameters
  echo "Please select a difficulty level:"
  echo "1) Easy (3 wallets, 50 ADA reward)"
  echo "2) Medium (5 wallets, 100 ADA reward)"
  echo "3) Hard (8 wallets, 250 ADA reward)"
  echo "4) Expert (12 wallets, 500 ADA reward)"
  read -p "Select [1-4]: " DIFFICULTY_CHOICE
  
  case $DIFFICULTY_CHOICE in
    1) DIFFICULTY="Easy" ;;
    2) DIFFICULTY="Medium" ;;
    3) DIFFICULTY="Hard" ;;
    4) DIFFICULTY="Expert" ;;
    *) DIFFICULTY="Medium" ;;
  esac
  
  # Secret phrase for the real wallet
  read -p "Enter a secret phrase for the real wallet: " SECRET_PHRASE
  
  if [ -z "$SECRET_PHRASE" ]; then
    SECRET_PHRASE="haskell_loves_monads_$(date +%s)"
    echo "Using auto-generated secret: $SECRET_PHRASE"
  fi
  
  # Generate secret hash
  local SECRET_HASH=$(echo -n "$SECRET_PHRASE" | sha256sum | cut -d ' ' -f 1)
  
  # Create game config file
  cat > "$GAME_DIR/game.conf" << EOF
GAME_NAME="$GAME_NAME"
DIFFICULTY="$DIFFICULTY"
SECRET_PHRASE="$SECRET_PHRASE"
SECRET_HASH="$SECRET_HASH"
CREATED_AT="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
STATUS="created"
EOF
  
  echo "Game created successfully!"
  echo "Configuration saved to $GAME_DIR/game.conf"
  echo ""
  echo "Next steps:"
  echo "1. Create wallets with 'wallet create NAME'"
  echo "2. Add wallets to the game with 'wallet add-to WALLET $GAME_NAME'"
  echo "3. Deploy the game with 'game deploy $GAME_NAME'"
}

# List all games
function list_games() {
  section "Available Games"
  
  if [ -z "$(ls -A $GAMES_DIR 2>/dev/null)" ]; then
    echo "No games found. Create one with 'game create NAME'."
    return
  fi
  
  echo "NAME | DIFFICULTY | STATUS | WALLETS | SECRET"
  echo "---------------------------------------------"
  
  for GAME_DIR in $GAMES_DIR/*; do
    if [ -d "$GAME_DIR" ]; then
      GAME_NAME=$(basename $GAME_DIR)
      CONF_FILE="$GAME_DIR/game.conf"
      
      if [ -f "$CONF_FILE" ]; then
        source $CONF_FILE
        
        # Count wallets
        WALLET_COUNT=$(ls -1 $GAME_DIR/wallets/ 2>/dev/null | wc -l)
        
        # Show partial secret for security
        PARTIAL_SECRET="${SECRET_PHRASE:0:3}***${SECRET_PHRASE: -3}"
        
        echo "$GAME_NAME | $DIFFICULTY | $STATUS | $WALLET_COUNT | $PARTIAL_SECRET"
      else
        echo "$GAME_NAME | No config file found"
      fi
    fi
  done
}

# Show game status
function game_status() {
  local GAME_NAME=$1
  
  if [ -z "$GAME_NAME" ]; then
    echo "Error: Game name is required."
    exit 1
  fi
  
  local GAME_DIR="$GAMES_DIR/$GAME_NAME"
  local CONF_FILE="$GAME_DIR/game.conf"
  
  if [ ! -d "$GAME_DIR" ]; then
    echo "Error: Game '$GAME_NAME' does not exist."
    exit 1
  fi
  
  if [ ! -f "$CONF_FILE" ]; then
    echo "Error: Configuration file not found for game '$GAME_NAME'."
    exit 1
  fi
  
  section "Game Status: $GAME_NAME"
  source $CONF_FILE
  
  echo "Name: $GAME_NAME"
  echo "Difficulty: $DIFFICULTY"
  echo "Status: $STATUS"
  echo "Created At: $CREATED_AT"
  
  # List wallets
  echo ""
  echo "Wallets:"
  if [ -z "$(ls -A $GAME_DIR/wallets/ 2>/dev/null)" ]; then
    echo "No wallets added to this game yet."
  else
    for WALLET_LINK in $GAME_DIR/wallets/*; do
      if [ -L "$WALLET_LINK" ]; then
        WALLET_NAME=$(basename $WALLET_LINK)
        WALLET_TYPE=$(cat $WALLET_LINK/type 2>/dev/null || echo "Unknown")
        echo "- $WALLET_NAME (Type: $WALLET_TYPE)"
      fi
    done
  fi
  
  # If deployed, show on-chain info
  if [ "$STATUS" == "deployed" ]; then
    if [ -f "$GAME_DIR/tx.json" ]; then
      TX_HASH=$(jq -r '.txHash' $GAME_DIR/tx.json)
      echo ""
      echo "Transaction Hash: $TX_HASH"
      echo "Explore at: https://preprod.cardanoscan.io/transaction/$TX_HASH"
    fi
  fi
}

# Add a wallet to a game
function wallet_add_to_game() {
  local WALLET_NAME=$1
  local GAME_NAME=$2
  
  if [ -z "$WALLET_NAME" ] || [ -z "$GAME_NAME" ]; then
    echo "Error: Both wallet name and game name are required."
    exit 1
  fi
  
  local WALLET_DIR="$WALLETS_DIR/$WALLET_NAME"
  local GAME_DIR="$GAMES_DIR/$GAME_NAME"
  
  if [ ! -d "$WALLET_DIR" ]; then
    echo "Error: Wallet '$WALLET_NAME' does not exist."
    exit 1
  fi
  
  if [ ! -d "$GAME_DIR" ]; then
    echo "Error: Game '$GAME_NAME' does not exist."
    exit 1
  fi
  
  # Load game config
  source $GAME_DIR/game.conf
  
  # Check if game is already deployed
  if [ "$STATUS" == "deployed" ]; then
    echo "Error: Game '$GAME_NAME' is already deployed. Cannot add wallets."
    exit 1
  fi
  
  # Check if wallet is already added to this game
  if [ -L "$GAME_DIR/wallets/$WALLET_NAME" ]; then
    echo "Error: Wallet '$WALLET_NAME' is already part of game '$GAME_NAME'."
    exit 1
  fi
  
  # Count existing wallets
  WALLET_COUNT=$(ls -1 $GAME_DIR/wallets/ 2>/dev/null | wc -l)
  
  # Determine max wallets based on difficulty
  case $DIFFICULTY in
    "Easy") MAX_WALLETS=3 ;;
    "Medium") MAX_WALLETS=5 ;;
    "Hard") MAX_WALLETS=8 ;;
    "Expert") MAX_WALLETS=12 ;;
    *) MAX_WALLETS=5 ;;
  esac
  
  if [ $WALLET_COUNT -ge $MAX_WALLETS ]; then
    echo "Error: Game '$GAME_NAME' already has the maximum number of wallets ($MAX_WALLETS)."
    exit 1
  fi
  
  # Choose wallet type
  if [ $WALLET_COUNT -eq 0 ]; then
    # First wallet must be the real one
    WALLET_TYPE="RealWallet"
    echo "This is the first wallet in the game and will be set as the REAL wallet."
  else
    WALLET_TYPE="DecoyWallet"
    echo "This wallet will be set as a DECOY wallet."
    
    # Ask for custom clue
    read -p "Enter a misleading clue for this decoy wallet: " CLUE
    if [ -z "$CLUE" ]; then
      CLUE="This might be the real wallet, who knows?"
    fi
    
    # Save clue
    echo "$CLUE" > "$WALLET_DIR/clue"
  fi
  
  # Save wallet type
  echo "$WALLET_TYPE" > "$WALLET_DIR/type"
  
  # Create symbolic link to wallet
  ln -s "$WALLET_DIR" "$GAME_DIR/wallets/$WALLET_NAME"
  
  echo "Wallet '$WALLET_NAME' added to game '$GAME_NAME' as $WALLET_TYPE."
  echo "Current wallet count: $(($WALLET_COUNT + 1))/$MAX_WALLETS"
}

# Deploy a game to the blockchain
function deploy_game() {
  local GAME_NAME=$1
  
  if [ -z "$GAME_NAME" ]; then
    echo "Error: Game name is required."
    exit 1
  fi
  
  local GAME_DIR="$GAMES_DIR/$GAME_NAME"
  local CONF_FILE="$GAME_DIR/game.conf"
  
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
  
  # Check if game is already deployed
  if [ "$STATUS" == "deployed" ]; then
    echo "Error: Game '$GAME_NAME' is already deployed."
    exit 1
  fi
  
  # Count wallets
  WALLET_COUNT=$(ls -1 $GAME_DIR/wallets/ 2>/dev/null | wc -l)
  
  # Determine required wallets based on difficulty
  case $DIFFICULTY in
    "Easy") REQUIRED_WALLETS=3 ;;
    "Medium") REQUIRED_WALLETS=5 ;;
    "Hard") REQUIRED_WALLETS=8 ;;
    "Expert") REQUIRED_WALLETS=12 ;;
    *) REQUIRED_WALLETS=5 ;;
  esac
  
  if [ $WALLET_COUNT -lt $REQUIRED_WALLETS ]; then
    echo "Error: Game '$GAME_NAME' requires $REQUIRED_WALLETS wallets, but only has $WALLET_COUNT."
    exit 1
  fi
  
  section "Deploying game: $GAME_NAME"
  
  # Choose a wallet for deployment (should have enough funds)
  echo "Select a wallet to pay for deployment costs:"
  list_wallets
  read -p "Enter wallet name: " DEPLOYER_WALLET
  
  local DEPLOYER_DIR="$WALLETS_DIR/$DEPLOYER_WALLET"
  local DEPLOYER_ADDR_FILE="$DEPLOYER_DIR/address"
  local DEPLOYER_SKEY="$DEPLOYER_DIR/payment.skey"
  
  if [ ! -d "$DEPLOYER_DIR" ] || [ ! -f "$DEPLOYER_ADDR_FILE" ] || [ ! -f "$DEPLOYER_SKEY" ]; then
    echo "Error: Invalid deployment wallet or missing key files."
    exit 1
  fi
  
  # Check deployer wallet balance
  local DEPLOYER_ADDRESS=$(cat $DEPLOYER_ADDR_FILE)
  local UTXO_DATA=$(cardano-cli query utxo \
    --address $DEPLOYER_ADDRESS \
    --testnet-magic $TESTNET_MAGIC \
    --out-file /dev/stdout)
  
  local LOVELACE_BALANCE=$(echo $UTXO_DATA | jq -r 'if type=="object" then to_entries | map(.value.value.lovelace) | add else 0 end' 2>/dev/null || echo "0")
  
  if [ $LOVELACE_BALANCE -lt 10000000 ]; then
    echo "Error: Deployment wallet has insufficient funds. Need at least 10 ADA."
    exit 1
  fi
  
  echo "Using wallet '$DEPLOYER_WALLET' for deployment."
  
  # Setup token policy
  ./scripts/deploy.sh "$DEPLOYER_WALLET"
  
  # After successful deployment, update game status
  if [ $? -eq 0 ]; then
    # Update game config
    sed -i 's/STATUS="created"/STATUS="deployed"/' $CONF_FILE
    
    # Copy deployment info
    cp "$OUTPUT_DIR/deployment-info.json" "$GAME_DIR/deployment-info.json"
    
    echo "Game '$GAME_NAME' deployed successfully!"
  else
    echo "Error: Deployment failed."
    exit 1
  fi
}

# Main command router
case $1 in
  setup)
    setup_environment
    ;;
  wallet)
    case $2 in
      create)
        create_wallet $3
        ;;
      list)
        list_wallets
        ;;
      balance)
        wallet_balance $3
        ;;
      add-to)
        wallet_add_to_game $3 $4
        ;;
      *)
        echo "Unknown wallet command: $2"
        show_help
        ;;
    esac
    ;;
  game)
    case $2 in
      create)
        create_game $3
        ;;
      deploy)
        deploy_game $3
        ;;
      status)
        game_status $3
        ;;
      list)
        list_games
        ;;
      *)
        echo "Unknown game command: $2"
        show_help
        ;;
    esac
    ;;
  help|*)
    show_help
    ;;
esac 