# RugRun API

Backend service for the RugRun blockchain Capture The Flag game.

## Overview

This API provides endpoints to interact with wallet data, track unlock attempts, and manage the leaderboard for RugRun - a blockchain-based CTF game where players hunt for the real wallet among decoys.

## Features

- Wallet retrieval and management
- Secret phrase unlock attempts
- Leaderboard tracking
- User statistics

## API Endpoints

### Wallets

- `GET /api/wallets` - Get all wallets
- `GET /api/wallets/:address` - Get wallet by address
- `POST /api/wallets/:address/unlock` - Attempt to unlock a wallet
- `POST /api/wallets` - Add a new wallet (admin only)

### Attempts

- `GET /api/attempts` - Get all attempts (can filter by wallet or user)
- `POST /api/attempts` - Create a new unlock attempt

### Leaderboard

- `GET /api/leaderboard` - Get leaderboard (can sort and limit)
- `GET /api/leaderboard/stats` - Get leaderboard statistics
- `GET /api/leaderboard/user/:address` - Get a user's rank

## Development

### Setup

```bash
# Install dependencies
npm install

# Create .env file (copy from .env.example)
cp .env.example .env
```

### Running the API

```bash
# Development with hot reload
npm run dev

# Build for production
npm run build

# Start production server
npm start
```

### Future Implementations

- Database integration (PostgreSQL/MongoDB)
- Authentication with JWT
- Actual Cardano blockchain integration
- Smart contract interactions via Blockfrost API
- Redis for caching 