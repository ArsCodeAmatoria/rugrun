import crypto from 'crypto';

/**
 * Types that mirror our Haskell contract types
 */

export enum RugStatus {
  Unclaimed = 0,
  Claimed = 1
}

export enum WalletType {
  RealWallet = 0,
  DecoyWallet = 1
}

export interface RugDatum {
  rugSecretHash: string;  // SHA-256 hash of the secret phrase
  rugStatus: RugStatus;   // Current status (unclaimed/claimed)
  rugCreatorPkh: string;  // Public key hash of the creator
}

export interface RugRedeemer {
  secretPhrase: string;   // Secret phrase that should hash to the stored hash
}

export interface DecoyDatum {
  decoyClue: string;      // Fake clue to mislead users
  decoyBurnOnUse: boolean; // Whether to burn tokens on interaction
}

export interface DecoyRedeemer {
  decoyInput: string;     // Input value (will always fail)
}

export interface WalletData {
  id: string;
  address: string;
  type: WalletType;
  datum: RugDatum | DecoyDatum;
  balance: number;
  clue: string;
}

/**
 * Simulates the Haskell SHA-256 function
 * @param input - Input string to hash
 * @returns Hexadecimal representation of SHA-256 hash
 */
export function sha256(input: string): string {
  return crypto.createHash('sha256').update(input).digest('hex');
}

/**
 * Simulates the Haskell verifySecret function
 * @param secretHash - The expected hash
 * @param providedSecret - The secret to verify
 * @returns Boolean indicating if the provided secret hashes to the expected hash
 */
export function verifySecret(secretHash: string, providedSecret: string): boolean {
  return sha256(providedSecret) === secretHash;
}

/**
 * Simulates the mkRugWalletValidator Haskell function to validate an attempt to claim the rug wallet
 * @param datum - The wallet's datum
 * @param redeemer - The redeemer with the claimed secret
 * @returns Result object with success status and message
 */
export function validateRugWallet(datum: RugDatum, redeemer: RugRedeemer): { success: boolean; message: string } {
  // Verify if the wallet is already claimed
  if (datum.rugStatus === RugStatus.Claimed) {
    return { success: false, message: "Wallet already claimed" };
  }
  
  // Verify if the hash matches
  if (!verifySecret(datum.rugSecretHash, redeemer.secretPhrase)) {
    return { success: false, message: "Invalid secret phrase" };
  }
  
  // Success!
  return { 
    success: true, 
    message: "Congratulations! You've successfully pulled the rug and claimed the funds!"
  };
}

/**
 * Simulates the mkDecoyWalletValidator Haskell function which always fails
 * @param datum - The decoy wallet's datum
 * @param redeemer - The redeemer for the attempt
 * @returns Result object always with success=false and an appropriate message
 */
export function validateDecoyWallet(datum: DecoyDatum, redeemer: DecoyRedeemer): { success: boolean; message: string } {
  if (datum.decoyBurnOnUse) {
    return { success: false, message: "Tokens burned due to interaction" };
  } else {
    return { success: false, message: "Invalid attempt to access decoy wallet" };
  }
}

/**
 * Creates a new wallet with specified parameters
 * @param type - Whether this is a real or decoy wallet
 * @param balance - The token balance of the wallet
 * @param clue - A clue hint for players
 * @param secretOrHash - For real wallets, the secret phrase hash; for decoys, the clue text
 * @returns A new wallet object
 */
export function createWallet(
  type: WalletType, 
  balance: number, 
  clue: string, 
  secretOrHash: string
): WalletData {
  const id = crypto.randomUUID();
  const address = `addr_${crypto.randomBytes(16).toString('hex')}`;
  
  if (type === WalletType.RealWallet) {
    const datum: RugDatum = {
      rugSecretHash: secretOrHash,
      rugStatus: RugStatus.Unclaimed,
      rugCreatorPkh: 'creator_' + crypto.randomBytes(8).toString('hex')
    };
    
    return {
      id,
      address,
      type,
      datum,
      balance,
      clue
    };
  } else {
    const datum: DecoyDatum = {
      decoyClue: secretOrHash,
      decoyBurnOnUse: Math.random() > 0.5 // Randomly decide if this decoy burns tokens
    };
    
    return {
      id,
      address,
      type,
      datum,
      balance,
      clue
    };
  }
}

/**
 * Attempt to unlock a wallet with a secret phrase
 * @param wallet - The wallet to unlock
 * @param secretPhrase - The provided secret phrase
 * @returns Result object with success status and message
 */
export function attemptUnlock(wallet: WalletData, secretPhrase: string): { success: boolean; message: string } {
  if (wallet.type === WalletType.RealWallet) {
    const datum = wallet.datum as RugDatum;
    const redeemer: RugRedeemer = { secretPhrase };
    return validateRugWallet(datum, redeemer);
  } else {
    const datum = wallet.datum as DecoyDatum;
    const redeemer: DecoyRedeemer = { decoyInput: secretPhrase };
    return validateDecoyWallet(datum, redeemer);
  }
}

/**
 * Generate a set of wallets for the game, with one real wallet and multiple decoys
 * @param count - Total number of wallets to generate
 * @param totalBalance - Total balance to distribute among wallets
 * @param secret - The secret phrase for the real wallet
 * @returns Array of wallet objects
 */
export function generateGameWallets(count: number, totalBalance: number, secret: string): WalletData[] {
  const wallets: WalletData[] = [];
  const realWalletIndex = Math.floor(Math.random() * count);
  const secretHash = sha256(secret);
  
  // Create clues that hint but don't reveal too much
  const clues = [
    "The first clue is hidden in plain sight, but who am I kidding? This is just a decoy.",
    "Look to the source code, it reveals the next address, but the real one? Only in your dreams.",
    "Three times the charm, but the rug is always pulled. Or maybe this is the real one?",
    "Decoys everywhere, but don't be discouraged. The fourth key is rarely the correct one.",
    "The final test, but is it though? Follow the rabbit hole deeper."
  ];
  
  // Distribute balance - give the real wallet about 50% of total
  const realWalletBalance = Math.floor(totalBalance * 0.5);
  const decoyBalance = totalBalance - realWalletBalance;
  const avgDecoyBalance = Math.floor(decoyBalance / (count - 1));
  
  for (let i = 0; i < count; i++) {
    if (i === realWalletIndex) {
      // Create the real wallet
      const realWallet = createWallet(
        WalletType.RealWallet,
        realWalletBalance,
        "This wallet seems different. The clue might be meaningful: 'Haskell rocks!'",
        secretHash
      );
      wallets.push(realWallet);
    } else {
      // Create a decoy wallet
      const decoyBalance = avgDecoyBalance + Math.floor(Math.random() * 500) - 250; // Add some variance
      const clueIndex = i % clues.length;
      const decoyWallet = createWallet(
        WalletType.DecoyWallet,
        decoyBalance > 0 ? decoyBalance : avgDecoyBalance, // Ensure positive balance
        clues[clueIndex],
        `Decoy clue ${i}: Try harder!`
      );
      wallets.push(decoyWallet);
    }
  }
  
  return wallets;
} 