import { 
  WalletData, 
  generateGameWallets, 
  attemptUnlock, 
  RugStatus,
  WalletType,
  verifySecret
} from '@/utils/haskellSimulation';

// Local storage keys for persisting game state
const WALLETS_STORAGE_KEY = 'rugrun_wallets';
const USER_ATTEMPTS_KEY = 'rugrun_user_attempts';
const SECRET_KEY = 'rugrun_real_secret';

export interface AttemptRecord {
  walletAddress: string;
  attempt: string;
  timestamp: number;
  success: boolean;
  message: string;
}

export class GameService {
  private wallets: WalletData[] = [];
  private userAttempts: AttemptRecord[] = [];
  private secret: string = 'haskellftw'; // Default secret phrase

  constructor() {
    this.loadGameState();
  }

  /**
   * Initialize or reset the game with new wallets
   * @param walletCount Number of wallets to generate
   * @param totalBalance Total balance to distribute
   * @param secret Secret for the real wallet (optional)
   */
  public initializeGame(walletCount = 5, totalBalance = 50000, secret?: string): void {
    // Use provided secret or generate a new one
    this.secret = secret || this.secret;
    
    // Generate new wallets
    this.wallets = generateGameWallets(walletCount, totalBalance, this.secret);
    
    // Reset user attempts
    this.userAttempts = [];
    
    // Save to local storage
    this.saveGameState();
  }

  /**
   * Get all wallet data (excluding sensitive information)
   */
  public getWallets(): Omit<WalletData, 'datum' | 'type'>[] {
    return this.wallets.map(wallet => {
      // Only return safe data
      const { datum, type, ...safeWallet } = wallet;
      return safeWallet;
    });
  }

  /**
   * Get details of a specific wallet by address
   * @param address Wallet address to retrieve
   */
  public getWalletByAddress(address: string): Omit<WalletData, 'datum' | 'type'> | null {
    const wallet = this.wallets.find(w => w.address === address);
    
    if (!wallet) {
      return null;
    }
    
    // Return only safe data
    const { datum, type, ...safeWallet } = wallet;
    return safeWallet;
  }

  /**
   * Get all user attempts
   */
  public getUserAttempts(): AttemptRecord[] {
    return [...this.userAttempts];
  }

  /**
   * Get attempts for a specific wallet
   * @param walletAddress Wallet address to get attempts for
   */
  public getWalletAttempts(walletAddress: string): AttemptRecord[] {
    return this.userAttempts.filter(a => a.walletAddress === walletAddress);
  }

  /**
   * Attempt to unlock a wallet with a secret phrase
   * @param walletAddress Wallet address to unlock
   * @param secretPhrase Secret phrase to try
   * @returns Result of the attempt
   */
  public attemptWalletUnlock(walletAddress: string, secretPhrase: string): AttemptRecord {
    // Find the wallet
    const walletIndex = this.wallets.findIndex(w => w.address === walletAddress);
    
    if (walletIndex === -1) {
      const failedAttempt: AttemptRecord = {
        walletAddress,
        attempt: secretPhrase,
        timestamp: Date.now(),
        success: false,
        message: "Wallet not found"
      };
      
      this.userAttempts.push(failedAttempt);
      this.saveGameState();
      return failedAttempt;
    }
    
    // Get the wallet and attempt unlock
    const wallet = this.wallets[walletIndex];
    const result = attemptUnlock(wallet, secretPhrase);
    
    // Record the attempt
    const attemptRecord: AttemptRecord = {
      walletAddress,
      attempt: secretPhrase,
      timestamp: Date.now(),
      success: result.success,
      message: result.message
    };
    
    this.userAttempts.push(attemptRecord);
    
    // If successful, update the wallet status
    if (result.success && wallet.type === WalletType.RealWallet) {
      const rugDatum = wallet.datum as any; // Type assertion needed here
      rugDatum.rugStatus = RugStatus.Claimed;
      this.wallets[walletIndex] = {
        ...wallet,
        datum: rugDatum
      };
    }
    
    this.saveGameState();
    return attemptRecord;
  }

  /**
   * Get the current game status
   */
  public getGameStatus(): {
    totalWallets: number;
    realWalletFound: boolean;
    totalAttempts: number;
    successfulAttempts: number;
  } {
    const realWallet = this.wallets.find(w => w.type === WalletType.RealWallet);
    const realWalletFound = realWallet ? (realWallet.datum as any).rugStatus === RugStatus.Claimed : false;
    
    return {
      totalWallets: this.wallets.length,
      realWalletFound,
      totalAttempts: this.userAttempts.length,
      successfulAttempts: this.userAttempts.filter(a => a.success).length
    };
  }

  /**
   * Debug method: Check if a given secret matches the real wallet's secret
   * @param secretToCheck - Secret to check against the real wallet
   * @returns Whether the secret is correct
   */
  public checkSecretCorrectness(secretToCheck: string): boolean {
    const realWallet = this.wallets.find(w => w.type === WalletType.RealWallet);
    if (!realWallet) {
      return false;
    }
    
    const rugDatum = realWallet.datum as any;
    return verifySecret(rugDatum.rugSecretHash, secretToCheck);
  }

  /**
   * Get the real wallet's address for debugging
   * @returns The address of the real wallet
   */
  public getRealWalletAddress(): string | null {
    const realWallet = this.wallets.find(w => w.type === WalletType.RealWallet);
    return realWallet ? realWallet.address : null;
  }

  /**
   * Load game state from local storage
   */
  private loadGameState(): void {
    if (typeof window === 'undefined') {
      return; // Skip in SSR context
    }
    
    try {
      // Load wallets
      const walletsJson = localStorage.getItem(WALLETS_STORAGE_KEY);
      if (walletsJson) {
        this.wallets = JSON.parse(walletsJson);
      } else {
        // Initialize with default values if not found
        this.initializeGame();
        return;
      }
      
      // Load attempts
      const attemptsJson = localStorage.getItem(USER_ATTEMPTS_KEY);
      if (attemptsJson) {
        this.userAttempts = JSON.parse(attemptsJson);
      }
      
      // Load secret
      const secret = localStorage.getItem(SECRET_KEY);
      if (secret) {
        this.secret = secret;
      }
    } catch (error) {
      console.error('Error loading game state:', error);
      // Initialize with default values on error
      this.initializeGame();
    }
  }

  /**
   * Save game state to local storage
   */
  private saveGameState(): void {
    if (typeof window === 'undefined') {
      return; // Skip in SSR context
    }
    
    try {
      localStorage.setItem(WALLETS_STORAGE_KEY, JSON.stringify(this.wallets));
      localStorage.setItem(USER_ATTEMPTS_KEY, JSON.stringify(this.userAttempts));
      localStorage.setItem(SECRET_KEY, this.secret);
    } catch (error) {
      console.error('Error saving game state:', error);
    }
  }
} 