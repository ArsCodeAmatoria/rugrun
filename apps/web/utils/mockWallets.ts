export type Wallet = {
  address: string;
  isReal: boolean;
  clue: string;
  lastAttempt?: {
    address: string;
    timestamp: number;
    status: 'failed' | 'pending' | 'success';
  }
};

export const wallets: Wallet[] = [
  // The real wallet (rug wallet)
  {
    address: "addr1qx8z0j76x5ylc6xh3dk4r5m82f8xtslzfs9slnefyg0fjdnvjdz48zg42xk4hmayuuhqynasjvq8h3qcxwxleh6e95rsagfjwx",
    isReal: true,
    clue: "The truth hides in plain sight. Look to the stars for guidance."
  },
  // Decoy wallets
  {
    address: "addr1qxg9j2p036un4cgwp6ffdfqhf4p0kgaadpvv5xlprjqrjtxr8tzs2u9s956h5jc6lts53qkev2qece7a30zlm5d9rd2qf223dn",
    isReal: false,
    clue: "Time is the key. Find where the seconds meet the minutes in cosmic alignment."
  },
  {
    address: "addr1qy2wd7e3xmk9hyy8r9j0cryjnz7xk3htklzu7wqnha09d3q7qsneqvqgvjupc4hf2fd9e3yf2wkh3zny69wqtgu5urssg80q2l",
    isReal: false,
    clue: "The answer is 42, but what's the question?"
  },
  {
    address: "addr1q80kqyn2jes7fcqvcaa3mz8ydhjg93rdm5w6z78tnmejk58nkkxhkvrsm7kxy84tqfk0mgtfpvd2j2ycas3fqp5nzc4q6wztmw",
    isReal: false,
    clue: "Prime numbers hold the secret. Find the largest one and decrypt with it."
  },
  {
    address: "addr1qxc3wy6t9vlw3uy4tfpwj675w8fxh7u8u0gy5t8a9m5ntzrth6sxw9vl3v0fs2lw5tkfzleaghm9h57vus2jcgpvq5qf7jtcx",
    isReal: false,
    clue: "The blockchain is a labyrinth. Only those who understand the path can find the exit."
  },
  {
    address: "addr1qxs7lk43320fwfmvrgc3xqz9qh7h9zchstc3hrktnwhx0vwvp4l9x5lfacezln8xmzy2k4rmcejg8fz45t05lkv0zjvswt5lqh",
    isReal: false,
    clue: "The key is hidden in the transaction history. Trace the pattern to find the truth."
  },
  {
    address: "addr1q9umpzjk95gll3293u2c0qd0z0h4m9hckhwvn76v6v60d6l5nkdw67l70qm7xrwhjqp93u64js4m77awgklzyj9kzueszgdjve",
    isReal: false,
    clue: "Look for the wallet with exactly 777 transactions. That's not this one."
  }
];

export const attemptHistory = [
  {
    walletAddress: "addr1qxg9j2p036un4cgwp6ffdfqhf4p0kgaadpvv5xlprjqrjtxr8tzs2u9s956h5jc6lts53qkev2qece7a30zlm5d9rd2qf223dn",
    userAddress: "addr1q9e3ehl24tgp0f4yh4smw9ugknlcpq96p7dzmpvvka9tlq5h34nf6l0pkyhj9uk8bsdmkh0fzkpzk3mi530lrntf9tsq909mpw",
    timestamp: Date.now() - 900000,
    attempt: "cosmic_time",
    status: "failed" as const
  },
  {
    walletAddress: "addr1q80kqyn2jes7fcqvcaa3mz8ydhjg93rdm5w6z78tnmejk58nkkxhkvrsm7kxy84tqfk0mgtfpvd2j2ycas3fqp5nzc4q6wztmw",
    userAddress: "addr1qx7z5jhed3p722lj50xrcd2hj53qxwv2m6kxjaqdekaz942lmyrve9kyfx75h2e9hnr598wkgwx06phs6a0mnpkzml5sv0cg3w",
    timestamp: Date.now() - 360000,
    attempt: "1459",
    status: "failed" as const
  },
  {
    walletAddress: "addr1qxs7lk43320fwfmvrgc3xqz9qh7h9zchstc3hrktnwhx0vwvp4l9x5lfacezln8xmzy2k4rmcejg8fz45t05lkv0zjvswt5lqh",
    userAddress: "addr1q8ek5gwth63m6xsrcpqvz82k9u8qldld05y9l6y8tal5x6vftxkdcny98mcavk8lrz6alsekdynapksrndvus0xrnzqsq63c43",
    timestamp: Date.now() - 120000,
    attempt: "txpattern_123",
    status: "failed" as const
  }
];

export const leaderboard = [
  {
    address: "addr1q9e3ehl24tgp0f4yh4smw9ugknlcpq96p7dzmpvvka9tlq5h34nf6l0pkyhj9uk8bsdmkh0fzkpzk3mi530lrntf9tsq909mpw",
    attempts: 12,
    lastAttempt: Date.now() - 900000
  },
  {
    address: "addr1qx7z5jhed3p722lj50xrcd2hj53qxwv2m6kxjaqdekaz942lmyrve9kyfx75h2e9hnr598wkgwx06phs6a0mnpkzml5sv0cg3w",
    attempts: 8,
    lastAttempt: Date.now() - 360000
  },
  {
    address: "addr1q8ek5gwth63m6xsrcpqvz82k9u8qldld05y9l6y8tal5x6vftxkdcny98mcavk8lrz6alsekdynapksrndvus0xrnzqsq63c43",
    attempts: 5,
    lastAttempt: Date.now() - 120000
  }
]; 