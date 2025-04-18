export interface Wallet {
  id: string;
  address: string;
  clue: string;
  isReal: boolean;
  secretPhrase?: string; // Only present for real wallet and secured
  balance?: number;
  createdAt: Date;
  updatedAt: Date;
}

// Temporarily store wallets in memory until database is set up
export const wallets: Wallet[] = [
  {
    id: '1',
    address: 'addr1qym5htxrsdpjq60kk5cgg8gxxgj6m3r4pqk4xenh4myt3fc08cgvsj4xvwq66n4u46a2jzyzn6jndu8z3qhcn8v26raqrrtu78',
    clue: 'The first clue is hidden in plain sight, but who am I kidding? This is just a decoy.',
    isReal: false,
    balance: 1000,
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '2',
    address: 'addr1qxzf8jqha56nxzupe7z9zjsj6vwt2z02v5zp0rpt94wl0dl47xny94ypchyf7ph2ml4sakawd9k58qdd4axnxapkjgq08tgyj',
    clue: 'Look to the source code, it reveals the next address, but the real one? Only in your dreams.',
    isReal: false,
    balance: 500,
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '3',
    address: 'addr1q8ksew9vhyfuxwalh2n37cjrclvqkk7m3axpun92mmahpn9v4e3tvzt2lygg87v2d9xl4daj9cfrwxthlxm3wh4q43vqx3n7k5',
    clue: 'Three times the charm, but the rug is always pulled. Or maybe this is the real one?',
    isReal: true,
    secretPhrase: 'haskellftw', // This would be securely stored in a real app
    balance: 50000,
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '4',
    address: 'addr1qy4g9pq0k7xvjm0axnvpyy269a29pe6jdz0umvc6ulm9lv5l6ve8s7xqw6qpdeqe94zn7guwtl3p47nklatkh767g7qssgj9xn',
    clue: 'Decoys everywhere, but don\'t be discouraged. The fourth key is rarely the correct one.',
    isReal: false,
    balance: 1200,
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '5',
    address: 'addr1q9kh2v8402k36ecp8a8dugjlf8xghsv8fzwdtlaqnzuv5gj2evsrp0szr89qfpxpcz47ryfntmhxrj4pnhaqtzwh67zqpz65cw',
    clue: 'The final test, but is it though? Follow the rabbit hole deeper.',
    isReal: false,
    balance: 800,
    createdAt: new Date(),
    updatedAt: new Date()
  }
]; 