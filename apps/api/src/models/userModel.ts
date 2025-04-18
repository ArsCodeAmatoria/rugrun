export interface User {
  id: string;
  address: string;
  username?: string;
  email?: string;
  points: number;
  wallets: string[]; // Array of wallet addresses owned by this user
  lastLogin?: Date;
  createdAt: Date;
  updatedAt: Date;
}

// Temporarily store users in memory until database is set up
export const users: User[] = [
  {
    id: '1',
    address: 'addr1qyk0rqp265fdyqd97lh2w23jtmxn4wlg9xudy8pjysd5wdqa95xx0k9fty93qqsm5mqsf2aynvqn2nh3gfq5aq3d0spqx9rwrg',
    username: 'crypto_hunter1',
    points: 1200,
    wallets: [],
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '2',
    address: 'addr1qyw7a7yj2x5r9sazvhqz7ker4mz29g7jg8c0lpmdcxc6gkvs7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8q4mk',
    username: 'blockchain_explorer',
    points: 850,
    wallets: [],
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '3',
    address: 'addr1qym7r4dzwh60suwk9zqct6g2yx5c57tl0pxa0r79ldc7ltys7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8xfqz',
    username: 'token_master',
    points: 2500,
    wallets: [],
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '4',
    address: 'addr1qy5vhjrz5c92qf3kpq6868mxpkzsknfrxy9n9r0g8rz7pk9s7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs52xqy',
    username: 'wallet_detective',
    points: 500,
    wallets: [],
    createdAt: new Date(),
    updatedAt: new Date()
  },
  {
    id: '5',
    address: 'addr1qy2cpat8mqe0emsytj4g6qhpqdrjxgfcnkg2kq4cwsmqkxvs7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs923vz',
    username: 'haskell_fan',
    points: 1000,
    wallets: [],
    createdAt: new Date(),
    updatedAt: new Date()
  }
]; 