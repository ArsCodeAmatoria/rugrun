export interface LeaderboardEntry {
  id: string;
  address: string;
  attempts: number;
  lastAttempt: number;
  successfulAttempts: number;
}

// Temporarily store leaderboard entries in memory until database is set up
export const leaderboard: LeaderboardEntry[] = [
  {
    id: '1',
    address: 'addr1qyk0rqp265fdyqd97lh2w23jtmxn4wlg9xudy8pjysd5wdqa95xx0k9fty93qqsm5mqsf2aynvqn2nh3gfq5aq3d0spqx9rwrg',
    attempts: 15,
    lastAttempt: 1629123556789,
    successfulAttempts: 0
  },
  {
    id: '2',
    address: 'addr1qyw7a7yj2x5r9sazvhqz7ker4mz29g7jg8c0lpmdcxc6gkvs7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8q4mk',
    attempts: 8,
    lastAttempt: 1629123656789,
    successfulAttempts: 0
  },
  {
    id: '3',
    address: 'addr1qym7r4dzwh60suwk9zqct6g2yx5c57tl0pxa0r79ldc7ltys7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8xfqz',
    attempts: 22,
    lastAttempt: 1629123756789,
    successfulAttempts: 0
  },
  {
    id: '4',
    address: 'addr1qy5vhjrz5c92qf3kpq6868mxpkzsknfrxy9n9r0g8rz7pk9s7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs52xqy',
    attempts: 5,
    lastAttempt: 1629123456789,
    successfulAttempts: 0
  },
  {
    id: '5',
    address: 'addr1qy2cpat8mqe0emsytj4g6qhpqdrjxgfcnkg2kq4cwsmqkxvs7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs923vz',
    attempts: 11,
    lastAttempt: 1629123356789,
    successfulAttempts: 0
  }
]; 