export interface Attempt {
  id: string;
  walletAddress: string;
  userAddress: string;
  timestamp: number;
  attempt: string; // The phrase/key they tried
  status: 'failed' | 'success' | 'pending';
}

// Temporarily store attempts in memory until database is set up
export const attempts: Attempt[] = [
  {
    id: '1',
    walletAddress: 'addr1qym5htxrsdpjq60kk5cgg8gxxgj6m3r4pqk4xenh4myt3fc08cgvsj4xvwq66n4u46a2jzyzn6jndu8z3qhcn8v26raqrrtu78',
    userAddress: 'addr1qyk0rqp265fdyqd97lh2w23jtmxn4wlg9xudy8pjysd5wdqa95xx0k9fty93qqsm5mqsf2aynvqn2nh3gfq5aq3d0spqx9rwrg',
    timestamp: 1629123456789,
    attempt: 'rugpull',
    status: 'failed'
  },
  {
    id: '2',
    walletAddress: 'addr1qym5htxrsdpjq60kk5cgg8gxxgj6m3r4pqk4xenh4myt3fc08cgvsj4xvwq66n4u46a2jzyzn6jndu8z3qhcn8v26raqrrtu78',
    userAddress: 'addr1qyk0rqp265fdyqd97lh2w23jtmxn4wlg9xudy8pjysd5wdqa95xx0k9fty93qqsm5mqsf2aynvqn2nh3gfq5aq3d0spqx9rwrg',
    timestamp: 1629123556789,
    attempt: 'cardano',
    status: 'failed'
  },
  {
    id: '3',
    walletAddress: 'addr1qxzf8jqha56nxzupe7z9zjsj6vwt2z02v5zp0rpt94wl0dl47xny94ypchyf7ph2ml4sakawd9k58qdd4axnxapkjgq08tgyj',
    userAddress: 'addr1qyw7a7yj2x5r9sazvhqz7ker4mz29g7jg8c0lpmdcxc6gkvs7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8q4mk',
    timestamp: 1629123656789,
    attempt: 'plutus',
    status: 'failed'
  },
  {
    id: '4',
    walletAddress: 'addr1q8ksew9vhyfuxwalh2n37cjrclvqkk7m3axpun92mmahpn9v4e3tvzt2lygg87v2d9xl4daj9cfrwxthlxm3wh4q43vqx3n7k5',
    userAddress: 'addr1qym7r4dzwh60suwk9zqct6g2yx5c57tl0pxa0r79ldc7ltys7j9xnwpvrzkkg7vvtyt4u0gsmzpu7vavf6e0ms3vuwaqs8xfqz',
    timestamp: 1629123756789,
    attempt: 'wrong_phrase',
    status: 'failed'
  }
]; 