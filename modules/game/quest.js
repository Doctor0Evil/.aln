export default function quest({ action, location }) {
  const actions = {
    explore: `You venture into the ${location} and discover a hidden chamber...`,
    fight: `You engage in a battle at the ${location}, weapons clashing.`,
    trade: `In the ${location}, you meet a merchant with rare goods.`
  };
  return actions[action] || `Unknown action: ${action}`;
}
