export default function rollDice({ sides = 6 }) {
  if (!Number.isInteger(sides) || sides < 2) {
    throw new Error(`Invalid dice sides: ${sides}`);
  }
  return Math.floor(Math.random() * sides) + 1;
}
