
const MAX_ATTEMPTS = 6;
export default function hangman({ letter, state }) {
  // 'state' carries current word, guessed letters, and attempt count
  const normalized = letter.toUpperCase();
  if (state.word.includes(normalized)) {
    state.guessed.push(normalized);
  } else {
    state.attempts++;
  }
  return state;
}
