import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const letter = (params?.letter || "").toUpperCase();
  if (!letter || !/^[A-Z]$/.test(letter)) {
    return ctx.octokit.issues.createComment({
      ...ctx.issue(),
      body: "Provide a single letter: `/aln.game.hangman { \"letter\": \"A\" }`"
    });
  }
  // Stateless demo: in real use, persist state to a discussion or issue body checklist
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🔤 Hangman guess: ${codeFence(letter)}`
  });
}
