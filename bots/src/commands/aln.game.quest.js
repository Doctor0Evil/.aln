import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const action = (params?.action || "explore").toLowerCase();
  const location = params?.location || "cave";
  const lines = {
    explore: `You venture into the ${location} and discover a hidden chamber lit by bioluminescent moss.`,
    fight: `Steel clashes in the ${location}. You parry, riposte, and seize the opening.`,
    trade: `A quiet stall in the ${location}. One relic hums softly, as if alive.`
  };
  const out = lines[action] || `Unknown action: ${action}`;
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🧭 Quest: ${codeFence(out)}`
  });
}
