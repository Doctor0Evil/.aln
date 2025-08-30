import { randInt } from "../utils/random.js";
import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const sides = Number.isInteger(params?.sides) ? params.sides : 20;
  if (sides < 2) {
    return ctx.octokit.reactions.createForIssueComment({
      ...ctx.repo(),
      comment_id: ctx.payload.comment.id,
      content: "-1"
    });
  }
  const roll = randInt(sides) + 1;
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🎲 D${sides} rolled: ${codeFence(String(roll))}`
  });
}
