import { parseCommand } from "../utils/parseCommand.js";
import { getHandler } from "../commands/dispatcher.js";
import { isOrgAllowed } from "../loaders/security.js";

export default async function onIssueComment(ctx, manifest, log) {
  if (!isOrgAllowed(ctx)) return;

  const { body } = ctx.payload.comment;
  const parsed = parseCommand(body);
  if (!parsed) return;

  const cmd = manifest.commands.find(c => c.id === parsed.cmd);
  if (!cmd) {
    await ctx.octokit.reactions.createForIssueComment({
      ...ctx.repo(),
      comment_id: ctx.payload.comment.id,
      content: "confused"
    });
    return;
  }

  const handler = getHandler(cmd.id);
  if (!handler) {
    log.warn({ cmd: cmd.id }, "Handler not found");
    return;
  }

  log.info({ cmd: cmd.id, params: parsed.params }, "Executing command");
  try {
    await handler(ctx, parsed.params);
  } catch (e) {
    log.error({ err: e, cmd: cmd.id }, "Command execution failed");
    await ctx.octokit.issues.createComment({
      ...ctx.issue(),
      body: `⚠️ Command failed: ${e.message}`
    });
  }
}
