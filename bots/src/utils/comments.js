export async function reply(ctx, body) {
  const issue = ctx.issue();
  return ctx.octokit.issues.createComment({ ...issue, body });
}

export function codeFence(content, lang = "") {
  return "```" + lang + "\n" + content + "\n```";
}
