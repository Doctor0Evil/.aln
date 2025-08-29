export default async function onPullRequest(ctx, manifest, log) {
  const pr = ctx.payload.pull_request;
  const labels = [];

  if (pr.changed_files >= 20) labels.push("size/XL");
  else if (pr.changed_files >= 10) labels.push("size/L");
  else if (pr.changed_files >= 5) labels.push("size/M");
  else labels.push("size/S");

  try {
    if (labels.length) {
      await ctx.octokit.issues.addLabels({
        ...ctx.repo(),
        issue_number: pr.number,
        labels
      });
    }

    // Gentle checklist
    const body = [
      "🔍 PR checklist:",
      "- [ ] Tests or validation updated",
      "- [ ] Manifest entries updated (if adding commands/tools)",
      "- [ ] Docs synced (`/utility.syncTree` to preview accessibility tree)"
    ].join("\n");

    await ctx.octokit.issues.createComment({ ...ctx.issue(), body });
  } catch (e) {
    log.error({ err: e }, "PR labeling/checklist failed");
  }
}
