export default async function onPush(ctx, manifest, log) {
  const files = ctx.payload.commits.flatMap(c => c.added.concat(c.modified));
  const touchedManifest = files.some(f => f.endsWith("bots.manifest.json"));

  if (touchedManifest) {
    try {
      await ctx.octokit.repos.createCommitStatus({
        ...ctx.repo({ sha: ctx.payload.after }),
        state: "pending",
        context: "aln/manifest-validate",
        description: "Validating bots.manifest.json"
      });
      // If it loaded here, manifest is valid.
      await ctx.octokit.repos.createCommitStatus({
        ...ctx.repo({ sha: ctx.payload.after }),
        state: "success",
        context: "aln/manifest-validate",
        description: "Manifest validated"
      });
    } catch (e) {
      log.error({ err: e }, "Manifest status update failed");
    }
  }
}
