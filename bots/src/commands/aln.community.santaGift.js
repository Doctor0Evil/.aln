export default async function handle(ctx, params) {
  const target = params?.target || "@everyone";
  const gift = params?.gift || "Mystery Box";
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🎁 Santa has delivered "${gift}" to ${target}!`
  });
}
