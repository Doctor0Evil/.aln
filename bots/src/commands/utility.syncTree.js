import fs from "node:fs";
import path from "node:path";

export default async function handle(ctx) {
  const treePath = path.resolve("config/accessibility.tree.json");
  const content = fs.readFileSync(treePath, "utf8");
  const body = [
    "♿ Accessibility Tree sync:",
    "```json",
    content,
    "```"
  ].join("\n");

  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body
  });
}
