import onIssueComment from "./events/onIssueComment.js";
import onPullRequest from "./events/onPullRequest.js";
import onPush from "./events/onPush.js";
import { loadManifest } from "./loaders/manifestLoader.js";

export default (app) => {
  const manifest = loadManifest(app.log);

  app.on("issue_comment.created", async (ctx) =>
    onIssueComment(ctx, manifest, app.log)
  );
  app.on(["pull_request.opened", "pull_request.synchronize"], async (ctx) =>
    onPullRequest(ctx, manifest, app.log)
  );
  app.on("push", async (ctx) =>
    onPush(ctx, manifest, app.log)
  );

  app.log.info({
    bots: manifest.bots.map(b => b.id),
    commands: manifest.commands.map(c => c.id)
  }, "github.aln.bots ready");
};
