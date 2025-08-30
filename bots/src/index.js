import { Probot } from "probot";
import appFn from "./app.js";

const port = process.env.PORT || 3000;

async function main() {
  const probot = new Probot({
    appId: process.env.ALN_APP_ID,
    privateKey: process.env.ALN_PRIVATE_KEY?.replace(/\\n/g, "\n"),
    secret: process.env.ALN_WEBHOOK_SECRET,
    logLevel: process.env.ALN_LOG_LEVEL || "info"
  });

  await probot.load(appFn);
  const server = await probot.httpServer({ port });
  // Graceful shutdown
  process.on("SIGTERM", () => server.close(() => process.exit(0)));
  process.on("SIGINT", () => server.close(() => process.exit(0)));
}

main().catch(err => {
  // eslint-disable-next-line no-console
  console.error("Fatal startup error:", err);
  process.exit(1);
});
