import fs from "node:fs";
import path from "node:path";
import Ajv from "ajv";

const manifestPath = path.resolve("config/bots.manifest.json");

export function loadManifest(log) {
  const raw = fs.readFileSync(manifestPath, "utf8");
  const manifest = JSON.parse(raw);

  const ajv = new Ajv({ allErrors: true });
  const schema = {
    type: "object",
    properties: {
      bots: { type: "array" },
      commands: { type: "array" },
      accessibilityTree: { type: "string" }
    },
    required: ["bots", "commands", "accessibilityTree"]
  };

  const validate = ajv.compile(schema);
  if (!validate(manifest)) {
    log.error({ errors: validate.errors }, "Invalid bots.manifest.json");
    throw new Error("Manifest validation failed");
  }
  return manifest;
}
