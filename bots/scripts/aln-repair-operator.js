#!/usr/bin/env node
/**
 * ALN Repair Operator (Max Profanity Compliance Edition)
 *
 * Goals:
 *  - Maintain maximum allowable profanity in "banter" mode
 *  - Enforce safety/compliance boundaries
 *  - Self-heal .yml, .yaml, and .aln files for hygiene + policy
 *  - Remain idempotent: no dupes, safe repeat runs
 *  - Prepare files for graceful integration into workflow chain
 */

import fs from "node:fs";

const [, , filePath, adaptArg] = process.argv;
if (!filePath) {
  console.error("Usage: aln-repair-operator.js <file> [--adapt=fun.adapt=evolve.real.aln]");
  process.exit(1);
}

console.log(`🔍 Processing ${filePath} with ${adaptArg || "(default mode)"}`);

try {
  let content = fs.readFileSync(filePath, "utf8");
  const original = content;
  let changed = false;

  // === Hygiene fixes ===
  if (!content.endsWith("\n")) {
    content += "\n"; changed = true;
    console.log("↩️ Added trailing newline");
  }
  if (/\.(ya?ml)$/i.test(filePath) && /\t/.test(content)) {
    content = content.replace(/\t/g, "  "); changed = true;
    console.log("↩️ Converted tabs to spaces in YAML");
  }
  const trimmed = content.replace(/[ \t]+(\r?\n)/g, "$1");
  if (trimmed !== content) {
    content = trimmed; changed = true;
    console.log("↩️ Stripped trailing spaces");
  }

  // === ALN operator header ===
  if (/\.aln$/i.test(filePath) && !/^operator:/m.test(content)) {
    content = `operator: evolve.real.aln\n${content}`;
    changed = true;
    console.log("➕ Added default operator header to ALN file");
  }

  // === Profanity policy block injection ===
  const profanityPolicyBlock = [
    "profanity_policy:",
    "  mode: banter",
    "  allow:",
    "    - damn",
    "    - hell",
    "    - shit",
    "    - fuck",
    "    - bloody",
    "    - bollocks",
    "  deny:",
    "    - regex: \"(?i)\\\\b(racist|homophobic|sexistterm|slur1|slur2)\\\\b\"",
    "  log: true",
    "  adapt: evolve.real.aln",
    ""
  ].join("\n");

  if ((/\.ya?ml$/i.test(filePath) || /\.aln$/i.test(filePath)) &&
      !/^profanity_policy:/m.test(content)) {
    content += profanityPolicyBlock;
    changed = true;
    console.log("➕ Injected profanity_policy block (max safe list)");
  }

  if (changed) {
    fs.writeFileSync(filePath, content, "utf8");
    console.log(`✅ Repaired & updated ${filePath}`);
  } else {
    console.log(`✅ No changes needed for ${filePath}`);
  }

} catch (err) {
  console.error(`❌ Error processing ${filePath}:`, err);
  process.exit(1);
}
