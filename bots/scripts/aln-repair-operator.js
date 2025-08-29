#!/usr/bin/env node
/**
 * ALN Repair Operator
 * Background-repairs .yml and .aln workflow/config files using "fun.adapt=evolve.real.aln".
 * Safe by default: fixes only if certain lint/parse rules fail.
 * Future expansion: integrate AI/ML transforms, schema migration.
 */

import fs from "node:fs";
import path from "node:path";

const [, , filePath, adaptArg] = process.argv;
if (!filePath) {
  console.error("Usage: aln-repair-operator.js <file> [--adapt=fun.adapt=evolve.real.aln]");
  process.exit(1);
}

console.log(`🔍 Processing ${filePath} with ${adaptArg || "(default mode)"}`);

try {
  let content = fs.readFileSync(filePath, "utf8");
  let original = content;

  // Basic hygiene: enforce trailing newline
  if (!content.endsWith("\n")) {
    content += "\n";
  }

  // Schema guard for .yml/.yaml: ensure keys are indented with 2 spaces, not tabs
  if (/\t/.test(content) && /\.(ya?ml)$/i.test(filePath)) {
    console.log("⚠️  Tabs detected in YAML — converting to spaces");
    content = content.replace(/\t/g, "  ");
  }

  // Ensure .aln files have 'operator:' header if missing
  if (/\.aln$/i.test(filePath) && !/^operator:/m.test(content)) {
    console.log("ℹ️ Adding default operator header to ALN file");
    content = `operator: evolve.real.aln\n` + content;
  }

  // Extra: remove trailing spaces
  content = content.replace(/[ \t]+(\r?\n)/g, "$1");

  // Only write if changed
  if (content !== original) {
    fs.writeFileSync(filePath, content, "utf8");
    console.log(`✅ Repaired ${filePath}`);
  } else {
    console.log(`✅ No changes needed for ${filePath}`);
  }
} catch (err) {
  console.error(`❌ Error processing ${filePath}:`, err);
  process.exit(1);
}
