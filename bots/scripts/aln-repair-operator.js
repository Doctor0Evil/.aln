import fs from "node:fs";

const [,, file, adaptArg] = process.argv;
console.log(`Applying ${adaptArg} to ${file}...`);
// TODO: Implement actual repair/evolution logic.
// For now, just confirm file is UTF-8 and ends with newline.
let content = fs.readFileSync(file, "utf8");
if (!content.endsWith("\n")) {
  content += "\n";
  fs.writeFileSync(file, content);
  console.log(`Fixed missing newline in ${file}`);
}
