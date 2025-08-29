// Parses /aln commands from issue/PR comments.
// Supports both slash and raw: "/aln.game.dice { sides: 20 }" or "aln.game.dice {sides:20}"
export function parseCommand(body) {
  const line = body.split("\n").find(l => l.trim().startsWith("/aln.") || l.trim().startsWith("aln."));
  if (!line) return null;

  const [cmd, ...rest] = line.trim().replace(/^\//, "").split(/\s+/, 2);
  let params = {};
  if (rest.length) {
    const jsonLike = rest[0]
      .replace(/([a-zA-Z0-9_]+)\s*:/g, '"$1":') // keys to JSON
      .replace(/'/g, '"');
    try { params = JSON.parse(jsonLike); } catch { params = {}; }
  }
  return { cmd, params };
}
