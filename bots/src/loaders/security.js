export function isOrgAllowed(ctx) {
  const allowed = (process.env.ALN_ALLOWED_ORGS || "").split(",").map(s => s.trim()).filter(Boolean);
  if (allowed.length === 0) return true;
  const org = ctx.payload.organization?.login || ctx.payload.repository?.owner?.login;
  return allowed.includes(org);
}
