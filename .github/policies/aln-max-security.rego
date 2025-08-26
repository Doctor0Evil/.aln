package aln.security
# Block any commit that introduces secrets or tokens
deny[msg] {
    input.changes[_].diff[_].content matches /[A-Za-z0-9]{32,}/
    msg := "Secrets or tokens detected in commit. Blocked by ALN Max Security Policy."
}
# Enforce branch protection rules (require PR, review, signed commits)
deny[msg] {
    not input.pr.protected
    msg := "All changes must be via protected PR with review and signed commit."
}
# Enforce dependency update review for every PR
deny[msg] {
    input.changes[_].file == "package.json"
    not input.pr.dependencies_reviewed
    msg := "Dependency updates require security review and SCA scan."
}
# Block pushes from unknown IPs or outside allowed networks
deny[msg] {
    not ip_allowed(input.actor.ip)
    msg := sprintf("Push from unauthorized IP %v blocked.", [input.actor.ip])
}
ip_allowed(ip) {
    ip == "10.1.1.0/24"
    ip == "203.0.113.5"
}
# Enforce least privilege token use
deny[msg] {
    input.context.token_scope != "repo,workflow"
    msg := "Token does not meet minimum privilege requirements."
}
