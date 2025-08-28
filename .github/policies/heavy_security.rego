package aln.heavy_security
# Block pushes with secrets/tokens
deny[msg] {
  input.changes[_].diff[_].content =~ "[A-Za-z0-9]{32,}"
  msg := "Potential secret/token detected in diff. Commit rejected."
}
# Enforce branch protection for main/develop
deny[msg] {
  input.target_branch == "main" or input.target_branch == "develop"
  not input.pr.open
  msg := "Direct push to protected branch rejected. PR with review required."
}
# Block unsigned commits
deny[msg] {
  not input.commit.signed
  msg := "All commits must be GPG-signed."
}
# Deny weak configurations or missing checks
deny[msg] {
  not input.status_checks.passed
  msg := "Required status checks not passed. Merge denied."
}
# Enforce locked dependencies
deny[msg] {
  input.changes[_].file == "package-lock.json" or input.changes[_].file == "yarn.lock"
  not input.dependency_update_reviewed
  msg := "Dependency file changed without required review."
}
