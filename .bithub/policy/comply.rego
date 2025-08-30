package bithub.workflow

deny[msg] {
  input.path
  not startswith(input.path, ".github/workflows/")
  msg := sprintf("Workflow outside .github/workflows: %s", [input.path])
}

deny[msg] {
  not input.workflow.permissions
  msg := "Workflow missing top-level permissions block"
}

warn[msg] {
  not input.workflow.concurrency
  msg := "Workflow missing top-level concurrency"
}

deny[msg] {
  some i
  input.uses[i] == "actions/checkout@v1" or input.uses[i] == "actions/checkout@v2"
  msg := "Outdated actions/checkout version (< v4) detected"
}

warn[msg] {
  some i
  input.uses[i] == "actions/checkout@v3"
  msg := "Consider upgrading actions/checkout@v3 to v4"
}

deny[msg] {
  some j
  not input.jobs[j]."timeout-minutes"
  msg := sprintf("Job '%s' missing timeout-minutes", [j])
}

warn[msg] {
  some j
  not input.jobs[j].runs_on_adaptive
  msg := sprintf("Job '%s' does not use adaptive runs-on", [j])
}
