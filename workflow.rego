package workflows

# Entry point: deny list
deny[msg] {
    not workflow_has_required_jobs
    msg := "❌ Workflow is missing one or more required jobs: validate, build-dotnet, security-scan"
}

deny[msg] {
    not workflow_has_security_scan
    msg := "❌ Workflow must include a security scan job"
}

deny[msg] {
    not workflow_has_branch_protection
    msg := "❌ Workflow must trigger only on main or develop branches"
}

# --- Rules ---

workflow_has_required_jobs {
    some job
    required := {"validate", "build-dotnet", "security-scan"}
    jobs := {name | name := input.jobs[_].name}
    required ⊆ jobs
}

workflow_has_security_scan {
    some job
    job := input.jobs[_]
    lower(job.name) == "🔒 security scan"
}

workflow_has_branch_protection {
    branches := input.on.push.branches
    branches == ["main", "develop"]
}
