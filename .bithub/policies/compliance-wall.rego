package bithub.compliance

default allow = false

# Inputs expected in evaluation:
# input.domain, input.protocol, input.headers, input.runner_labels, input.org, input.actions_pinned, input.dispatch_sig_valid, input.host

# Egress
deny[msg] {
  some d
  d := input.domain
  not d == allowed_domains[_]
  msg := sprintf("Domain not allowlisted: %v", [d])
}
deny[msg] {
  input.domain == denied_domains[_]
  msg := sprintf("Domain explicitly denied: %v", [input.domain])
}
deny[msg] {
  input.protocol != "https"
  msg := "Only HTTPS is allowed"
}

# Headers (DLP)
deny[msg] {
  some h
  h := lower(input.headers[_].name)
  h == "authorization"
  msg := "Authorization header is redacted by policy"
}

# Runners
deny[msg] {
  some l
  l := input.runner_labels[_]
  l == denied_runner_labels[_]
  msg := sprintf("Runner label denied: %v", [l])
}
deny[msg] {
  not any_allowed_label(input.runner_labels)
  msg := "No approved runner labels present"
}

# GitHub org & dispatch signature
deny[msg] {
  not input.org == allowed_orgs[_]
  msg := sprintf("Repository org not allowed: %v", [input.org])
}
deny[msg] {
  require_dispatch_sig
  not input.dispatch_sig_valid
  msg := "Missing or invalid signed repository_dispatch"
}

# Actions pinning
deny[msg] {
  input.actions_pinned == false
  msg := "GitHub actions must be pinned by digest"
}

# Branding / host routing
deny[msg] {
  not input.host == "so.bit"
  msg := "Host must be so.bit per branding policy"
}

# Allow if no denies fired
allow {
  not deny[_]
}

# Helpers
any_allowed_label(labels) {
  some l
  l := labels[_]
  l == allowed_runner_labels[_]
}

# Policy data stitched in by runner (from ALN manifest)
allowed_domains := input.allowed_domains
denied_domains := input.denied_domains
allowed_runner_labels := input.allowed_runner_labels
denied_runner_labels := input.denied_runner_labels
allowed_orgs := input.allowed_orgs
require_dispatch_sig := input.require_dispatch_sig
