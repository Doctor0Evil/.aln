package aln.downtime_intel

default allow = false

# Block known virus/implant signatures
deny[msg] {
  some sig
  sig := data.threat_signatures[_]
  input.scan.contents[_] == sig.signature
  msg := sprintf("Known threat '%v' (%v) detected in scan.", [sig.signature, sig.type])
}

# Block unusual privilege use or code execution outside normal patterns
deny[msg] {
  input.behavior.anomalous
  msg := "Anomalous runtime behavior identified by ML/NLP. Blocked by hotwire.network.intel."
}

# Identify suspicious changes, expansions, or inserts to critical ALN modules
deny[msg] {
  input.change.target == "src/aln_framework/core.aln"
  not allowed_insert(input.change)
  msg := sprintf("Unauthorized insert/expansion in core framework: %v", [input.change.details])
}

# Allow only if scanned history is clear and no anomalies/vulns found
allow {
  not deny[_]
  input.status == "intelligence.clear"
}
