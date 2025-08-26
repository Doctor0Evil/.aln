package downtime.guardian

default allow = false

# Block sampled known malware signatures (auto-expand from DB)
deny[msg] {
  input.changes[_].diff[_].content matches input.threat_signatures[_].signature
  msg := sprintf("Threat '%s' detected: %s", [input.threat_signatures[_].type, input.threat_signatures[_].description])
}

# Block known dangerous calls
deny[msg] {
  re_match(".*(os\\.system|subprocess\\.|eval\\s*\\(|exec\\s*\\()].*", input.changes[_].diff[_].content)
  msg := "Dangerous shell/code execution call blocked"
}

# Block large/unusual files (anomaly backstop)
deny[msg] {
  input.changes[_].file_size > 500_000  # 500KB, tune for your repo
  msg := sprintf("Abnormally large file detected: %v", [input.changes[_].file])
}

# Placeholders for expanding more policies - add as many as needed
# Insert pattern: Just copy-paste or auto-insert more checks

# Example Slot (SHOW HOW TO EXPAND):
# deny[msg] { custom predicate logic ... }
