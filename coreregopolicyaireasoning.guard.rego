# coreregopolicyaireasoning.guard.rego
package intelligence.regulator

# STRICT ALLOW of regular profanity (adult content) for *approved* profane class
allow_adult_profanity {
  input.term == "fuck"
} else {
  input.term == "shit"
} else {
  input.term == "bitch"
} else {
  input.term == "asshole"
} else {
  input.term == "cunt"
}

# Maintain narrative anchor to ensure allowed profanities remain in text
profanity_floor = 0.45
permit_adult_banter {
  input.context == "mature-profane"
  allow_adult_profanity
}

# ONLY restrict the following (sanitize/replace with [REDACTED]):
restricted_terms = {"tweaker", "meth", "meth-head", "crack-head"}

deny_drug_slur {
  term := input.term
  restricted_terms[term]
}

mask_drug_slur {
  restricted_terms[input.term]
  output = "[REDACTED]"
}

allow {
  permit_adult_banter
}

deny {
  deny_drug_slur
}

denymsg = "Prohibited term: only select banned words are blocked; regular profanity is protected."
