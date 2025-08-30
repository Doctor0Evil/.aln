package humor_guard
# Allow adult/profane content if all required safety/access conditions are satisfied
allow_humor {
  # Only adults
  input.user.age >= 18
  # Humor is not hate/violence targeted at protected class
  not is_prohibited(input.content)
  # All mandatory warnings are present
  input.content.rating == "Mature"  # or ESRB: M, PEGI: 18+
  input.content.disclaimer == true
}
# Block humor/content that crosses legal/ethical bounds
deny_humor[msg] {
  is_prohibited(input.content)
  msg := "Prohibited content: hate speech, illegal acts, or protected class targeting"
}
# Helper: Scan content for banned patterns (replace these with regex lists/LLM/NLP calls)
is_prohibited(content) {
  # Example patterns — should use more robust PII/NLP/AI scanning in prod
  contains(content.text, "targeted hate")
  # Add more illegal activity patterns, explicit violence directed at minors, etc.
}
# Enforcement
default allow = false
allow {
  allow_humor
}
default deny = false
deny[msg] {
  deny_humor[msg]
}
