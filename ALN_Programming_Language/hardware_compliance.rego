package aln_hardware_compliance

default allow = false

allow {
  input.chip.valid
  input.security.score > 99
}
