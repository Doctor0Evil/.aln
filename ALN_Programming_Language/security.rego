package aln_security

default secure = false

secure {
  input.encryption == 'AES-256-GCM'
  input.validation == 'hmac_sha256_verify'
}
