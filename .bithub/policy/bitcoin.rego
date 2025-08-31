package bithub.bitcoin

default allow_execution = false

allow_execution {
  input.runner.bitcoin_balance >= input.job.required_bitcoin_stake
  valid_compliance_proof
}

valid_compliance_proof {
  input.compliance.pass == true
  input.compliance.policy_version == "1.0.0"
  input.compliance.audit_hash == input.bitcoin_token.proof.compliance_hash
}

deny[msg] {
  not allow_execution
  msg := sprintf("Insufficient .bit.coin balance or invalid compliance proof for job %s", [input.job.id])
}
