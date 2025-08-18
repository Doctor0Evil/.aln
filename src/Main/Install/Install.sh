```bash
# Install ALN CLI
curl -s https://aln-lang.com/install.sh | bash

# Verify installation
aln --version
# Output: ALN v5.0.0 (Production-Ready)
```

### Create Your First ALN Program

```aln
// hello.aln
@INIT {
  cfg.aln.compliance!enforce: true,
  cfg.aln.security!level: "quantum_stealth",
  cfg.aln.runtime!mode: "production"
}

@PROCESS hello_world {
  input: "Hello, ALN World!",
  @TRANSFORM input TO aln_syntax {
    source: "user_input",
    validation: "strict_schema_check",
    output_format: "aln_terminal_commands"
  }
  @EXEC { command: "echo $input", success_criteria: "status=0" }
}
```
