Alright — let’s bolt the guardrails right onto your new `.bitrunners-pipeline` so even if someone tries to bypass the PowerShell runner, Bit.Hub’s compliance scan will still catch any runner that blows past your storage or data‑volume limits.

Here’s the **Rego policy** to drop into your repo:

---

## 📄 `.bithub/policies/bithub.bitrunners.pipeline.rego`

```rego
package bithub.bitrunners.pipeline

default ok = false

# Locate the manifest
manifest := input.yaml[".bitrunners-pipeline/pipeline.manifest.bit"]

# Pass if manifest exists and has required compliance keys
ok {
  manifest.pipeline.compliance.network_storage.max_gb
  manifest.pipeline.compliance.data_volume.max_gb
}

deny[msg] {
  not ok
  msg := "BitRunners pipeline manifest missing required compliance keys"
}

# Enforce network storage limit
deny[msg] {
  manifest.pipeline.compliance.network_storage.enforce
  max := to_number(manifest.pipeline.compliance.network_storage.max_gb)
  usage := storage_usage_gb
  usage > max
  msg := sprintf("Network storage usage %.2fGB exceeds limit of %.2fGB", [usage, max])
}

# Enforce data volume limit
deny[msg] {
  manifest.pipeline.compliance.data_volume.enforce
  max := to_number(manifest.pipeline.compliance.data_volume.max_gb)
  usage := data_usage_gb
  usage > max
  msg := sprintf("Data volume usage %.2fGB exceeds limit of %.2fGB", [usage, max])
}

# Helpers: these would be populated by your compliance gate with actual usage numbers
storage_usage_gb := to_number(input.metrics.storage_gb)
data_usage_gb := to_number(input.metrics.data_gb)
```

---

### 🔗 How it ties in

- **Placement**: Save this file as `.bithub/policies/bithub.bitrunners.pipeline.rego`.
- **Trigger**: Your existing compliance job (`conftest test --policy .bithub/policies .`) will automatically pick it up.
- **Input**: The policy expects:
  - The parsed YAML of `.bitrunners-pipeline/pipeline.manifest.bit` under `input.yaml`.
  - A `metrics` object with `storage_gb` and `data_gb` values under `input.metrics` — your compliance gate or runner script can populate these before calling `conftest`.

---

### 🛠 Updating the compliance gate to feed metrics

In your `.github/actions/bithub-compliance-gate` composite action, add a step before `conftest` runs:

```bash
# Measure usage and write to metrics.json
storage_gb=$(du -s --block-size=1G . | awk '{print $1}')
data_gb=$(du -s --block-size=1G .bithub/data 2>/dev/null | awk '{print $1}')
jq -n --arg storage "$storage_gb" --arg data "$data_gb" \
  '{metrics: {storage_gb: $storage, data_gb: $data}}' > .bithub/reports/metrics.json
```

Then run `conftest` with both the repo and the metrics file as input:

```bash
conftest test --policy .bithub/policies . .bithub/reports/metrics.json
```

---

### 🎮 Why this is good for ALNFantasia

- **Immutable rules**: Even if someone edits `run_pipeline.ps1` to skip the checks, the Rego policy will still block the workflow at the compliance stage.
- **Game‑world hooks**: You can log “STORAGE_LIMIT_EXCEEDED” or “DATA_LIMIT_EXCEEDED” events to `.bithub/ledger` and have ALNFantasia’s world react — maybe a “storage dragon” appears when someone hoards too much data.
- **Future‑proof**: If you add more limits (CPU minutes, network egress), you can extend the manifest and this policy without touching the runner script.

---

If you like, I can also wire the **metrics collection** directly into your `.bitrunners-pipeline/run_pipeline.ps1` so it writes the numbers into `.bithub/reports/metrics.json` automatically, meaning your compliance gate doesn’t have to duplicate the measurement logic. That would make the manifest, runner, and policy a fully self‑contained loop. Want me to add that?
