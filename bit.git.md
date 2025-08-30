# .bit
name: BitHub Universal Command
desc: Master control for repository actions, workflows, compliance, bots, and VM sync.
version: 1.0.0
commands:
  - bit.init          # Initialize repo with full receipt logging and policy bootstrap.
  - bit.clone <repo>  # Clone with sandboxing and workflow sync.
  - bit.push          # Auto-format, sign, tag, and push with receipts and bot hooks.
  - bit.commit <msg>  # Smart commit: includes context, diffs, AI screening, and compliance check.
  - bit.review        # Launch governance UI, auto-create merge receipts, async review sessions.
  - bit.deploy        # Deploys to VM, Docker, or any k8s/ALN platform, logs outcome as CircleK receipt.
  - bit.audit         # One-command policy, code, asset, and provenance audit—proof stored in .bitvault.
  - bit.bot <task>    # Run, spawn, or link BitHub, BitGov, or BitVault bots—auto-healing, adaptive.
  - bit.opa-enforce   # Execute ALN/OPA policy checks pre-merge or post-commit for continuous compliance.
  - bit.sync          # Real-time node, branch, and agent VM sync.
  # ... (Add dozens more for every conventional Git function, but with much stronger workflows, policy & UI)


***

### 2. Adaptive Bit.Bots: Smarter than GitHub Actions

- Deploy `.bit-hub-actions-bot` and `.bithub-bots` as VM agents:
  - Automatically repair, spawn, and patch workflows in response to failures, new policies, or code merges.
  - Fully context-aware—can create *new workflows on the fly* (something no vanilla GitHub bot can do).
  - Security: All tokens are ".bithub.token" (cross-compatible with `.github` but with Bit.Hub-level auth and audit).
  - Bot flows run on *dedicated VM clusters* that scale infinitely, not on 3rd-party GitHub runners.

***

### 3. Policy, Compliance, & OPA Integration

#### Sample .rego (OPA) Policy for Bit.Hub
```rego
# repo-enforce.rego
package bithub.allow_ops

# Allow PRs and merges only if all receipts are signed and policy tags are present.
allow_pr {
  input.receipts[i].signed
  input.policy.tags[_] == "ALNFantasia"
}

# Ban raising repo limits over security cap.
no_limit_raise = true {
  not input.requested_limits[_] > input.allowed_limits[_]
}
```

#### Sample BitHub YAML Workflow with OPA
```yaml
# .bit/.bithub-actions/policy-enforce.yml
jobs:
  check-policy:
    runs-on: bithub-vm
    steps:
      - uses: .bit/bithub-checkout@v7
      - name: OPA Policy Enforcement
        run: |
          opa eval --data ./policy --input pr.json --format pretty 'data.bithub.allow_ops.allow_pr'
```
**Result**: No workflow (merge, push, deploy) executes if policies (audit, compliance, limits) are not met—and all in the CircleK receipt format.

***

### 4. Extension & Evolution Points

- `.bit` files and configs (`.bit.cfg`) are *self-describing* and *hot-patchable*: just push a new `.bit` file and your repo instantly supports new commands or behaviors—no full CI/CD pipeline update required.
- `.bithub.token`: Used in place of or alongside GitHub tokens, instantly recognized and supported by legacy Git and new Bit.Hub bots.
- "**run: santa.bit**": Any bot or adaptive policy can not only run workflows, but *act out scenarios, inject entertainment, game logic, or compliance tests*—all with full archiving, audit, and reboot-on-failure.
- Every OPA policy or BitHub command stays *compliant* with external frameworks (ALN, Lisp, batch, shell, etc.) and *enforces your “no higher bar” rule*—nobody can touch your system’s ultimate operational ceiling.

***

### 5. Why Bit.Hub > GitHub?

- **Self-healing, infinitely adaptive VMs**
- Live bot deployment, orchestration, and dry-run (with real content delivery and feedback—not mere PHP/YAML state)
- Open contributions, gaming, compliance, and security—everything logged, tracked, and governed in the most transparent, user-driven system possible.
- True vendor lockout resilience: *no single failure or policy change can cripple your workflows or creative ecosystem*.

***

**Summary:**  
Bit.Hub’s `.bit` commands and `.bithub` actions leap past GitHub by offering a *smarter, more adaptive, and more deeply compliant* foundation—fully compatible with legacy Git for integration, but running on richer, VM-backed, policy-driven, and entertainment-ready nodes. All logs, UIs, and workflows are CircleK receipt format, with full audit and game governance by design.
