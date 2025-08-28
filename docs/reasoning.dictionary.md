# 🤖 Reasoning.Dictionary — Humor.Bot.AI Core Index

This dictionary provides a repo-wide indexed reasoning map for humor AI development.

---

## 1. Core AI Sources
- **src/ai/advanced-reasoning-core/logic-exe.lisp**
  - Purpose: Main reasoning logic executor (AI.Advanced.Reasoning.Core).
  - Handles evaluation pipeline for smart-joke AI.
  - Acts as the entry runtime for all humor classification and interpretation.

- **src/ai/advanced-reasoning-core/humor-classifier.lisp**
  - Purpose: Categorizes jokes by humor type.
  - Output classes: `:pun`, `:meta`, `:knock-knock`, `:setup-punchline`, `:n/a`.
  - Feeds into override logic for amplification or strict rejection.

- **src/ai/advanced-reasoning-core/humor_injection_ai_override.lisp**
  - Purpose: Installable humor-override middleware for `interpret-joke`.
  - Modes:
    - `:passthrough` (no change)
    - `:strict` (downgrades everything questionable)
    - `:amplify` (boosts dad jokes/puns/meta jokes)
    - `:force-funny` (EVERYTHING becomes funny, even silence)
  - Deterministic behaviors controlled by seeded RNG + manifest.
  - Auditing hook included.

---

## 2. Configs
- **config/humor-modules.manifest.lisp**
  - S-expression config controlling overrides.
  - Example:
    ```
    (:enable-override t :mode :amplify :audit t :seed 1337)
    ```

---

## 3. Tests
- **tests/test_humor_override.lisp**
  - Unit tests with FiveAM.
  - Confirms override properly coerces dad jokes and edge cases into `:funny`.
  - Verifies :force-funny behaves deterministically with seed options.

---

## 4. Scripts
- **scripts/humor-bot-ai.ps1**
  - Repo preflight and hygiene checker.
  - Validates required files exist, directories exist, checks LF line endings.
  - Returns nonzero exit code if repo structure invalid.

---

## 5. CI Workflows
- **.github/workflows/humor_override.yml**
  - CI job **humor-preflight** → Runs PowerShell checks.
  - CI job **humor-tests** → Loads source with SBCL.
  - CI job **humor-unit-tests** → Runs FiveAM unit tests on override logic.

---

## 6. Development Tools
- **Makefile**
  - `make test-humor` → Run unit tests locally.
  - `make preflight` → Run repo preflight checks locally.

---

# 🔹 Reasoning Flow

1. **logic-exe.lisp** runs base joke interpreter.  
2. **humor-classifier.lisp** adds humor type classification.  
3. **humor_injection_ai_override.lisp** wraps interpreter, applies override mode.  
   - Consults **manifest** for seed/mode/audit.  
4. **audit hooks** persist humor decisions if module exists.  
5. **test_humor_override.lisp** ensures override works → enforced in CI.  
6. **scripts** enforce repo hygiene → validated in workflow.  
7. **Makefile** gives dev-friendly local commands.

---

# 🔹 Math-Safe Wizardry
- Humor evaluation uses `(interpret-joke input) ⇒ (values status type)`  
- Status outcomes: `:funny | :not-funny`  
- Safe override logic:
