;; Correction Directive for: workflow.file.lol.ps1
;; PowerShell cannot interpret GitHub Actions YAML or ALN workflow DSL: must use .yml not .ps1.

;; Core logic:
;;   - workflow files for GitHub Actions = YAML (.yml/.yaml)
;;   - workflow logic in ALN DSL = .aln or .mk.file in repo
;;   - PowerShell (.ps1) files = imperative scripts, NOT workflow specs

;; Enforce: Do NOT run workflow YAML, ALN workflow, or GitHub Actions config as PowerShell scripts.

(defun diagnose-workflow-misuse (filepath)
  (when (and (stringp filepath)
             (or (string-equal (file-name-extension filepath) "yml")
                 (string-equal (file-name-extension filepath) "aln")))
    (log-event (list :type "workflow-file-misuse"
                     :error "Attempted to run workflow/config file as PowerShell"
                     :file filepath
                     :advice "Run YAML in GitHub Actions or ALN in ALN engine; only .ps1 in PowerShell.")))
  t)
;; Example:
(diagnose-workflow-misuse ".github/workflows/workflow.file.lol.ps1")
