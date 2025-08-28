;; =====================================================
;; ALN Orchestration / Automation Workflow Lisp Compound
;; =====================================================
(defun aln-onboard-session ()
  "Initialize ALN onboarding as a bootstrap sequence"
  (let ((ps-result (invoke-ps "aln.ps1")))
    (if (zerop (exit-code ps-result))
        (log-info "[BOOTSTRAP] ALN setup sequence complete.")
        (progn
          (log-error (format nil "[ERROR] Powershell bootstrap failed: ~A"
                             (exit-code ps-result)))
          (return-from aln-onboard-session :fail)))))

(defun aln-config-init ()
  "Ensure .alnconfig exists and is set active"
  (if (not (file-exists? (expand-path "~/.alnconfig")))
      (progn
        (create-file "~/.alnconfig")
        (log-info "[CONFIG] New ALN config created")))
  (write-file "~/.alnconfig"
              (format nil "ALN_ENV=active;SESSION=non-admin;START=~A"
                      (now)))
  (log-info "[ALN CONFIG] Session marked active."))

(defun aln-launch-terminal ()
  "Launch pseudo-terminal session"
  (log-info "[ALN BOOTSTRAP] Launching ALN Terminal…")
  (simulate-process "powershell" '("-NoLogo" "-NoExit" "-Command" "echo Welcome to ALN terminal")))

(defun workflow-scan-and-fix ()
  "Scan workflows, apply path fixes, and orchestrate jobs"
  (dolist (wf (scan-dir ".github/workflows/*.yml"))
    (log-debug (format nil "[ORCH] Checking workflow: ~A" wf))
    (let ((fix (auto-path-fix wf)))
      (if fix
          (log-info (format nil "[FIXED] paths corrected in ~A" wf))
          (log-error (format nil "[ERROR] Failed path correction in ~A" wf))))))

(defun workflow-run ()
  "Trigger ALN agent after fixer pass"
  (log-info "[RUN] Issuing ALN workload commands…")
  (aln-launch-terminal)
  (log-info "[RUN COMPLETE] Workload processed."))

(defun full-orchestrate ()
  "Full orchestrator pipeline execution"
  (aln-onboard-session)
  (aln-config-init)
  (workflow-scan-and-fix)
  (workflow-run)
  (log-info "[COMPLETE] Orchestration lifecycle done."))
