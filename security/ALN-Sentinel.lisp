;;; ================================================================
;;; ALN-Sentinel.lisp --- Real-Time Security Guard for GitHub Repo
;;; ================================================================
;;; This module enforces basic DevSecOps protections for the repo:
;;;   - Scans files for secret tokens (AWS, GitHub, Slack, etc).
;;;   - Enforces branch protection (restricts pushes to main/develop).
;;;   - Aborts commit/push on policy violations.
;;; ================================================================
(defpackage :aln-sentinel
  (:use :cl :sb-posix))
(in-package :aln-sentinel)
(defun secret-pattern-p (str)
  "Detect presence of forbidden secrets/token-like patterns in STR."
  (or (search "GITHUB_" str)                 ; GitHub tokens
      (search "AKIA" str)                    ; AWS Access Key prefix
      (search "xoxb-" str)                   ; Slack bot tokens
      (and (> (length str) 32)               ; Long alphanumeric suspicious string
           (every #'alphanumericp str))))
(defun scan-file (path)
  "Scan a single file for secrets/patterns. Block commit if triggered."
  (with-open-file (s path)
    (loop for line = (read-line s nil)
          while line
          when (secret-pattern-p line) do
            (progn
              (format t "~%SECURITY ALERT: Potential secret in ~A: ~A~%" path line)
              (error "Commit blocked by ALN Sentinel - Secret Detected!")))))
(defun scan-repo (repo-path)
  "Recursively scan files in repo-path for secrets."
  ;; Warning: simplified directory recursion. Extend for deep-walk if needed.
  (dolist (file (directory (merge-pathnames "*" repo-path)))
    (when (probe-file file)
      (scan-file file))))
(defun enforce-branch-policy ()
  "Enforce branch policy to allow only safe commits on main or develop."
  (let* ((branch (run-program "git" '("rev-parse" "--abbrev-ref" "HEAD") :output :string)))
    (unless (or (string= branch "main")
                (string= branch "develop"))
      (error "ALN-Sentinel: Pushes allowed only from protected branches (main/develop)."))))
(defun main ()
  (format t "~%ALN Sentinel Initialized. Scanning...~%")
  (scan-repo "/path/to/ALN_Repo")  ;; Replace with actual repo path
  (enforce-branch-policy)
  (format t "~%ALN Sentinel passed. Git operation allowed.~%"))
(main)
