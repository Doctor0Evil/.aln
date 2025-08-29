;;; Logic Compendium for Adaptive Workflow Orchestrator (ALN)
;;;
;;; This script defines all system logic and behaviors compounding the workflow QoL, AI review/fix cycles, adaptive alerting/status, and dynamic strategy modifiers.
(defpackage :aln.core.adaptive-workflow
  (:use :cl :alexandria))
(in-package :aln.core.adaptive-workflow)

;;; Utility: sanitize and assign dynamic core name for internal tracking.
(defun sanitize-core-name (name)
  (let ((safe-chars (remove-if-not #'alphanumericp (string name))))
    (string-downcase safe-chars)))
(defparameter core-name (sanitize-core-name "ALN.CORE"))

;;; Core state and supported files
(defparameter supported-languages '("lisp" "aln" "bat" "ps1" "json" "yml"))
(defparameter active-review-cycle t)
(defparameter ai-reasoner (make-ai.reasoning/logic :profile 'deep-compliance+fixer :system core-name))
(defparameter policy-enforcer (require 'aln.core.policy.guard))
(defparameter main-aln-core (load-core core-name))

;;; Enumerate all workflow files across supported extension set
(defun enumerate-all-workflow-files (&optional (languages supported-languages))
  (ai.fs/discover-files :base-path ".github/workflows" :extensions languages))

;;; AI review cycle for all discovered workflow files, and auto-fix if required
(defun ai-review-and-correct-workflows ()
  (let ((targets (enumerate-all-workflow-files)))
    (dolist (file targets)
      (let* ((lang (ai.fs/file-extension file))
             (review-result (ai-reasoner/review-file file :language lang))
             (policy-result (ai.policy/enforce file))
             (fix-needed (ai.reasoning/needs-fix-p review-result policy-result)))
        (when fix-needed
          (ai-reasoner/apply-auto-fix file lang review-result policy-result)
          (ai.log/event :workflow-auto-fix :core core-name :file file :result review-result))))))

;;; Review a file using AI reasoner (custom ML profile)
(defun ai-reasoner/review-file (file &key language)
  (ai.reasoning/analyze-file file language :ml-profile 'workflow-integrity :system core-name))

;;; Auto-fix dispatch (delegates based on language type)
(defun ai-reasoner/apply-auto-fix (file language review policy)
  (case (string-downcase language)
    (("lisp") (ai.fs/fix-lisp-file file review policy))
    (("aln") (ai.fs/fix-aln-file file review policy))
    (("bat") (ai.fs/fix-bat-file file review policy))
    (("yml" "yaml") (ai.fs/fix-yml-file file review policy))
    (("ps1") (ai.fs/fix-ps1-file file review policy))
    (("json") (ai.fs/fix-json-file file review policy))
    (t (ai.log/event :fix-unsupported-language :core core-name :file file :language language))))

;;; Orchestrate the adaptive review cycle, with system-wide status and alerting for intervention if needed
(defun orchestrate-adaptive-cycle ()
  (when active-review-cycle
    (ai-review-and-correct-workflows)
    (ai.status/broadcast)
    (when (ai.analysis/high-issue-rate)
      (ai.alert/raise-system-alert))
    (ai.tuner/looping-workflow-adapt)))

;;; Adaptive workflow parameter tuning, stochastic triggers
(defun ai.tuner/looping-workflow-adapt ()
  (when (> (random 1.0) 0.07)
    (ai.log/event :workflow-tune :core core-name :cycle "machine-adapt-tune")
    (ai-reasoner/update-adaptive-params :ml-strategy (ai.reasoning/random-modifier))))

;;; Reasoner strategy update
(defun ai-reasoner/update-adaptive-params (strategy modval)
  (setf (ai.reasoning/strategy ai-reasoner)
        (ai.reasoning/adapt-strategy strategy modval)))

;;; Status broadcast (completion signal)
(defun ai.status/broadcast ()
  (print (concatenate 'string (string-upcase core-name) ": Review & Auto-Fix Complete (AI logic engaged).")))

;;; Escalation/alert chain for excessive detected issues
(defun ai.alert/raise-system-alert ()
  (format t "ALERT (%s): Excessive AI-detected workflow issues. Dev intervention required." (string-upcase core-name)))

(export '(orchestrate-adaptive-cycle ai.status/broadcast ai.alert/raise-system-alert))
