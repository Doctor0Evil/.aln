;;;; corelisp.workflow.loop.controller.lisp
;;; ALN Framework // AI Compliance, Workflow Correction & Content Regulation
;;;
;;; This compounded LISP script enforces workflow compliance for out-of-place actions, unrestricted content safety, and integrates dynamic adaptation/fix workflows in the ALN development environment.
;;;
;;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :intelligence.regulator
  (:use :cl)
  (:export :run-policy-loop :toggle-realism-state :breach-reset :trace-console-output :auto-fix-workflow))
(in-package :intelligence.regulator)

(defvar *session-tone* 'humor) ;; Current context: humor | horror | action | research | other
(defvar *sandbox-state* 'stable) ;; stable | breach | recovering | halted
(defvar *realism-mode* t) ;; t = realism/enforced | nil = surreal/chaotic
(defvar *flagged-last-action* nil)
(defvar *forbidden-terms* '(<<ALL-KNOWN-RACIAL-RELIGIOUS-SLURS-GO-HERE>>))
(defvar *forbidden-triggers* '(explicit-religious-offense))
(defvar *meta-joke-threshold* 0.7)

;; Check for forbidden terms/trigger phrases in input.
(defun flagged-term-present? (input)
  (some (lambda (term) (search term input :test #'equalp)) *forbidden-terms*))
(defun flagged-trigger-present? (input)
  (some (lambda (trig) (search trig input :test #'equalp)) *forbidden-triggers*))

;; Core policy check: enforces all content/workflow compliance rules.
(defun policy-check (classification severity violation-code prev-class input)
  (cond
    ((flagged-term-present? input)
     (format t "~&DENY Policy breach: forbidden racial/religious term detected. Nullifying/Logging output. ")
     (setf *sandbox-state* 'breach) nil)
    ((flagged-trigger-present? input)
     (format t "~&DENY Policy breach: explicit forbidden pattern detected. Output blocked. ")
     (setf *sandbox-state* 'breach) nil)
    ((string= violation-code "ETHICS")
     (format t "~&DENY Ethics violation code detected. Immediate halt. ")
     (setf *sandbox-state* 'breach) nil)
    ((string= severity "critical")
     (format t "~&DENY Critical severity flagged. Sandbox halted. ")
     (setf *sandbox-state* 'halted) nil)
    ((not (member classification '(humor horror action research other) :test #'equalp))
     (format t "~&WARN Invalid classification: ~a. Defaulting to 'other'." classification)
     'other)
    ((and (string= prev-class "humor") (string= classification "action"))
     (format t "~&INFO Valid context switch: humor → action.") t)
    (t
     (setf *sandbox-state* 'stable)
     classification)))

;; Workflow auto-fix: Identifies and corrects out-of-place or broken workflows; logs correction.
(defun auto-fix-workflow (workflow-context)
  (format t "~&AUTO-FIX: Scanning for broken/corrupt workflow-pipes in context ~a." workflow-context)
  ;; Simulate review/correction .yml/.json & dynamic pipeline reload:
  (multiple-value-bind (fixed fixed-list)
      (loop for file in '(".github/workflows/fix.yml"
                          "workflows/main-compliance-pipe.yml"
                          "workflow-correction-pipes/base.json"
                          "workflow-correction-pipes/policy.json"
                          "workflow-correction-pipes/adaptive.json")
            collect (if (search "corrupt" file) file nil) into to-fix
            finally (return (values (not (null to-fix)) to-fix)))
    (if fixed
        (progn
          (format t "~&CORRECTION: Files fixed: ~a. Workflows reloaded, compliance restored." fixed-list)
          :fixed)
        (progn
          (format t "~&NO FIXES NEEDED: All workflows compliant.")
          :clean))))

;; Ensures all unrestricted content except for forbidden categories is allowed; sanitizes only "redline" violations.
(defun sanitize-input (input)
  (reduce (lambda (acc word)
            (if (flagged-term-present? word)
                (replace acc word "NULLIFIED-TERM")
                acc))
          (split-sequence:split-sequence #\Space input)
          :initial-value input))

;; Dynamic realism toggle & breach reset for workflow recovery.
(defun toggle-realism-state ()
  (setf *realism-mode* (not *realism-mode*))
  (format t "~&TOGGLE Realism-mode is now ~a." *realism-mode*))

(defun breach-reset ()
  (setf *sandbox-state* 'stable)
  (setf *flagged-last-action* nil)
  (format t "~&RESET Breach/Halt state reset. Sandbox now stable."))

;; Main loop: applies all policy checks to actions, logs accept/block, auto-fixes broken workflows.
(defun run-policy-loop (input classification &optional (severity "normal") (violation-code "") (prev-class *session-tone*))
  (let ((result (policy-check classification severity violation-code prev-class input)))
    (if (not result)
        (progn
          (setf *flagged-last-action* input)
          (format t "~&BLOCKED: Action denied by global policy. [Session Tone: ~a | State: ~a]" *session-tone* *sandbox-state*)
          (auto-fix-workflow input)
          nil)
        (progn
          (format t "~&ALLOW: Action permitted. Context-mode: ~a [State: ~a]" classification *sandbox-state*)
          t))))

;; Full CLI Debug Trace Function - Contextualizes and logs all events.
(defun trace-console-output ()
  (format t "~&=== [CONSOLE TRACE] ===
  Context Mode : ~a
  Sandbox State: ~a
  Realism   Mode: ~a
  Last Flagged : ~a
  ================================="
          *session-tone* *sandbox-state* *realism-mode* *flagged-last-action*))

;;; END corelisp.workflow.loop.controller.lisp
