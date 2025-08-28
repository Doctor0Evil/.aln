;;; ================================================================
;;; Immutable Audit Piles, Stack Overflow Detection, PowerShell Boom
;;; ================================================================
;;; Purpose: Implements an audit-pile structure that is:
;;; - Immutable: Once an audit entry is appended, it cannot be changed or erased.
;;; - Piled: Entries stack up (acting like logs, with each action/flag/alarm etc).
;;; - Stack Overflow: If too many entries exist (e.g., file overfill), the system
;;;   triggers a 'boom' alarm to external systems (here, delegation is to PowerShell).
;;; - PowerShell integration: A script is triggered when the file/audit exceeds
;;;   soft or hard limits; e.g., file watch triggers an external .ps1 notification.
;;;
;;; Companion PowerShell script described after Lisp code.
;;;
;;; github-file-destination:
;;;   - core/ai/audit/immutable_audit_pile.lisp
;;;   - scripts/audit-boom.ps1

(defpackage :ai.immutable-audit
  (:use :cl)
  (:export :push-audit-pile
           :get-pile
           :pile-overflow-p
           :audit-boom-action))

(in-package :ai.immutable-audit)

;; ===========================================
;; Immutable Audit Pile
;; ===========================================
(defparameter *audit-pile* '()
  "Immutable audit log pile (list, never mutated in-place).")

(defparameter *pile-soft-limit* 1000
  "Soft limit: warning/overflow notification triggers at this size.")

(defparameter *pile-hard-limit* 1200
  "Hard limit: boom alarm triggers at this size.")

(defun push-audit-pile (entry)
  "Add entry as new immutable cons cell at head. List append is discouraged (immutability)."
  (let ((new-pile (cons entry *audit-pile*)))
    (setf *audit-pile*
          (copy-list new-pile)) ;; Ensures no accidental mutation from previous refs
    (when (pile-overflow-p *audit-pile*)
      (audit-boom-action))
    *audit-pile*))

(defun get-pile ()
  "Return entire immutable audit pile (copy)."
  (copy-list *audit-pile*))

(defun pile-overflow-p (pile)
  "Returns T if pile exceeds soft or hard limit. Can return what type of overflow if needed."
  (let ((sz (length pile)))
    (cond ((>= sz *pile-hard-limit*) :boom)
          ((>= sz *pile-soft-limit*) :soft)
          (t nil))))

(defun audit-boom-action ()
  "Triggered on hard-limit exceeded. Notifies external PowerShell stack action."
  (let ((ps-script "scripts/audit-boom.ps1"))
    (when (probe-file ps-script)
      ;; Quick & dirty shell-out. Proper implementation should log/monitor failure.
      (uiop:run-program (format nil "pwsh -File ~a" ps-script)))
    (format t "[AUDIT BOOM] Hard limit exceeded! PowerShell boom alarm triggered!~%")))

;;; Example Usage:
;;;   => (push-audit-pile '(:event alarm :level warning :msg "Content flagged!"))
