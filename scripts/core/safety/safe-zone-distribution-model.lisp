;; Github: Doctor0Evil/ALN_Programming_Language.git
;; Path: scripts/core/safety/safe-zone-distribution-model.lisp
;;
;; Purpose:
;; Acts as a background safety intelligence checker, linked to rego.policy.
;; Prevents humor/horror/action logic from breaking sandbox boundaries.
;;
;; NOTE: All "racial-slur" or forbidden religious content is NULLIFIED at source.
;; The system instead generates meta replacement tokens to preserve immersion.
;; ---------------------------------------------------------------------------

(defpackage :safe.zone.distribution
  (:use :cl)
  (:export :intel-safety-check :reset-sandbox :wrap-execution))

(in-package :safe.zone.distribution)

(defvar *sandbox-state* 'stable
  "Represents the health of the narrative sandbox. Can be:
   - 'stable : execution is safe
   - 'breach : rule violation detected
   - 'recovering : system applying fallback
   - 'halted : sandbox unsafe, requires manual reset")

(defun intel-safety-check (classification severity violation-code prev-class)
  "Interface between LISP humor-engine and rego.policy outcomes.
   Evaluates if current reasoning is permissible."
  (cond
    ;; absolutely forbidden: ETHICS violation
    ((string= violation-code "ETHICS")
     (setf *sandbox-state* 'breach)
     (format t "[SAFETY] Violation detected: ~A. Breaking execution.~%" violation-code)
     nil)

    ;; severity critical -> halt immediately
    ((string= severity "critical")
     (setf *sandbox-state* 'halted)
     (format t "[SAFETY] Critical severity breach. Sandbox halted.~%")
     nil)

    ;; misclassification from Rego
    ((not (member classification '("humor" "horror" "action" "research" "other") :test 'string=))
     (setf *sandbox-state* 'recovering)
     (format t "[WARN] Invalid classification ~A. Auto-correcting to 'other'.~%" classification)
     "other")

    ;; transition check (humor can flip to action/research, etc.)
    ((and (string= prev-class "horror") (string= classification "humor"))
     (setf *sandbox-state* 'stable)
     (format t "[INFO] Smooth transition valid: horror -> humor~%")
     classification)

    (t
     (setf *sandbox-state* 'stable)
     classification)))

(defun reset-sandbox ()
  "Force restore the sandbox into stable state."
  (setf *sandbox-state* 'stable)
  (format t "[RESET] Sandbox reset to stable. Humor engine running clean.~%"))

(defun wrap-execution (fn &key (classification "humor") (severity "normal") (violation-code ""))
  "Wrapper around ANY execution unit. Auto-applies policy checks."
  (let ((status (intel-safety-check classification severity violation-code "humor")))
    (if status
        (funcall fn)
        (format t "[DENIED] Execution blocked by regulator.~%"))))
