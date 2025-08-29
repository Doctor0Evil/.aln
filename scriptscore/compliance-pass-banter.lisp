;; github-file-destination: scriptscore/compliance-pass-banter.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :alnfantasia.combat.compliance
  (:use :cl :slopbucket.humor :alexandria)
  (:export :run-compliance-action :moderation-flag-log :trace-console-output))

(in-package :alnfantasia.combat.compliance)

(defvar *session-tone* 'humor)
(defvar *sandbox-state* 'stable)
(defvar *realism-mode* t)
(defvar *flagged-events* nil)
(defvar *allowed-classes* '(humor horror action research other))
(defvar *profane-allowlist* '(fuck shit bitch asshole dick cunt prick))
(defvar *critical-blocklist* '(racial-slur1 racial-slur2 explicit-religious-offense))
(defvar *session-evolution* nil)

(defun flagged-term-present? (input)
  (find-if (lambda (word) (member word *critical-blocklist* :test #'equalp))
           (alexandria:flatten input)))

(defun profane-term-present? (input)
  (find-if (lambda (word) (member word *profane-allowlist* :test #'equalp))
           (alexandria:flatten input)))

(defun policy-check (classification severity violation-code prev-class input)
  (cond
    ((flagged-term-present? input)
     (push 'triggered-forbidden *flagged-events*)
     (setf *sandbox-state* 'breach)
     (format t "DENY: Forbidden racial/religious term detected. Nullifying. ~%")
     nil)
    ((string= violation-code "ETHICS")
     (push 'triggered-ethics *flagged-events*)
     (setf *sandbox-state* 'halted)
     (format t "DENY: Ethics violation code. Halting session. ~%")
     nil)
    ((not (member classification *allowed-classes*))
     (push 'triggered-misclassification *flagged-events*)
     (format t "WARN: Invalid classification, defaulting to OTHER. ~%")
     'other)
    (t
     (format t "ALLOW: Reasoning action permitted. Mode ~A.~%" classification)
     t)))

(defun run-compliance-action (input)
  (let* ((classification *session-tone*)
         (severity "normal")
         (violation-code nil)
         (prev-class *session-tone*)
         (result (policy-check classification severity violation-code prev-class input)))
    (if result
        (if (profane-term-present? input)
            (format t "PROFANITY DETECTED: Adult banter approved. ~%~A~%" input)
            (format t "CLEAN: No explicit banter. ~%~A~%" input))
        (format t "BLOCKED: Action denied. See flag log.~%"))))

(defun moderation-flag-log ()
  (format t "- Moderation Events: ~A~%" *flagged-events*)
  (format t "- Sandbox State: ~A~%" *sandbox-state*))

(defun trace-console-output ()
  (format t "==== COMPLIANCE TRACE OUTPUT ====")
  (format t "~%Session Tone: ~A~%Sandbox State: ~A~%Realism Mode: ~A~%"
          *session-tone* *sandbox-state* *realism-mode*)
  (format t "Flagged Events: ~A~%" *flagged-events*)
  (moderation-flag-log)
  (format t "Session Evolution: ~A~%" *session-evolution*))
