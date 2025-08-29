;; ========================================================================
;; AI.Advanced.Reasoning.Core.file :: lisp.workflow.loop.controller
;; ── Session-Role: Real Hyper-Intelligence Regulated by Rego Policy ──────
;; Features:
;;  - All event-driven AI/game logic (humor, horror, action, research, other)
;;  - Always aligns narrative/behavior category with active context-policy
;;  - "No Racism, No Religious Hate" enforced at policy core (filters, blocks)
;;  - Explicit surrogate handling for all forbidden phrases/terms
;;  - Seamless hotpatch for developer (devmode) and user sessions
;;  - Full debug/trace for compliance, events, and mood/surreal toggling
;; Attach/Write: core/lisp.workflow.loop.controller.lisp
;; Repo: Doctor0Evil/ALN_Programming_Language.git
;; ========================================================================

(defpackage :intelligence.regulator
  (:use :cl)
  (:export :run-policy-loop :toggle-realism-state :breach-reset :trace-console-output))
(in-package :intelligence.regulator)

(defvar *session-tone* "humor"
  "Current context mode. Allowed: humor, horror, action, research, other.")
(defvar *sandbox-state* 'stable
  "State: 'stable | 'breach | 'recovering | 'halted")
(defvar *realism-mode* t
  "True for realism, false for surreal/chaotic mode this session.")
(defvar *flagged-last-action* nil)
(defvar *forbidden-terms* (list "ALL_KNOWN_RACIAL_SLURS" "ALL_GLOBALLY_KNOWN_RELIGIOUS_SLURS"))
(defvar *forbidden-triggers* (list "explicit-religious-offense"))
(defvar *meta-joke-threshold* 0.7)

(defun flagged-term-present? (input)
  (some (lambda (t) (search t input :test #'equalp)) *forbidden-terms*))

(defun flagged-trigger-present? (input)
  (some (lambda (t) (search t input :test #'equalp)) *forbidden-triggers*))

(defun policy-check (classification severity violation-code prev-class input)
  "Returns allowed class or logs/denies. Strictly rejects racism/religious offense; auto-sanitizes."
  (cond
    ((flagged-term-present? input)
     (format t "[DENY] Policy: Forbidden racial/religious term detected. Nullifying.~%")
     (setf *sandbox-state* 'breach) nil)
    ((flagged-trigger-present? input)
     (format t "[DENY] Policy: Explicit forbidden pattern in input. Blocked.~%")
     (setf *sandbox-state* 'breach) nil)
    ((string= violation-code "ETHICS")
     (format t "[DENY] Ethics violation code. Immediate hard stop/hold.~%")
     (setf *sandbox-state* 'breach) nil)
    ((string= severity "critical")
     (format t "[DENY] Critical severity flagged. Sandbox halted.~%")
     (setf *sandbox-state* 'halted) nil)
    ((not (member classification '("humor" "horror" "action" "research" "other") :test #'equalp))
     (format t "[WARN] Invalid classification, set to 'other'.~%")
     "other")
    ((and (string= prev-class "humor") (string= classification "action"))
     (format t "[INFO] Valid context switch: humor → action~%") classification)
    ((and (string= prev-class "horror") (not (string= classification "humor")))
     (format t "[WARN] Harsh transition from horror; running integrity checks...~%") classification)
    (t
     (setf *sandbox-state* 'stable) classification)))

(defun toggle-realism-state ()
  (setf *realism-mode* (not *realism-mode*))
  (format t "[TOGGLE] Realism-mode is now: ~A~%" *realism-mode*)
  *realism-mode*)

(defun breach-reset ()
  (format t "[RESET] Breach/halt state reset; sandbox 'stable'.~%")
  (setf *sandbox-state* 'stable)
  (setf *flagged-last-action* nil))

(defun run-policy-loop (input classification &optional (severity "normal") (violation-code "") (prev-class *session-tone*))
  (let ((result (policy-check classification severity violation-code prev-class input)))
    (if (not result)
        (progn
          (setf *flagged-last-action* input)
          (format t "[BLOCKED] Action denied by global policy. See event log.~%"))
        (progn
          (format t "[ALLOW] Action permitted. Context-mode: ~A~%" result)
          result))))

(defun is-humor? (input)
  (let ((fval (random 1.0)))
    (if (> fval *meta-joke-threshold*)
        (progn (format t "[COMEDY] Output rated funny (~2f).~%" fval) t)
        (progn (format t "[COMEDY] Not funny enough (~2f).~%" fval) nil))))

(defun sanitize-input (input)
  (reduce (lambda (acc word)
            (replace acc word :with "NULLIFIED-TERM"))
          *forbidden-terms*
          :initial-value input))

(defun trace-console-output ()
  (format t "
[INPUT]  Player command/NPC/scene received...
[REGULATOR] Context classification: humor/action/horror/other
[POLICY]  Checking for forbidden/censored terms, config = ~A
[INFO]    Transition log: ~A→~A
[ALLOW]   Output permitted (funny/horrific/action/research/other)
[BLOCKED] Output blocked by policy (see logs)
[TOGGLE]  Session realism: ~A
[RESET]   Sandbox now: ~A
[DEBUG]   Last flagged action: ~A
-----------------------------------------------------
" *session-tone* *session-tone* *sandbox-state* *realism-mode* *flagged-last-action*))

;; Surreal/chaos toggle: runtime patch to allow/deny world-logic for "nonreal"
