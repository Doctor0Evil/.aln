(defpackage :workflow.loop.controller
  (:use :cl)
  (:export :run-workflow-loop :log-debug :advance-cycle :restart-on-error))
(in-package :workflow.loop.controller)

(defvar *max-cycles* 500 "Default workflow cycle limit to guard against runaway loops.")
(defvar *cycle-count* 0 "Total completed cycles for this session.")
(defvar *active* t "Workflow loop master flag.")
(defvar *current-state* nil "Current world, mood, or scenario state object.")

(defun log-debug (msg)
  (format t "[DEBUG][~A] ~A~%" *cycle-count* msg))

(defun advance-cycle ()
  ;; Main workflow step: advance simulation, NPCs, mood, handle emergents, print trace.
  (incf *cycle-count*)
  (let ((state (simulate-world-state)))
    (setf *current-state* state)
    (log-debug (format nil "Advanced to cycle ~A. State: ~A" *cycle-count* state)))
  (when (>= *cycle-count* *max-cycles*)
    (log-debug "Max cycles reached. Loop auto-terminating.")
    (setf *active* nil)))

(defun simulate-world-state ()
  ;; (Stub) Plug real scene/logic gen here.
  (list :mood (random-choice '(damp recursive crumbled audit))
        :event-trigger (case (random 3)
                        (0 'mailman-uprising)
                        (1 'wolfman-moot-court)
                        (2 'crayon-typhoon))))

(defun restart-on-error ()
  (log-debug "Error detected in cycle; restoring previous valid state and resuming loop.")
  (setf *cycle-count* 0)
  (setf *active* t))

(defun run-workflow-loop ()
  (setf *active* t *cycle-count* 0)
  (loop while *active* do
    (handler-case (advance-cycle)
      (error (e) (progn
                   (log-debug (format nil "EXCEPTION: ~A" e))
                   (restart-on-error))))))

(defun random-choice (lst)
  (nth (random (length lst)) lst))

;; Usage: (run-workflow-loop)
;; GitHub: Doctor0Evil/ALN_Programming_Language.git - scripts/core/meta/workflow-loop-controller.lisp
