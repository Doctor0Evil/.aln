;; scripts/core/meta/workflow-fsm-controller.lisp
;; Workflow loop as explicit finite state machine; robust state-transition logging.

(defpackage :workflow.fsm.controller
  (:use :cl)
  (:export :fsm-main-loop :fsm-transition :fsm-reset))
(in-package :workflow.fsm.controller)

(defvar *fsm-state* :init)
(defvar *fsm-cycle* 0)
(defvar *fsm-final-states* '(:terminated :fatal-error))

(defun fsm-log (msg)
  (format t "[FSM][Cycle ~A][State: ~A] ~A~%" *fsm-cycle* *fsm-state* msg))

(defun fsm-transition ()
  (case *fsm-state*
    (:init (setf *fsm-state* :running)
           (fsm-log "FSM initialized, entering :running"))
    (:running (incf *fsm-cycle*)
              (let ((evt (random (length '(a b c)))))
                (case evt
                  (0 (fsm-log "Routine event."))
                  (1 (setf *fsm-state* :error)
                     (fsm-log "Error detected; switching state."))
                  (2 (fsm-log "Continuing run..."))))
              (when (>= *fsm-cycle* 100)
                (setf *fsm-state* :terminated)))
    (:error (fsm-log "FSM in error state. Resetting.")
            (fsm-reset))
    (:terminated (fsm-log "FSM has safely terminated."))
    (:fatal-error (fsm-log "Unrecoverable error! Loop forcibly exited."))))

(defun fsm-reset ()
  (setf *fsm-state* :init
        *fsm-cycle* 0)
  (fsm-log "FSM reset. Ready to restart."))

(defun fsm-main-loop ()
  (loop until (member *fsm-state* *fsm-final-states*)
        do (fsm-transition)))

;; Usage: (fsm-main-loop)
;; GitHub: Doctor0Evil/ALN_Programming_Language.git - scripts/core/meta/workflow-fsm-controller.lisp
