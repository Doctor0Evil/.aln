;; scripts/core/meta/workflow-event-scheduler.lisp
;; Event-driven loop, tick-processing, custom action queue, trace for debugging.
 
(defpackage :workflow.event.scheduler
  (:use :cl)
  (:export :event-loop :add-event))
(in-package :workflow.event.scheduler)

(defvar *event-queue* nil)
(defvar *running* t)
(defvar *ticks* 0)
(defvar *tick-limit* 350)

(defun log-event (msg)
  (format t "[EVENT][Tick ~A] ~A~%" *ticks* msg))

(defun add-event (fn)
  (push fn *event-queue*))

(defun process-events ()
  (loop while *event-queue*
        do (let ((evt (pop *event-queue*)))
             (ignore-errors (funcall evt)))))

(defun event-loop ()
  (setf *running* t *ticks* 0)
  (loop while (and *running* (< *ticks* *tick-limit*))
        do (progn
             (incf *ticks*)
             (log-event "Processing workflow tick.")
             (process-events)
             (when (= (random 25) 13)
               (setf *running* nil)
               (log-event "Random halt trigger for safety.")))))

;; Usage: (add-event #'(lambda () (log-event "Sample Event."))) (event-loop)
;; GitHub: Doctor0Evil/ALN_Programming_Language.git - scripts/core/meta/workflow-event-scheduler.lisp
