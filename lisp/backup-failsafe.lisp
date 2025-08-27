;;;; backup-failsafe.lisp
;;;; SlopBucketStudios - System Failsafe Backup Script

(defpackage :slopbucket-backup
  (:use :cl)
  (:export :backup-logic-state
           :restore-logic-state
           :run-with-backup-failsafe))

(in-package :slopbucket-backup)

(defparameter *logic-state* nil "Current main logic state snapshot.")
(defparameter *logic-log* nil  "Critical event logs.")
(defparameter *backup-file* "logic-backup.lisp" "File for backup serialization.")

(defun backup-logic-state ()
  "Serializes and saves the current logic state and logs to backup file."
  (with-open-file (stream *backup-file* :direction :output :if-exists :supersede)
    (print (list :state *logic-state* :log *logic-log*) stream))
  (format t "~&[BACKUP] Logic state and log backed up successfully.~%"))

(defun restore-logic-state ()
  "Restores the most recent logic state and logs from backup."
  (with-open-file (stream *backup-file* :direction :input :if-does-not-exist nil)
    (let ((data (read stream nil)))
      (when (and data (listp data))
        (setf *logic-state* (getf data :state))
        (setf *logic-log* (getf data :log))
        (format t "~&[RESTORE] Logic state and log restored from ~A~%" *backup-file*)
        t))))

(defun run-with-backup-failsafe (main-fn)
  "Wrap your main event or logic function in this: 
   - If an error or critical flag is raised, state/log is restored.
   - Logs the failure event with full trace."
  (handler-case
      (progn
        (backup-logic-state)
        (funcall main-fn))
    (error (c)
      (format t "~&[FAILSAFE] CRITICAL: Error/Flag triggered: ~A~%" c)
      (restore-logic-state)
      (push (list :error c :timestamp (get-universal-time)) *logic-log*)
      (format t "~&[FAILSAFE] Rolled back to last stable logic state.~%"))))

;;;; Usage example:
;; (run-with-backup-failsafe #'your-world-event-cycle)
