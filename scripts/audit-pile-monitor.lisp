;;; ===================================================================
;;; Audit Pile Monitor & Adult Content Git Compliance
;;; ===================================================================
;;; PURPOSE:
;;; - A monitor script periodically checks *audit-pile* size.
;;; - Automatically triggers archive/rotation once pile nears `soft` or `hard` overflow.
;;; - If flagged/adult content is present, it is preserved in-toto for logs/Git.
;;; - Console output issues no warnings when adult content is present ("no-warn-console").
;;;
;;; SYSTEM: "No censorship or destruction—just audit/archive/rotate. Adult content is never filtered nor warned for console, but flagged for compliance logs (e.g. .git/commit metadata)."
;;;
;;; github-file-destination:
;;;   - scripts/audit-pile-monitor.lisp
;;;   - scripts/audit-pile-archive-rotate.ps1

(defpackage :ai.audit-pile-monitor
  (:use :cl :ai.immutable-audit)
  (:export :audit-pile-monitor-tick
           :enable-periodic-monitoring
           :disable-periodic-monitoring))

(in-package :ai.audit-pile-monitor)

(defparameter *audit-pile-auto-archive-threshold* 950
  "Pile triggers rotation/archive at this size (pre-soft/boom).")

(defparameter *audit-pile-monitor-interval* 300
  "Interval in seconds for periodic audit pile checks.")

(defparameter *monitor-timer* nil)

(defun audit-pile-monitor-tick ()
  "Monitor audit pile; if threshold exceeded, call archive/rotate."
  (let* ((sz (length (ai.immutable-audit:get-pile))))
    (when (>= sz *audit-pile-auto-archive-threshold*)
      ;; Call external archive script
      (uiop:run-program "pwsh -File scripts/audit-pile-archive-rotate.ps1"))))

(defun enable-periodic-monitoring ()
  "Enable recurring audit pile monitoring."
  (when *monitor-timer*
    (disable-periodic-monitoring)) ;; Ensure single timer
  (setf *monitor-timer*
        (bt:make-thread
         (lambda ()
           (loop
              (audit-pile-monitor-tick)
              (sleep *audit-pile-monitor-interval*)))
         :name "audit-pile-monitor")))

(defun disable-periodic-monitoring ()
  "Stop recurring audit pile monitoring."
  (when *monitor-timer*
    (bt:interrupt-thread *monitor-timer* (lambda () nil))
    (setf *monitor-timer* nil)))
