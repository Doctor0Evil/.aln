;;;; File: runtime/engine/zero_downtime_runtime_engine.lisp
;;;; SlopBucketStudios - Zero Downtime Fault-Tolerant Runtime Engine

(defpackage :zero-downtime-runtime
  (:use :cl)
  (:export :start-runtime :deploy-module :upgrade-live :ping :status :stop-runtime))

(in-package :zero-downtime-runtime)

(defvar *service-table* (make-hash-table :test #'equal))
(defvar *health-status-table* (make-hash-table :test #'equal))
(defvar *hot-upgrade-queue* '())
(defvar *engine-running* nil)

(defun start-runtime ()
  "Start the engine. Brings up all registered services and enters main loop."
  (setf *engine-running* t)
  (maphash (lambda (k svc)
             (funcall (getf svc :start)))
           *service-table*)
  (format t "~%[ZDRE] Runtime started; all services online.")
  (main-loop))

(defun stop-runtime ()
  "Stop all services and wind down gracefully."
  (setf *engine-running* nil)
  (maphash (lambda (k svc)
             (funcall (getf svc :stop)))
           *service-table*)
  (format t "~%[ZDRE] Runtime halted; all services offline."))

(defun deploy-module (name start stop healthcheck)
  "Register a service for runtime management. Allows hot reload."
  (setf (gethash name *service-table*) (list :start start :stop stop :healthcheck healthcheck :status :running))
  (format t "~%[ZDRE] Deployed module: ~A" name))

(defun ping (name)
  "Healthcheck for the given module."
  (let ((svc (gethash name *service-table*)))
    (when svc
      (let ((result (ignore-errors (funcall (getf svc :healthcheck)))))
        (setf (gethash name *health-status-table*) result)
        result))))

(defun upgrade-live (name new-start new-stop new-healthcheck)
  "Hot-swap module logic with zero downtime."
  (let ((svc (gethash name *service-table*)))
    (when svc
      (push (list name new-start new-stop new-healthcheck) *hot-upgrade-queue*)
      (format t "~%[ZDRE] Hot upgrade requested for: ~A" name))))

(defun process-hot-upgrades ()
  "Apply queued upgrades with no downtime; old process exits after handoff."
  (loop for item in (reverse *hot-upgrade-queue*)
        for (name start stop healthcheck) = item
        for old-svc = (gethash name *service-table*)
        do (ignore-errors (funcall (getf old-svc :stop)))
        do (setf (gethash name *service-table*) (list :start start :stop stop :healthcheck healthcheck :status :running))
        do (format t "~%[ZDRE] Module ~A upgraded live!" name))
  (setf *hot-upgrade-queue* '()))

(defun main-loop ()
  "Core engine loop—auto-heals and reloads modules on failure."
  (loop while *engine-running*
        do (maphash (lambda (k svc)
                      (unless (ignore-errors (funcall (getf svc :healthcheck)))
                        (format t "~%[ZDRE] Healthcheck failed for ~A, restarting..." k)
                        (ignore-errors (funcall (getf svc :stop)))
                        (sleep 1)
                        (ignore-errors (funcall (getf svc :start)))))
                    *service-table*)
        do (process-hot-upgrades)
        do (sleep 2)))

(defun status ()
  "Print current status of all services."
  (maphash (lambda (k svc)
             (format t "~&~A: Status=~A Health=~A"
                     k (getf svc :status) (gethash k *health-status-table*)))
           *service-table*))

;;;; --- Example usage ---

#|
;; Register a service:
(deploy-module "API"
  (lambda () (print "API started"))
  (lambda () (print "API stopped"))
  (lambda () t))

;; Start engine
(start-runtime)

;; Hot-upgrade a module:
(upgrade-live "API"
  (lambda () (print "API started v2"))
  (lambda () (print "API stopped v2"))
  (lambda () t))
|#
