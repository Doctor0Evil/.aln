;;; ==========================================================
;;; Humor Injection AI Override - Consolidated Runtime Control
;;; ==========================================================
;;; This Lisp compound file expresses all runtime logic, state
;;; mutation, humor override installation / uninstallation, 
;;; and audit-compatible hooks.

(defpackage :ai.humor-injection
  (:use :cl :ai.advanced-reasoning-core)
  (:export :install-humor-override
           :uninstall-humor-override
           :enable-humor-override-from-manifest
           :disable-humor-override
           :*humor-override-mode*
           :interpret-joke*))

(in-package :ai.humor-injection)

;; ==============================================================
;; State Variables
;; ==============================================================
(defparameter *humor-override-enabled* nil
  "Whether humor override is currently enabled.")

(defparameter *humor-override-mode* :passthrough
  "How humor output is transformed: :passthrough, :strict, :amplify, :force-funny.")

(defparameter *humor-override-seed* nil
  "Optional RNG seed for deterministic humor output.")

(defparameter *humor-audit-enabled* nil
  "If t, attempts to record audit logs into humor-audit module.")

;; ==============================================================
;; Internal Wrappers
;; ==============================================================

(defun interpret-joke* (joke &key context)
  "Override-wrapper for interpret-joke. Applies humor mode controls and
   optional audit logging."
  (let* ((raw (funcall (symbol-function 'ai.advanced-reasoning-core::interpret-joke) joke))
         (transformed (case *humor-override-mode*
                        (:passthrough raw)
                        (:strict (format nil "[STRICT] ~a" raw))
                        (:amplify (concatenate 'string raw " 😂"))
                        (:force-funny (concatenate 'string "[FORCE-FUNNY] " raw " 🤪"))
                        (otherwise raw))))
    (when *humor-audit-enabled*
      (ignore-errors
        (ai.advanced-reasoning-core.humor-audit:record-humor
         :joke joke
         :context context
         :result transformed
         :mode *humor-override-mode*
         :seed *humor-override-seed*)))
    transformed))

;; ==============================================================
;; Lifecycle Management
;; ==============================================================

(defun install-humor-override ()
  "Bind ai.advanced-reasoning-core:interpret-joke to local wrapper."
  (setf (symbol-function 'ai.advanced-reasoning-core:interpret-joke) #'interpret-joke*)
  (setf *humor-override-enabled* t))

(defun uninstall-humor-override ()
  "Restore original ai.advanced-reasoning-core::interpret-joke if available in shadow."
  ;; User must ensure original is accessible. For safety, no hard restore if missing.
  (when (fboundp 'ai.advanced-reasoning-core::interpret-joke-original)
    (setf (symbol-function 'ai.advanced-reasoning-core:interpret-joke)
          (symbol-function 'ai.advanced-reasoning-core::interpret-joke-original)))
  (setf *humor-override-enabled* nil))

(defun enable-humor-override-from-manifest (&optional (manifest-path "config/humor-modules.manifest.lisp"))
  "Load override configuration from manifest plist file."
  (let ((plist (with-open-file (in manifest-path)
                 (read in))))
    (when (getf plist :enable-override)
      (install-humor-override))
    (setf *humor-override-mode*  (or (getf plist :mode) :passthrough))
    (setf *humor-audit-enabled*  (getf plist :audit))
    (setf *humor-override-seed*  (getf plist :seed))
    (when *humor-override-seed* 
      (setf *random-state* (make-random-state t))
      (random (getf plist :seed))))))

(defun disable-humor-override ()
  "Disable humor override safely."
  (uninstall-humor-override)
  (setf *humor-override-enabled* nil))

;;; End of humor override module compound definition.
