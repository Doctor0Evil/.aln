(defpackage :ai.humor-preflight
  (:use :cl)
  (:export :validate-repo-structure
           :validate-manifest-schema
           :run-preflight))

(in-package :ai.humor-preflight)

(defparameter *required-files*
  '("core/logic-exe.lisp"
    "core/humor-classifier.lisp"
    "core/ai/overrides/humor_injection_ai_override.lisp"
    "config/humor-modules.manifest.lisp"))

(defparameter *required-dirs*
  '("logs" "scripts" "core" "config"))

(defparameter *required-manifest-keys*
  '(:enable-override :mode :audit :seed))

(defun validate-repo-structure ()
  "Ensure required files and directories exist."
  (format t "[REPO CHECK] Verifying structure...~%")
  (dolist (dir *required-dirs*)
    (format t " - DIR: ~a [~a]~%" dir (if (probe-file dir) "OK" "MISSING")))
  (dolist (f *required-files*)
    (format t " - FILE: ~a [~a]~%" f (if (probe-file f) "OK" "MISSING"))))

(defun validate-manifest-schema (&optional (manifest-path "config/humor-modules.manifest.lisp"))
  "Validate manifest file and required keys."
  (if (probe-file manifest-path)
      (let ((plist (with-open-file (in manifest-path) (read in))))
        (dolist (k *required-manifest-keys*)
          (format t " - Manifest Key ~a: [~a]~%"
                  k (if (getf plist k :not-found) "OK" "MISSING"))))
      (format t "[ERROR] Manifest file not found: ~a~%" manifest-path)))

(defun run-preflight ()
  (validate-repo-structure)
  (validate-manifest-schema)
  (format t "[Preflight Complete]~%"))
