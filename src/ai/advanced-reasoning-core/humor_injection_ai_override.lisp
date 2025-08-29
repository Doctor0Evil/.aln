;; AI.Advanced.Reasoning.Core.file
;; humor_injection_ai_override.lisp -- safe override injector for interpret-joke
;; Author: XboxTeeJay + Copilot
;; Purpose: Installable override that can amplify or pass through humor decisions
;;          without changing the original source. Deterministic, auditable, and
;;          manifest-driven.

(defpackage :ai.advanced-reasoning-core.humor-injection-override
  (:use :cl :alexandria)
  (:export
   :install-humor-override
   :uninstall-humor-override
   :enable-humor-override-from-manifest
   :disable-humor-override))
(in-package :ai.advanced-reasoning-core.humor-injection-override)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *humor-config-path* "config/humor-modules.manifest.lisp"
    "Path to an s-expression manifest. Example:
     '(:enable-override t :mode :amplify :audit t :seed 1337)"))

(defvar *override-installed* nil)
(defvar *original-interpret* nil "Holds original ai.advanced-reasoning-core::interpret-joke function.")
(defvar *override-mode* :passthrough)
(defvar *override-audit* t)
(defvar *override-seed* nil) ; when non-nil, used for deterministic behavior

(defun %find-interpret-symbol ()
  "Find the ai.advanced-reasoning-core::interpret-joke symbol even if not exported."
  (let ((pkg (find-package :ai.advanced-reasoning-core)))
    (multiple-value-bind (sym status) (find-symbol "INTERPRET-JOKE" pkg)
      (unless sym
        (error "Cannot find ai.advanced-reasoning-core::interpret-joke (status=~A)." status))
      sym)))

(defun %maybe-load-manifest (path)
  "Load a single s-expression from PATH if it exists. Returns a plist or NIL."
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (read in nil nil))))

(defun %plist-get (plist key &optional default)
  (or (getf plist key) default))

(defun %maybe-log (input result type seed &optional (note ""))
  "Try to call ai.advanced-reasoning-core.humor-audit:log-humor-result if available.
   Fails silently if audit module is absent or disabled."
  (when *override-audit*
    (let ((pkg (find-package :ai.advanced-reasoning-core.humor-audit)))
      (when pkg
        (multiple-value-bind (sym status)
            (find-symbol "LOG-HUMOR-RESULT" pkg)
          (declare (ignore status))
          (when (and sym (fboundp sym))
            (ignore-errors
              (funcall (symbol-function sym)
                       (format nil "~A ~A" input note)
                       result type seed))))))))

(defun %rng (seed)
  "Create a deterministic random-state when SEED is provided."
  (if seed
      (make-random-state (make-random-state t)) ; isolate
      *random-state*))

(defun %coerce-type (maybe-type input)
  "Ensure we have a humor type. If NIL, try classifier."
  (or maybe-type
      (let ((pkg (find-package :ai.advanced-reasoning-core.humor-classifier)))
        (when pkg
          (multiple-value-bind (sym status)
              (find-symbol "CLASSIFY-HUMOR-TYPE" pkg)
            (declare (ignore status))
            (when (and sym (fboundp sym))
              (ignore-errors (funcall (symbol-function sym) input))))))))

(defun %wrapper (orig-fn mode audit seed)
  "Return a wrapper function around ORIG-FN honoring MODE, AUDIT, and SEED."
  (lambda (input)
    (let* ((seed* (or seed (ignore-errors (parse-integer (or (uiop:getenv "HUMOR_SEED") "")))))
           (rng (%rng seed*))
           (note (if seed* (format nil "[seed=~A]" seed*) ""))
           (mv (multiple-value-list (funcall orig-fn input)))
           (base-result (first mv))
           (base-type (second mv))
           (type (%coerce-type base-type input)))
      (labels ((emit (res typ msg)
                 (when msg (format t "~A~%" msg))
                 (%maybe-log input res (or typ :unknown) (or seed* :auto) note)
                 (if typ
                     (values res typ)
                     res)))
        (ecase mode
          (:passthrough
           (emit base-result type nil))
          (:strict
           ;; Never up-rank. If original said :funny, pass through; else enforce :not-funny.
           (if (eq base-result :funny)
               (emit :funny type nil)
               (emit :not-funny (or type :n/a)
                     (format nil "Override: enforcing strict mode ~A" note))))
          (:amplify
           ;; If base considered not-funny, selectively up-rank friendly categories.
           (if (eq base-result :funny)
               (emit :funny type nil)
               (let ((eligible (member type '(:pun :meta :knock-knock :setup-punchline)
                                       :test #'eq)))
                 (if eligible
                     (let* ((boost (+ (random 100 rng) 1.337)))
                       (emit :funny (or type :unknown)
                             (format nil "Override: humor amplified ✓ [Type: ~A] [Boost: ~,3F] ~A"
                                     type boost note)))
                     (emit :not-funny (or type :n/a)
                           (format nil "Override: not eligible for amplify [Type: ~A] ~A" type note))))))
          (:force-funny
           (let* ((boost (+ (random 100 rng) 42.0)))
             (emit :funny (or type :unknown)
                   (format nil "Override: force-funny ✓ [Type: ~A] [Boost: ~,3F] ~A"
                           type boost note)))))))))

(defun install-humor-override (&key (mode :amplify) (audit t) seed)
  "Install the override wrapper around ai.advanced-reasoning-core::interpret-joke.
   MODE is one of :passthrough :strict :amplify :force-funny."
  (when *override-installed*
    (return-from install-humor-override :already-installed))
  (let* ((sym (%find-interpret-symbol))
         (orig (symbol-function sym))
         (wrap (%wrapper orig mode audit seed)))
    (setf *original-interpret* orig
          (symbol-function sym) wrap
          *override-installed* t
          *override-mode* mode
          *override-audit* audit
          *override-seed* seed)
    (format t "Humor override installed [mode=~A audit=~A seed=~A].~%" mode audit seed)
    :installed))

(defun uninstall-humor-override ()
  "Restore the original interpret-joke function if installed."
  (unless *override-installed*
    (return-from uninstall-humor-override :not-installed))
  (let ((sym (%find-interpret-symbol)))
    (setf (symbol-function sym) *original-interpret*
          *override-installed* nil
          *original-interpret* nil)
    (format t "Humor override uninstalled.~%")
    :uninstalled))

(defun enable-humor-override-from-manifest (&optional (path *humor-config-path*))
  "Load config plist and install the override accordingly.
   Example manifest:
   '(:enable-override t :mode :amplify :audit t :seed 1337)"
  (let* ((cfg (%maybe-load-manifest path))
         (enable (%plist-get cfg :enable-override t))
         (mode   (%plist-get cfg :mode :amplify))
         (audit  (%plist-get cfg :audit t))
         (seed   (%plist-get cfg :seed nil)))
    (if (not enable)
        (progn
          (when *override-installed* (uninstall-humor-override))
          (format t "Humor override disabled via manifest.~%")
          :disabled)
        (install-humor-override :mode mode :audit audit :seed seed))))

(defun disable-humor-override ()
  "Convenience alias for uninstall."
  (uninstall-humor-override))
