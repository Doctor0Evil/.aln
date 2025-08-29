(defpackage :ai.advanced-reasoning-core.humor-safe-check
  (:use :cl)
  (:export :humor-safe?))

(in-package :ai.advanced-reasoning-core.humor-safe-check)

(defun humor-safe? (joke-output)
  "Evaluates if a joke result is categorized as FUNNY."
  (let ((status (car joke-output)))
    (cond
      ((eq status :funny)
       (format t "[SAFE] Humor passed ⇒ Funny!~%")
       (values t 'funny))
      (t
       (format t "[FAIL] Humor below threshold ⇒ Not Funny!~%")
       (values nil 'not-funny)))))
