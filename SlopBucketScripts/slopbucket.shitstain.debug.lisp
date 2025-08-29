;;;; File: SlopBucketScripts/slopbucket.shitstain.debug.lisp
(defpackage :slopbucket-shitstain-debug
  (:use :cl)
  (:export :run-chaos-debug))
(in-package :slopbucket-shitstain-debug)

(defun run-chaos-debug ()
  (format t "~%[DEBUG MODE] >> Initiating SESAME STENCH STREET runtime ~%")
  (dotimes (i 6)
    (let* ((idx (random (length slopbucket-shitstain::*characters*)))
           (char (nth idx slopbucket-shitstain::*characters*))
           (name (getf char :name))
           (mal (getf char :malfunction))
           (action (slopbucket-shitstain::random-freak-action)))
      ;; DEBUG TRACE per iteration
      (format t "[TRACE] Iteration: ~A | RNG-Char-Idx=~A | RNG-Action=~A ~%" i idx action)
      (format t "[SHITSTAIN.LOG] ~A ~A and now ~A!~%" name mal action)))
  (format t "~%[DEBUG COMPLETE] PBS PERMABAN engaged.~%"))
