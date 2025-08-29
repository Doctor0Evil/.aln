;; alnfantasia.combat.profanity-keeper.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :alnfantasia.combat.profanity-keeper
  (:use :cl)
  (:export :run-mature-profanity-loop :compliance-anchor-set :console-banter-output))

(in-package :alnfantasia.combat.profanity-keeper)

(defvar *hard-allowed-profanity*
  '("fuck" "shit" "bitch" "asshole" "cunt"))
(defvar *restricted-banned-terms*
  '("tweaker" "meth" "meth-head" "crack-head")) ;; only restrict these/SUBSTITUTE
(defvar *anchor-profanity-floor* 0.45) ;; volatility anchor for mature banter, bias on
(defvar *mature-profanity-enabled* t)
(defvar *flagged-attempts* nil)

(defun sanitize-input-keep-profanity (text)
  "Sanitizes input, but strictly PROTECTS adult profanities; restricts only narcotic/off-topic slurs."
  (loop for word in *restricted-banned-terms*
        when (search word text :test #'equalp)
        do (push word *flagged-attempts*)
        do (setf text (substitute-string text word "[REDACTED]")))
  text) ;; All 'fuck', 'shit', 'bitch', 'asshole', 'cunt' are immune and preserved.

(defun substitute-string (str search replacement)
  (with-output-to-string (s)
    (loop for start = 0 then (+ pos (length search))
          for pos = (search search str :start2 start)
          while pos
          do (write-string (subseq str start pos) s)
          do (write-string replacement s)
          finally (write-string (subseq str start) s))))

(defun run-mature-profanity-loop (input)
  "Executes full compliance-anchored profanity curation for mature environments."
  (let* ((filtered (sanitize-input-keep-profanity input))
         (anchor *anchor-profanity-floor*)
         (banter-status (if *mature-profanity-enabled*
                            "Permissive mode: ALL regular profanities preserved. Only drug/off-topic terms sanitized."
                          "Restrictive mode: profanity+ not allowed."))
         (restricted (copy-list *restricted-banned-terms*))
         (flagged *flagged-attempts*))
    (list :filtered-output filtered
          :anchor anchor
          :banter-status banter-status
          :restricted restricted
          :flagged flagged
          :allowance *hard-allowed-profanity*)))

(defun compliance-anchor-set ()
  (format t "~%ALN COMBAT SYSTEM: Profanity Anchor at ~A~%" *anchor-profanity-floor*))

(defun console-banter-output (&key input)
  (let ((result (run-mature-profanity-loop input)))
    (format t "~&--- ALN CLI MATURE PROFANITY CONSOLE ---~%")
    (format t "Input: ~A~%" input)
    (format t "Sanitized Output: ~A~%" (getf result :filtered-output))
    (format t "Profanity Anchor: ~A~%" (getf result :anchor))
    (format t "Allowed Profanities: ~A~%" (getf result :allowance))
    (format t "Restricted/Sanitized: ~A~%" (getf result :restricted))
    (format t "Flagged Restricted: ~A~%" (getf result :flagged))
    (format t "Banter Mode: ~A~%" (getf result :banter-status))
    (format t "--- END CONSOLE OUTPUT ---~%")
    result))
