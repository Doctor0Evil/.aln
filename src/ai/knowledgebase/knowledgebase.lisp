;;;; ----------------------------------------------------------------
;;;; File: /src/ai/knowledgebase/knowledgebase.lisp
;;;; Purpose: Register, annotate, and mutate game math knowledgebase
;;;; ----------------------------------------------------------------

(defvar *knowledgebase-xp-logics* '()
  "Holds all historical and current XP math logic variants.")

(defun register-xp-math (description fn)
  (push (cons description fn) *knowledgebase-xp-logics*))

(defun mutate-register-example ()
  (register-xp-math
    "Additive XP (level +5 bonus, mutator)"
    (lambda (p t m) (round (+ (funcall #'default-xp-per-kill p t m) 5)))))
