;;;; ----------------------------------------------------------------
;;;; File: /src/ai/xp/xp_mutation_stack.lisp
;;;; Purpose: Maintain mutation/fold history for XP formulae
;;;; ----------------------------------------------------------------

(defvar *xp-mutation-stack* '()
  "Record all XP math logic fold events for audit/debug.")

(defun log-xp-mutation (desc)
  (push (list :time (get-universal-time)
              :type :xp-mutate
              :desc desc)
        *xp-mutation-stack*))
