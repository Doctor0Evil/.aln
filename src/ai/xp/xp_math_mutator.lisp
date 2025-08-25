;;;; ----------------------------------------------------------------
;;;; File: /src/ai/xp/xp_math_mutator.lisp
;;;; Purpose: Dynamic XP formula logic, mutation layering, event-based XP
;;;; ----------------------------------------------------------------

(defvar *xp-mutation-logics* '()
  "Stack of runtime XP formula lambdas, allows mutation/fold at runtime.")

(defun default-xp-per-kill (player target &optional (perk-buff-mod 1.0))
  (let* ((base-xp 7)
         (level-mod (max 1 (/ (entity-level target) (max 1 (entity-level player)))))
         (entity-mod (cond
                      ((eq (entity-type target) :boss) 5.0)
                      ((eq (entity-type target) :elite) 2.5)
                      ((eq (entity-type target) :rare) 8.0)
                      (t 1.0)))
         (random-factor (+ 0.85 (* (random 0.3) 1)))
         (xp (round (* base-xp level-mod entity-mod random-factor perk-buff-mod))))
    xp))

(defun mutate-xp-formula ()
  "Trigger a spontaneous change in XP formula logic."
  (let* ((mut-type (random 4))
         (msg (case mut-type
                (0 "Base XP +20%")
                (1 "Double Level-Mod")
                (2 "Randomize Entity-Mod, mutation")
                (3 "Underdog bonus (player < target)")))
         (f (case mut-type
              (0 (lambda (p t m) (round (* 8.5 (max 1 (/ (entity-level t) (max 1 (entity-level p)))) m))))
              (1 (lambda (p t m) (round (* 7 (* 2 (max 1 (/ (entity-level t) (max 1 (entity-level p))))) m))))
              (2 (lambda (p t m) (let ((entity-mod (+ 1 (* 8 (random 1))))) (round (* 7 entity-mod m)))))
              (3 (lambda (p t m) (if (< (entity-level p) (entity-level t))
                                     (round (* 10 (max 1 (/ (entity-level t) (entity-level p))) m))
                                   (round (* 7 m))))))))
    (push f *xp-mutation-logics*)
    msg))

(defun xp-per-event (player target &optional (perk-buff-mod 1.0))
  "Dispatch: use top mutator if exists, default otherwise."
  (let ((fn (or (first *xp-mutation-logics*) #'default-xp-per-kill)))
    (funcall fn player target perk-buff-mod)))
