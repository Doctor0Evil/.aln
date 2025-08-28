;; personality-matrix.lisp
;; Implements a mock calculator for the "h.aln.tytp" style vector input
;; Generates structured persona profiles

(defun personality-matrix-calculator (x-sensitive x y z o)
  "Compute personality cube from ALN-tytp input values."
  (let* ((rational-weight x)
         (emotional-weight y)
         (instinct-weight z)
         (override-flag o)
         (sensitivity x-sensitive))
    ;; Display calculated matrix
    (format t "~%--- Personality Cube Output ---~%")
    (format t "Rational Weight (x): ~a~%" rational-weight)
    (format t "Emotional Weight (y): ~a~%" emotional-weight)
    (format t "Instinct Layer (z): ~a~%" instinct-weight)
    (format t "Override Flag (o): ~a~%" override-flag)
    (format t "Sensitivity Scalar: ~a~%" sensitivity)
    ;; Persona Interpretation
    (cond
      ((and (= override-flag 1) (> instinct-weight emotional-weight))
       (format t "Persona: Grindhouse Rationalist (Logic + Instinct > Emotion).~%"))
      ((> emotional-weight rational-weight)
       (format t "Persona: Empathic-Driven Actor.~%"))
      (t (format t "Persona: Neutral-Logical Actor.~%")))
    ))

;; Example Execution
(personality-matrix-calculator 0.01 5 2 5 1)
