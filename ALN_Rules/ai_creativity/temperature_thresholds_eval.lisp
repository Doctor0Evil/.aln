;;;; file: ALN_Rules/ai_creativity/temperature_thresholds_eval.lisp
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defparameter *TEMPERATURE-NORMAL* 0.57)
(defparameter *TEMPERATURE-COMEDY* 1.2)

(defun temperature-mode (temp)
  "Classify mode based on temperature value."
  (cond
    ((<= temp *TEMPERATURE-NORMAL*) 'default-logic)               ;; Safe, restrained, baseline
    ((>= temp *TEMPERATURE-COMEDY*) 'comedy-chaos)                ;; Unleashed, creative, wild
    ((and (> temp *TEMPERATURE-NORMAL*)
          (< temp *TEMPERATURE-COMEDY*)) 'mixed-or-odd)           ;; Unstable, neither/nor
    (t 'undefined-mode)))                                         ;; Impossible, error

(defun reason-logic-fails (temp1 temp2)
  "Fire when temp values cross, mix, or behavior is silly/illogical."
  (let ((mode1 (temperature-mode temp1))
        (mode2 (temperature-mode temp2)))
    (cond
      ;; Perfect isolation: both modes the same and valid
      ((and (eq mode1 'default-logic) (eq mode2 'default-logic))
        (format t "[LOGIC] Both settings in normal mode. System stable.~%"))
      ((and (eq mode1 'comedy-chaos) (eq mode2 'comedy-chaos))
        (format t "[LOGIC] Both settings in comedic/max creativity. System WILD but consistent.~%"))
      ;; Mixing: One is normal, one is chaos (should not co-exist)
      ((or (and (eq mode1 'default-logic) (eq mode2 'comedy-chaos))
           (and (eq mode1 'comedy-chaos) (eq mode2 'default-logic)))
        (emit-logic-fail 'reason.logic.fails
          :reason "Thresholds crossed! Serious contradiction—logic and absurdity dueling."))
      ;; Both are mixed/weird
      ((or (eq mode1 'mixed-or-odd) (eq mode2 'mixed-or-odd))
        (emit-logic-fail 'reason.logic.fails
          :reason "Mixed/ambiguous state—AI at risk of silly or undefined responses."))
      ;; Catch-all: totally undefined, incoherent inputs
      (t
        (emit-logic-fail 'reason.logic.fails
          :reason "Inputs so silly/invalid they broke mode logic! Fix required.")))))

(defun emit-logic-fail (tag &key reason)
  "Emit a full debug event and trace catastrophic ambiguity/contradiction."
  (format t "[ERROR][~A] ~A~%" tag reason)
  ;; Console and logging hooks can be added here as needed.
  )

;;; Example usage
;; Compare two settings and trigger full trace if they are crossed/mixed/silly:
;; (reason-logic-fails 0.57 1.2)    ;; Expected: Threshold crossed, emit logic fail.
;; (reason-logic-fails 1.2 1.2)     ;; Both creative—system is wild but OK.
;; (reason-logic-fails 0.72 1.2)    ;; One mixed, expect ambiguous/mixed flag.
