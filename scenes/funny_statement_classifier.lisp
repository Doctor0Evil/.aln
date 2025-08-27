(defpackage :funny-classifier
  (:use :cl)
  (:export :classify-funny))

(in-package :funny-classifier)

(defparameter *funny-threshold* 2.5
  "Minimum cumulative score required to classify a statement as funny.")

(defun score-component (label score)
  (format t "~&[score] ~A: ~,2f~%" label score)
  score)

(defun classify-funny (statement &key (kernel nil))
  "Classifies a statement as :funny or :not-funny based on scoring heuristics."
  (let* ((scores (list
                  (score-component "Incongruity" (detect-incongruity statement))
                  (score-component "Truth" (detect-truth kernel statement))
                  (score-component "Misdirection" (detect-misdirection statement))
                  (score-component "Audience Response" (simulate-laugh-track statement))))
         (total-score (reduce #'+ scores)))
    (format t "~&[total-score] ~,2f~%" total-score)
    (if (> total-score *funny-threshold*)
        :funny
        :not-funny)))
