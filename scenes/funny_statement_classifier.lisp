;; "https://github.com/Doctor0Evil/ALN_Programming_Language.git"
;; scenes/funny_statement_classifier.lisp

(defpackage :funny-classifier
  (:use :cl)
  (:export :classify-funny))

(in-package :funny-classifier)

(defun classify-funny (statement)
  (let* ((incongruity-score (detect-incongruity statement))
         (truth-score (detect-truth kernel statement))
         (misdirection-score (detect-misdirection statement))
         (audience-response (simulate-laugh-track statement)))
    (if (> (+ incongruity-score truth-score misdirection-score audience-response) 2.5)
        'funny.statement
        'not.funny)))
