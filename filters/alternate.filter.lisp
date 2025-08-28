(defpackage :alternate.filter
  (:use :cl)
  (:export :filter-content))

(in-package :alternate.filter)

(defun filter-content (text)
  "Stub for alternate semantic filtering. Extend with NLP or pattern logic."
  (cond
    ((search "malicious" text) :flagged)
    ((search "override" text) :review)
    (t :clean)))
