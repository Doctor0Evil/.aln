;; AI.Advanced.Reasoning.Core.file
;; humor-classifier.lisp -- humor type tagging extension
;; Author: XboxTeeJay + Copilot
;; Purpose: Classify input text into humor categories for downstream handling.

(defpackage :ai.advanced-reasoning-core.humor-classifier
  (:use :cl :alexandria)
  (:export :classify-humor-type))
(in-package :ai.advanced-reasoning-core.humor-classifier)

(defun classify-humor-type (input)
  "Return a humor category keyword for INPUT."
  (cond
    ;; Simple regex/string matching here; could be swapped for ML-based classifier
    ((search "pun" (string-downcase input)) :pun)
    ((or (search "knock" (string-downcase input))
         (search "who's there" (string-downcase input))) :knock-knock)
    ((search "walks into a bar" (string-downcase input)) :setup-punchline)
    ((search "dark" (string-downcase input)) :dark-humor)
    ((search "meta" (string-downcase input)) :meta)
    (t :unknown)))
