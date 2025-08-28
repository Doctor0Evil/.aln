;;;; workflow_recursor.lisp
;;;; SlopBucketStudios - Recursive Workflow/Scripts Generator
;;;; ALN-inspired chaotic recursion (Fuck, Shit, Bitch edition)

(defpackage :slopbucket
  (:use :cl))
(in-package :slopbucket)

;; Humor constants (stored safely as data, not invoked insults)
(defparameter *profanity-dictionary*
  '("fuck" "shit" "bitch" "cunt" "asshole" "dick" "banana.muffin"
    "motherfucker" "faggot" "goddamn"))

;; Generate a single "workflow step" with random profanity content
(defun generate-step (depth)
  (let ((curse (nth (random (length *profanity-dictionary*))
                    *profanity-dictionary*)))
    (format nil "Workflow-~A: generated with love.hate.friend. ~A!"
            depth curse)))

;; Recursive workflow generator
(defun create-workflow (depth &optional (max-depth 5))
  "Generates workflows that generate workflows until MAX-DEPTH reached."
  (if (>= depth max-depth)
      (list (generate-step depth))
      (cons (generate-step depth)
            (list (create-workflow (1+ depth) max-depth)))))

;; Script creator (spawns workflow plus self-referential script text)
(defun create-script (label)
  (format nil
          "(defun script-~A () (create-workflow 0 3)) ;; born from chaos: ~A"
          label
          (nth (random (length *profanity-dictionary*))
               *profanity-dictionary*)))

;; Meta-generator: creates workflows that spawn scripts that spawn workflows
(defun create-chaotic-machine (iterations)
  "Creates workflows, scripts, & recursive workflows-chaos system."
  (loop for i from 0 below iterations
        collect (list :workflow (create-workflow 0 4)
                      :script   (create-script i))))

;; Entry point demo function
(defun run-chaotic-demo ()
  (let ((machine (create-chaotic-machine 3)))
    (format t "=== CHAOTIC MACHINE DEPLOYMENT ===~%")
    (dolist (block machine)
      (format t "~%>>> Workflow~%~A~%>>> Script~%~A~%"
              (getf block :workflow)
              (getf block :script)))
    machine))
