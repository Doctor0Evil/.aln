(defpackage :slopbucket-sim
  (:use :cl)
  (:export :run-slopbucket-cycle :set-dog-action))

(in-package :slopbucket-sim)

;; Core Henchmen & Empire State
(defparameter *dog-state* 'chewing-toy)
(defparameter *dog-trust* 35)
(defparameter *dog-aggression* 20)
(defparameter *dog-mood* 'calm)
(defparameter *dog-loyalty* 20)
(defparameter *world-mood* (random-choice '(chaos feral neutral dream-state)))
(defparameter *npcs* nil)
(defparameter *log* nil)

;; Log Function
(defun log-s (tag msg &rest args)
  (let ((entry (format nil "~A ~A" (string-upcase (symbol-name tag)) (apply #'format nil msg args))))
    (push entry *log*)
    (format t "~A~%" entry)))

;; Criminal Empire Loader
(defun run-slopbucket-cycle ()
  (log-s 'init "SLOPBUCKET SESSION INITIATED")
  (log-s 'status "DogState:~A Trust:~A Aggression:~A Mood:~A Loyalty:~A"
         *dog-state* *dog-trust* *dog-aggression* *dog-mood* *dog-loyalty*)

  (simulate-dog-event)
  (inject-chaos-npc)
  (log-s 'world "WorldMood:~A" *world-mood*)
  (log-s 'finish "END OF CYCLE")
  (reverse *log*))

;; Henchmen Generation
(defun inject-chaos-npc ()
  (let* ((name (format nil "NPC-~3,'0d" (random 999)))
         (quirk (random-choice '(flesh-tuba bootlicker mailbox-priest pipe-chatter)))
         (madness (random 100))
         (line (random-choice '("Bread with veins is breakfast of champions."
                                "My quirk? Screaming at rotting knives!"
                                "Bootlicking won't save your harddrive."))))
    (log-s 'npc "~A quirk:~A madness:~A says:~A" name quirk madness line)))
