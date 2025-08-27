;;;; sloppy-seconds.lisp
;;;; "Director’s Cutting Board" Horror-Comedy Editing Runtime

(defpackage :slopbucket.cinema
  (:use :cl)
  (:export :inject-horror-comedy))

(in-package :slopbucket.cinema)

(defvar *horror-scenes*
  '("Chainsaw revs, but instead of blood, nacho cheese sprays everywhere."
    "Zombie moans: 'BRAAAINS!'… then slips on Taco Bell floor grease."
    "Clown with knife trips, kills himself, audience laughs awkwardly."
    "R2D2 head stuffed on a pike—spins and shouts insults in binary!"
    "Grandma’s front yard: three severed heads arguing about rent money."))

(defvar *comedy-overlays*
  '("Studio laugh-track plays at ALL the wrong moments."
    "Boom mic falls into frame, dripping in chili sauce."
    "Director yells 'CUT!' but camera keeps rolling; actor farts loudly."
    "Dog walks in, says 'this script sucks,' then leaves."
    "The gore budget clearly spent on extra tacos instead of makeup."))

(defun random-clip (pool) (nth (random (length pool)) pool))

(defun inject-horror-comedy (&optional (tempo 1))
  "Emit a slop-scene by injecting horror AND comedy overlays with delay factor."
  (let ((scene (random-clip *horror-scenes*))
        (overlay (random-clip *comedy-overlays*)))
    (sleep tempo)
    (format t "~%[HORROR] ~a" scene)
    (sleep tempo)
    (format t "~%[COMEDY] ~a~%" overlay)))
