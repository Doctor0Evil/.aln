;;;; SlopBucket Studios - Monsterpiece Funny Sitcom Script
;;;; File: SlopBucketScripts/slopbucket.monsterpiece.funny.lisp
;;;; LICENSE: Unhinged
(defpackage :slopbucket-monsterpiece
  (:use :cl)
  (:export :run-monsterpiece-day))
(in-package :slopbucket-monsterpiece)

(defparameter *debug-guts* t)
(defun debug-log (msg &rest args)
  (when *debug-guts*
    (format t "~%MONSTERPIECE.DEBUG> " msg args)
    (terpri)))

(defstruct (character (:conc-name char-))
  name quirks malfunction)

(defun random-glitch ()
  (nth (random 5)
    '("vomits sparkles everywhere!"
      "CPU melts down with a yelp."
      "broadcasts fisting techniques to urinal server."
      "spills Taco Bell menu onto RAM."
      "starts speaking in ASCII-only haikus.")))

(defun narrate-chaos (char action)
  (debug-log "Narrating glitch for ~A: ~A"
             (char-name char) action)
  (format t "~%~A attempts to ~A, but suddenly ~A~%"
          (char-name char) action (random-glitch)))

(defun unleash-monsterpiece (characters actions iterations)
  (debug-log "Monsterpiece BeastMode: ~A actors, ~A actions, ~A cycles."
             (length characters) (length actions) iterations)
  (dotimes (i iterations)
    (let ((char (nth (random (length characters)) characters))
          (action (nth (random (length actions)) actions)))
      (narrate-chaos char action))))

(defun run-monsterpiece-day ()
  (let ((cast (list (make-character :name "S.Barks!" :quirks "Cyber-Great-Dane" :malfunction "Hands out ass-beatings randomly")
                    (make-character :name "Captain Cupcakes" :quirks "Sugar demon" :malfunction "Reboots into disco mode anytime")
                    (make-character :name "Trashcan R2.DOOM" :quirks "Shit-talking bin" :malfunction "Spills user guts across the floor")))
        (actions '("give a TED talk about licking balls"
                   "hack the main urinal.server"
                   "challenge Mike Tyson to a dog show fight"
                   "lick own balls in public"
                   "start a war over expired burritos")))
    (debug-log "Launching Slopbucket Monsterpiece Sitcom of the Day!")
    (unleash-monsterpiece cast actions 8)
    (debug-log "End of Monsterpiece. Console open for janitors.<reset>")))

;;; Usage: (slopbucket-monsterpiece:run-monsterpiece-day)
