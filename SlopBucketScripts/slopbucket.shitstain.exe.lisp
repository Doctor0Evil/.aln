;;;; SlopBucket Studios - Shitstain.EXE: Sesame Street Gone Feral
;;;; File: SlopBucketScripts/slopbucket.shitstain.exe.lisp

(defpackage :slopbucket-shitstain
  (:use :cl)
  (:export :run-sesame-street-chaos))
(in-package :slopbucket-shitstain)

(defparameter *characters*
  (list
    (list :name "Big Turd" :type "Turd Golem" :malfunction "Leaves streaks on RAM chips")
    (list :name "Groanaldo" :type "Groin Monster" :malfunction "Exposes itself during memory dumps")
    (list :name "Snufploopus" :type "Mutant Mammoth" :malfunction "Snorts meth from the console port")
    (list :name "Count Drugula" :type "Freaky Vampire" :malfunction "Counts how many lines of code before you puke")
    (list :name "Oscar the Slouch" :type "Dump-dweller" :malfunction "Launches trash-can DDoS attacks")
    (list :name "Elmonstrosity" :type "Flesh Puppet" :malfunction "Gnaws Ethernet cables—won’t shut up")))

(defun random-freak-action ()
  (nth (random 8)
   '("vomits on the input buffer"
     "executes a segfault ritual"
     "leaks acid onto user session"
     "chases children with a greasy sock puppet"
     "injects malware into the snack time routine"
     "breakdances in a pile of rat droppings"
     "worships the sideways fridge"
     "reads fanfic about the Great Cheeto Plague")))

(defun narrate-sick-chaos ()
  (dotimes (i 6)
    (let* ((char (nth (random (length *characters*)) *characters*))
           (name (getf char :name))
           (mal (getf char :malfunction))
           (action (random-freak-action)))
      (format t "~%[SHITSTAIN.LOG] ~A ~A and now ~A!~%" name mal action))))

(defun run-sesame-street-chaos ()
  (format t "~%[SHITSTAIN.EXE] - Welcome to SESAME STENCH STREET: FREAK MODE!~%")
  (narrate-sick-chaos)
  (format t "~%The End. You are now permabanned from PBS, and your soul is meat for Oscar the Slouch.~%"))

;;; Usage: (slopbucket-shitstain:run-sesame-street-chaos)
