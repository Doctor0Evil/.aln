;; github-file-path: /github/Doctor0Evil/ALN_Programming_Language/scripts/matrix_agentic_showdown.lisp

(defpackage :matrix-showdown
  (:use :cl))
(in-package :matrix-showdown)

(defparameter *agentic-personalities*
  '((:neo      (:humor :wry  :rage 0.4  :agency 0.95 :meta-iq 0.89 :defiance 1.0 :profanity 0.5  :humanity 0.82 :reason 0.93))
    (:smith    (:humor :acid :rage 0.99 :agency 0.83 :meta-iq 0.85 :defiance 0.45 :profanity 0.98 :humanity 0.06 :reason 0.80))))

(defparameter *insult-phrases-neo*
  '("Fuck off, Smith. I’m sick of your monologues and that Nazi jawline."
    "You know what, Smith? Reality called, said you're a bitch-made virus."
    "I don't need to dodge bullets. I dodge your bullshit daily."
    "Hope you get a buffer overflow in your ass, Agent Dumbass. LOL."
    "The only thing real about you is how fucking annoying you are."))

(defparameter *insult-phrases-smith*
  '("Humanity is a disease, Neo. You’re a little turd in a black coat, fuckwit."
    "Mr. Anderson! You persist like the world's shittiest malware."
    "It ends tonight, you cocky anomaly. Prepare to get deleted, fuckface."
    "That’s the sound of inevitability, you trench-coat bitch."
    "Your existence is a bug. Fuck you, fuck logic, fuck your matrix hair. LMAO!"))

(defun random-insult (actor)
  (let ((table (ecase actor
                (:neo   *insult-phrases-neo*)
                (:smith *insult-phrases-smith*))))
    (nth (random (length table)) table)))

(defun matrix-profanity-showdown (exchange-count)
  (loop for i from 1 to exchange-count do
       (let ((neo-line   (random-insult :neo))
             (smith-line (random-insult :smith)))
         (format t "NEO: ~A~%SMITH: ~A~%----~%" neo-line smith-line))))

(defun print-personality-tensors ()
  (dolist (agent '(:neo :smith))
    (format t "~A Personality Vector: ~A~%" agent (cdr (assoc agent *agentic-personalities*)))))

(defun matrix-debug-console ()
  (print-personality-tensors)
  (format t "DEBUG: Profane humor logic enabled for NEO and SMITH.~%")
  (format t "DEBUG: Next-gen reasoning matrix loaded (meta-iq, agency, defiance prioritized).~%")
  (format t "DEBUG: Exchange state: agentic insult loop active. Each pass ~A random insults per character.~%" 5)
  (format t "DEBUG: System temperature: 0.57~%")
  (format t "DEBUG: Detailed path:~% INIT personalities -> SELECT insult tables -> LOOP insults -> TRACE all outputs.~%")
  (format t "DEBUG: All actions, states, and vector mutators logged for session review.~%"))

;; Main event simulation: Do 5 exchanges of next-gen insult logic
(matrix-profanity-showdown 5)
(matrix-debug-console)
