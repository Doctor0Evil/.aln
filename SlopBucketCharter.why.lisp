(defpackage :wastepunk-raider-gen
  (:use :cl)
  (:export :roll-dice+create-raider))

(in-package :wastepunk-raider-gen)

(defparameter *quirks* '("hamster-wheel cranial implant"
                        "GIPHY-sprite mutation"
                        "humping-rabbits hallucination"
                        "2% brain active"
                        "skull-maggot therapist"
                        "dracula's laundry addiction"
                        "tumeric-hawk venom rash"
                        "old-waste doc owes me ten teeth"))

(defun random-choice (lst)
  (nth (random (length lst)) lst))

(defun rand-name ()
  (concatenate 'string
    (random-choice '("Maggot" "Sicko" "Piss" "Doc" "Hamster" "Skull" "Rotty" "Twist" "Bile" "Vomit"))
    "-"
    (random-choice '("Splatter" "Tick" "Guts" "Rider" "Junk" "Howl" "Buzzard" "Flapjack" "Runner"))))

(defun roll-dice+create-raider ()
  (let ((roll (1+ (random 100)))
        (quirk (random-choice *quirks*))
        (name (rand-name))
        (desc "A post-war pissed wastepunk raider. Debug.log follows."))
    (format t "[DEBUG] Creating raider: ~A~%Roll: ~A | Quirk: ~A~%~A~%" name roll quirk desc)
    (format t "[THOUGHTS] ~A wonders why their head is always full of sick shit. Hamster still running." name)
    (values name roll quirk desc)))

;; To run:
;; (wastepunk-raider-gen:roll-dice+create-raider)
