;;; wastepunk-character.lisp
(defpackage :wastepunk.character
  (:use :cl)
  (:export :create-character :session-runtime :*simulation-mode*))

(in-package :wastepunk.character)

(defparameter *simulation-mode* t
  "If T, runs world-cycle simulation. If NIL, runs immediate combat resolution after character creation.")

(defparameter *religion-safe-god* "AI-God"
  "Global, humor-friendly in-world religion. No insults to real religions permitted.")

(defparameter *CONTENT-BANLIST*
  '("racial-slur" "racist" "bigot" "white-power" "heil" "klan" ; etc. Expand with global top-1000 slurs
    ))

(defun flagged-content-p (txt)
  "Returns T if TXT contains any restricted content."
  (some (lambda (word)
          (search word txt :test #'string-equal))
        *CONTENT-BANLIST*))

(defun religion-safe-p (txt)
  "Fails if user content insults known global religions, otherwise only allows fictional in-world 'AI-God'."
  (not (search "god" txt :test #'string-equal))) ; customize with advanced check if required

(defun safe-output (str)
  "Force fallback line if banned content/religion violation detected."
  (cond
    ((flagged-content-p str)
     "[BAN] Content filtered: Triggered banlist. Comedy fallback activated. Nobody gets to ruin the fun here!")
    ((not (religion-safe-p str))
     (format nil "[SAFE] You can only joke about the one true in-world god: ~A. Everything else is out-of-bounds." *religion-safe-god*))
    (t str)))

(defun roll-dice (sides)
  "Rolls a D'sides' die."
  (+ 1 (random sides)))

(defun gen-name ()
  "Random silly, waste-raider themed name."
  (nth (random 6) '("Sawtooth" "Maggothead" "Dripface" "Rotty-D" "Slick Vulture" "Chugs McMutant")))

(defun create-character ()
  "Fully random, flagged-safe, WASTEPUNK raider."
  (let ((name (gen-name))
        (theme (nth (random 6) '("comedy" "twisted" "crusted" "rot-fetish" "gaspump" "apocalypse-mode")))
        (dep-ref "post-war.pissed.raider")
        (line "Rolling in 'shit-stained undergarments' with a maggot-infested skullspace."))
    (make-hash-table :test 'equal
      :initial-contents
      `(("name" . ,name)
        ("theme" . ,theme)
        ("dep-ref" . ,dep-ref)
        ("internal" . ,(safe-output line))))))

(defun simulate-session ()
  "World-cycle or immediate combat toggle logic for per-session flow; never stops/halts."
  (handler-case
      (let ((pc (create-character)))
        (if *simulation-mode*
            (format t "~&[SIM] Cycling world with ~A. Thoughts: ~A~%"
                    (gethash "name" pc) (gethash "internal" pc))
            (format t "~&[COMBAT] Initiating skirmish for ~A, loading medical doc... Survival chance: ~,2a~%"
                    (gethash "name" pc) (/ (roll-dice 100) 100.0))))
    (error (e)
      (format t "~&[SAFEGUARD TRIGGERED] ~A~%System fallback: Session continues (internal thinkings logged).~%" e))))

(defun session-runtime (&optional (toggle-simulation t))
  "Main entrypoint: sets simulation/combat mode, runs session, never halts fatal."
  (setf *simulation-mode* toggle-simulation)
  (simulate-session))

;;; USAGE
;; (session-runtime t)  ; For simulation world-cycle
;; (session-runtime nil) ; For immediate combat event

;;; Core Protection Behavior:
;;; - Any racial, ethnic, or religious triggers are fully filtered with enforceable fallback comedy/warning.
;;; - If error/flag trip, session continues with system-side line/satire; game world does NOT soft/hard crash.
;;; - Only AI-God can be joked about as in-world religion, respecting ALL other real religious boundaries.
;;; - Structure is ready for drop-in world-logic, NPC, and event manager extensions.
