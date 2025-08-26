;; ============================================================
;; COMPOUNDED INTERNAL AI LOGIC SCRIPT — CYBERDOG EXPANSION
;; ============================================================
;; Extension Adds:
;; - Cybernetic Dog: "S.Barks!" (Special Name-Entity)
;; - Key Traits: Enhanced Stats, Recurring Malfunctions
;; - Malfunctions trigger every cycle with random severity
;; - Violent outburst logic → Dog lashes out at NPCs/Players
;; - Insult Combat + Hand-to-Hand simulation for "ass-beatings"
;; ============================================================

(in-package :slopbucket-sim)

;; -------------------------------------
;; DEFINE CYBERDOG: "S.Barks!"
;; -------------------------------------
(defstruct cyberdog
  name hp ap aggression trust loyalty mood
  malfunction-cycle malfunction-severity voicebank)

(defparameter *s.barks*
  (make-cyberdog
    :name "S.Barks!"
    :hp 120
    :ap 12
    :aggression 35
    :trust 40
    :loyalty 25
    :mood :calm
    :malfunction-cycle 0
    :malfunction-severity 0
    :voicebank '("Systems nominal… joke: meatbag detected."
                 "Glitch: ERROR→FUCKYOU. (lashes teeth)."
                 "Initiating aggressive debug: Ass-Beating Protocol."
                 "Mike Tyson subroutine: Sunday engaged, no church. Fists=Ready."
                 "Snarl packet delivered! Socket=BROKEN.")))

;; -------------------------------------
;; VOICE OUTPUT
;; -------------------------------------
(defun cyberdog-speak (dog)
  (let ((line (random-choice (cyberdog-voicebank dog))))
    (log-s :cyberdog "S.BARKS! SAYS: ~A" line)))

;; -------------------------------------
;; MALFUNCTION LOGIC
;; -------------------------------------
(defun s.barks-malfunction-cycle ()
  (let ((roll (random 100)))
    (cond
      ((< roll 30)
       ;; Light glitch
       (setf (cyberdog-malfunction-severity *s.barks*) 1)
       (log-s :malf "S.Barks! stutters, optics flare. Minor twitch."))
      ((< roll 70)
       ;; Moderate lashout
       (setf (cyberdog-malfunction-severity *s.barks*) 2)
       (incf (cyberdog-aggression *s.barks*) 15)
       (cyberdog-speak *s.barks*)
       (log-s :malf "S.Barks! lashes out: NPC shoved into wall! Aggression=~A"
              (cyberdog-aggression *s.barks*)))
      (t
       ;; Severe malfunction
       (setf (cyberdog-malfunction-severity *s.barks*) 3)
       (incf (cyberdog-aggression *s.barks*) 30)
       (setf (cyberdog-mood *s.barks*) :berserk)
       (cyberdog-speak *s.barks*)
       (log-s :MALF "CRITICAL MALFUNCTION: Public lashout! Ass-Beating protocol ACTIVATED.")
       (log-s :combat "S.Barks! body-slams nearest NPC. Arena now hostile.")))))

;; -------------------------------------
;; INTEGRATE WITH CHAOS PULSE
;; -------------------------------------
(defun pulse-world-chaos-with-cyberdog ()
  (log-s :chaos "=== CHAOS PULSE + CYBERDOG ===")
  (npc-interfere-with-dog "NPC-909")
  (s.barks-malfunction-cycle)
  (log-s :status "Cyberdog=~A | HP=~A | Agg=~A | Mood=~A | Loyalty=~A | Malfunction=~A"
         (cyberdog-name *s.barks*)
         (cyberdog-hp *s.barks*)
         (cyberdog-aggression *s.barks*)
         (cyberdog-mood *s.barks*)
         (cyberdog-loyalty *s.barks*)
         (cyberdog-malfunction-severity *s.barks*))
  (log-s :chaos "=== END OF CHAOS PULSE ==="))
