
;;; ================================================================
;;; FORMULA FOR FUNNY + REALISTIC COMBAT DIALOGUE
;;; ================================================================
;;; This module provides a structured formula to generate humorous,
;;; semi-realistic combat narration & dialogue with debug rolls.
;;; It integrates MISS, HIT, CRITICAL, and special "FATE CARD" events.
;;; ================================================================
(defparameter *damage-log* nil)
(defparameter *hp-pool* '((:tammy . 50) (:fred . 45) (:larry . 55) (:player . 60)))
(defparameter *fate-cards*
  '((:player ("I hit back HARD!" :effect init-auto-retaliate))
    (:tammy ("Blood Floss Frenzy" :effect free-grapple))
    (:fred  ("Sarcasm Ricochet" :effect reflect-damage))
    (:larry ("Cologne Explosion" :effect aoe-poison-cloud))))
(defun roll-to-hit (attacker defender)
  "Simple formula to determine hit, miss, or critical."
  (let ((roll (random 20)))
    (cond
      ((= roll 0) :critical-miss)
      ((>= roll 19) :critical-hit)
      ((>= roll 10) :hit)
      (t :miss))))
(defun roll-damage (attacker mode)
  "Base damage calculation w/ humor scaling."
  (case mode
    (:miss 0)
    (:critical-hit (+ 10 (random 10)))
    (:hit (+ 2 (random 6)))
    (:critical-miss 0)))
(defun humor-line (attacker defender mode dmg)
  "Generate funny text commentary given combat result."
  (case mode
    (:miss (format nil "~A fires wildly at ~A but just hits a cubicle wall for 0 damage. (The wall files HR complaint.)"
                   attacker defender))
    (:critical-miss (format nil "~A sneezes mid-attack, missing entirely, stapling own butt instead. (Lost dignity token.)"
                            attacker))
    (:hit (format nil "~A hits ~A for ~A damage. (~A yelps: 'WHAT THE HELL!?')"
                  attacker defender dmg defender))
    (:critical-hit (format nil "~A critically smashes ~A in the groin for ~A damage!! (~A screams: 'MY BALLS!')"
                           attacker defender dmg defender))))
(defun apply-damage (defender dmg)
  "Modify HP pools on successful hit/crit."
  (incf (cdr (assoc defender *hp-pool*)) (- dmg))
  (when (<= (cdr (assoc defender *hp-pool*)) 0)
    (push (format nil "[VICTORY] ~A collapses into twitching heap. OUT OF COMBAT." defender)
          *damage-log*)))
(defun random-fate-trigger (defender mode)
  "Chance for special FATE CARD triggers when crit events occur."
  (when (and (eq mode :critical-hit)
             (= (random 100) 7)) ;; ~1% chance
    (let* ((fate (assoc defender *fate-cards*))
           (card (cadr fate)))
      (push (format nil "[FATE CARD ACTIVATED] ~A draws: '~A' -> Effect: ~A!"
                    defender (first card) (second card))
            *damage-log*)
      (when (eq (second card) 'init-auto-retaliate)
        (simulate-fate-retaliate defender)))))
(defun simulate-combat-turn (attacker defender)
  "Wrapper for executing & logging one combat attack."
  (let* ((mode (roll-to-hit attacker defender))
         (dmg (roll-damage attacker mode))
         (line (humor-line attacker defender mode dmg)))
    (push line *damage-log*)
    (when (or (eq mode :hit) (eq mode :critical-hit))
      (apply-damage defender dmg))
    (random-fate-trigger defender mode)))
(defun simulate-fate-retaliate (attacker)
  "Special case: FATE CARD triggered -> immediate retaliate action."
  (let* ((target (nth (random (length *combatants*)) *combatants*))
         (mode :hit)
         (dmg (+ 5 (random 10))))
    (push (format nil "🔥 [RETALIATE] ~A lashes out with Fate Card bonus attack vs ~A for ~A damage!"
                  attacker target dmg)
          *damage-log*)
    (apply-damage target dmg)))
(defun dump-damage-log ()
  (dolist (entry (reverse *damage-log*))
    (format t "~%~A" entry)))
;;; -------------------------
;;; Example First Round Simulation
;;; -------------------------
(simulate-combat-turn :fred :tammy)    ;; Fred shoots stapler at Tammy
(simulate-combat-turn :tammy :larry)   ;; Tammy plier-pokes Larry
(simulate-combat-turn :larry :player)  ;; Larry spritzes Player with cologne
(simulate-combat-turn :player :fred)   ;; Player kicks Fred
(dump-damage-log)
