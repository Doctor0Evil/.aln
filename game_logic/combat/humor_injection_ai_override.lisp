;;; ---------------------------------------------------------------------------
;;; Humor AI Decision Override System
;;; ---------------------------------------------------------------------------
;;; Extends humor injection effects to alter AI behavior/decisions.
;;; Uses morale & distraction thresholds to override NPC actions.
;;; Standards compliance: .compy.git:logic.comply = "true"
;;;
;;; Integration Path:
;;; defined.object.game.logic:path$humor-injection → humor-response-chain →
;;; humor-decision-override
;;; ---------------------------------------------------------------------------

(defparameter *morale-threshold-retreat* 20
  "NPC retreats if morale drops below this.")
(defparameter *distraction-threshold-skip* 50
  "NPC loses a combat action if distraction exceeds this.")

(defun humor-override-ai-decision (npc)
  "Examines NPC stats & humor effects. Overrides next action if thresholds crossed."
  (let ((morale (getf npc :morale))
        (distraction (getf npc :distraction))
        (focus (getf npc :focus)))
    (cond
      ;; Forced retreat: morale collapse
      ((< morale *morale-threshold-retreat*)
       (setf (getf npc :next-action) 'retreat)
       (setf (getf npc :status) 'panicked)
       :decision-override-retreat)

      ;; Skip turn: distraction too high
      ((> distraction *distraction-threshold-skip*)
       (setf (getf npc :next-action) 'idle)
       (setf (getf npc :status) 'dazed)
       :decision-override-skip)

      ;; Fumble: focus extremely low, mis-execution possible
      ((< focus 10)
       (setf (getf npc :next-action) 'attack-fumble)
       (setf (getf npc :status) 'uncoordinated)
       :decision-override-fumble)

      (t
       :no-override))))

(defun humor-decision-cycle (combat-state)
  "Checks NPCs after humor effects, enforces overrides."
  (let ((npc-list (getf combat-state :combatants)))
    (dolist (npc npc-list combat-state)
      (let ((decision (humor-override-ai-decision npc)))
        (when (keywordp decision)
          (push (list :npc (getf npc :id)
                      :decision decision
                      :tick (getf combat-state :tick))
                (getf combat-state :combat-log)))))))
