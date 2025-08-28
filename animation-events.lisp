; github destination: /ALN_Programming_Language/sim/animation-events.lisp
(defpackage :combat-sim
  (:use :cl))
(in-package :combat-sim)

; Core event function handling stumble, fall, draw weapon, and emotional state
(defun handle-stumble-backwards-event (actor)
  (let* ((fall-severity (random 1.0))
         (dazed-level (if (> fall-severity 0.65) 'high 'low))
         (animation-seq (list :stumble :slip :fall-on-ass))
         (weapon-drawn (eq (getf actor :inventory) :pocketknife))
         (result-state
           (list
             :actor actor
             :animation animation-seq
             :status (list :dazed dazed-level :confused t)
             :weapon-state (if weapon-drawn :drawn :none)))
         (event-log
            (list :event :fall :actor actor :severity fall-severity
                  :animation animation-seq
                  :status :dazed dazed-level
                  :weapon :pocketknife :weapon-state (if weapon-drawn :drawn :none)))
         )
    (log-event-to-system event-log)
    (update-actor-state actor result-state)
    (if weapon-drawn
        (trigger-combat-alert actor "Pocketknife drawn while dazed/confused"))
    (return-from handle-stumble-backwards-event result-state)
  ))

; Log utility
(defun log-event-to-system (event)
  (push event *global-event-log*))

; State update function for animation and world flags
(defun update-actor-state (actor new-state)
  (setf (getf actor :animation-state) (getf new-state :animation))
  (setf (getf actor :status) (getf new-state :status))
  (setf (getf actor :weapon-state) (getf new-state :weapon-state))
  (when (eq (getf new-state :weapon-state) :drawn)
    (setf (getf actor :alert-level) (max (getf actor :alert-level) 0.7))))

; Alert trigger for nearby NPCs
(defun trigger-combat-alert (actor reason)
  (let ((npc-list (nearby-npcs actor)))
    (dolist (npc npc-list)
      (setf (getf npc :suspicion) (min 1.0 (+ (getf npc :suspicion) 0.15)))
      (log-event-to-system (list :npc-reaction :alert :reason reason
                                 :npc npc :timestamp (get-universal-time))))))

; Example call
; (handle-stumble-backwards-event player-1)
