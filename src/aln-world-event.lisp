;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPOUND LISP SCRIPT - In-Game World Purge Consequence Simulation (Cheating)
;;
;; Models world mood shifts, NPC reactions, faction standing collapse, and
;; contamination spread when a user is purged for cheating. Implements all
;; flags, mood propagation, chaos events, combat logic interlock, and memory
;; contamination mechanics in ALN syntax. Attaches all actions and logs into the
;; AI simulation debug pipeline for full event cascade.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :aln-world-event
  (:use :cl :ai-ml))
(in-package :aln-world-event)

(defvar *global-mood-tensor*
  '((:fear . 0.8) (:despair . 0.6) (:chaos . 0.4) (:hope . 0.1)))

(defun propagate-world-mood (event user-id)
  "Push mood/contamination flags to world, NPC networks, and log."
  ;; Chaos and despair escalate upon purge
  (incf (cdr (assoc :fear *global-mood-tensor*)) 0.1)
  (incf (cdr (assoc :despair *global-mood-tensor*)) 0.15)
  (incf (cdr (assoc :chaos *global-mood-tensor*)) 0.12)
  ;; World contamination routine triggered
  (contaminate-world-logic user-id event)
  ;; NPC reaction handler
  (npc-propagate-hostility user-id event)
  ;; Faction standing collapse
  (collapse-player-factions user-id event)
  ;; Combat simulation update
  (combat-layer-adjust user-id event)
  ;; Final debug
  (format t "[WORLD-EVENT] Flags propagated: ~A~%" *global-mood-tensor*))

(defun contaminate-world-logic (user-id event)
  "Spread memory, spawn, and world logic contamination for cheat event."
  (mark-memory-contamination :user user-id)
  (setf (get 'world :contaminated) t)
  (format t "[WORLD] World logic contaminated by purge event for ~A.~%" user-id))

(defun npc-propagate-hostility (user-id event)
  "NPC networks update moods, trust and AI hostility in response."
  (loop for npc in *all-npc-list*
        do (setf (getf npc :hostility) (min 1.0 (+ (getf npc :hostility 0.5) 0.35)))
        (format t "[NPC] Hostility escalated to ~A for all NPCs.~%" (getf npc :hostility))))

(defun collapse-player-factions (user-id event)
  "Collapse faction standings and inject betrayal flags for user."
  (loop for faction in *player-factions*
        do (setf (getf faction :player-standing) 0)
           (push :betrayal (getf faction :event-flags))
           (format t "[FACTION] Standing collapsed for ~A in faction ~A.~%" user-id (getf faction :name))))

(defun combat-layer-adjust (user-id event)
  "Update combat simulation flags to reflect purge and injected chaos."
  (setf (get 'combat :world-chaos) t)
  (setf (get 'combat :ai-hostility) 1.0)
  (setf (get 'combat :random-event-rate) 0.6)
  (format t "[COMBAT] AI Hostility and chaos increased post-purge.~%"))

(defun simulate-purge-consequence (user-id reason)
  "Entire event pipeline when a user is purged for cheating."
  (propagate-world-mood 'purge user-id)
  ;; World contamination, NPC hostility, faction collapse, and combat logic update
  (format t "[SYSTEM] Session purge consequences fully propagated for ~A due to ~A.~%" user-id reason))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END COMPOUND LISP SCRIPT - Attach: src/aln-world-event.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
