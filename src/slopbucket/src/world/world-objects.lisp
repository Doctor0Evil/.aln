;; ================================================================
;; EXTENDED LOGIC SCRIPT – APPLY_SUGGESTIONS + RESOLVE WORLD.OBJECTS
;; ================================================================
;; Based on user directive: system evolves from just a "world-cycle"
;; into one that can also embed *combat resolution states* into the
;; generated world objects. Two distinct paths:
;;   1. World-Cycle Simulation (passive, narrative-driven).
;;   2. Combat Encounter Resolution (active, raider vs NPC skirmish).
;;
;; We define :world.objects to store dynamic entities (raiders, NPCs, dog, etc.)
;; and register state-updates each cycle using `resolve-world-objects`.
;; ================================================================

(defpackage :world.objects
  (:use :cl)
  (:export :*entities* :register-entity :resolve-world-objects))

(in-package :world.objects)

(defparameter *entities* (make-hash-table :test 'equal))

(defun register-entity (eid data)
  "Registers or updates an entity object into the world-state hash."
  (setf (gethash eid *entities*) data))

(defun resolve-world-objects ()
  "Resolves current state of all active entities each tick."
  (maphash
   (lambda (eid data)
     (format t "[RESOLVE] ~A => ~A~%" eid data))
   *entities*)
  t)

;; ================================================================
;; EXTENSION TO COMBAT SYSTEM
;; ================================================================
(defpackage :wastepunk-combat
  (:use :cl :world.objects)
  (:export :init-combat-encounter :simulate-combat-round))

(in-package :wastepunk-combat)

(defun init-combat-encounter (raider npc)
  "Registers combatants raider + npc as world entities and flags encounter."
  (register-entity "combat-raider"
                   `(:type raider :hp ,(+ 30 (random 20)) :atk ,(+ 3 (random 5)) :def ,(+ 1 (random 3)) :ref ,raider))
  (register-entity "combat-npc"
                   `(:type npc :hp ,(+ 20 (random 15)) :atk ,(+ 2 (random 6)) :def ,(+ 0 (random 4)) :ref ,npc))
  (format t "[INIT-COMBAT] Raider vs NPC encounter registered.~%"))

(defun simulate-combat-round ()
  "Executes one combat round; resolves damage exchange."
  (let* ((raider (gethash "combat-raider" *entities*))
         (npc (gethash "combat-npc" *entities*)))
    (when (and raider npc)
      (let* ((atk-r (getf raider :atk))
             (atk-n (getf npc :atk))
             (def-r (getf raider :def))
             (def-n (getf npc :def))
             (hp-r (getf raider :hp))
             (hp-n (getf npc :hp))
             (dmg-to-npc (max 0 (- atk-r (random (1+ def-n)))))
             (dmg-to-raider (max 0 (- atk-n (random (1+ def-r))))))

        (decf (getf raider :hp) dmg-to-raider)
        (decf (getf npc :hp) dmg-to-npc)

        (format t "[COMBAT] Raider deals ~A dmg | NPC deals ~A dmg~%" dmg-to-npc dmg-to-raider)
        (format t "[STATUS] Raider HP:~A | NPC HP:~A~%" (getf raider :hp) (getf npc :hp))

        ;; Update back into entities
        (setf (gethash "combat-raider" *entities*) raider
              (gethash "combat-npc" *entities*) npc)

        ;; Check resolution
        (cond
          ((<= (getf raider :hp) 0) (format t "[RESOLUTION] Raider is DEAD.~%"))
          ((<= (getf npc :hp) 0) (format t "[RESOLUTION] NPC is DEAD.~%"))
          (t (format t "[ROUND] Fight continues...~%")))))))
