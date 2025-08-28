;; ============================================================
;; COMPOUNDED INTERNAL AI LOGIC SCRIPT
;; ============================================================
;; This script defines the logic, behaviors, and systemic logging
;; for player interaction with a dog NPC ("dog" entity) using the
;; ALN_Command_Terminal combat/dialogue simulation engine.
;; ============================================================

(defentity Dog-NPC
  :id "npc_dog_01"
  :type :creature
  :subtype :dog
  :disposition :neutral
  :mood :agitated
  :hp 35
  :ap 8
  :loyalty 20        ;; scale 0-100
  :trust 15          ;; affects chance of calming
  :faction :animal)

(defun calm-dog (player dog)
  (log "Attempting to calm dog...")
  (let* ((base-chance 40)
         (charisma (getstat player :charisma))
         (speech (getskill player :speech))
         (modifier (+ (* 2 charisma) (/ speech 2)))
         (success-threshold (+ base-chance modifier))
         (roll (rand 1 100)))
    (log (format nil "Calm attempt roll: ~a vs. threshold ~a" roll success-threshold))
    (if (<= roll success-threshold)
        (progn
          (setf (dog :mood) :calm)
          (incf (dog :trust) 20)
          (log "Dog calmed successfully.")
          :success)
        (progn
          (decf (dog :trust) 5)
          (setf (dog :mood) :growling)
          (log "Dog does not trust player, growling.")
          :failure)))))

(defun process-player-dialogue (player target line)
  (log (format nil "Player speaks: '~a'" line))
  (case target
    (:dog
     (if (string-contains line "good dog")
         (calm-dog player dog-npc)
         (log "Dog tilts head, confused...")))
    (otherwise
      (log "Target not recognized for dialogue processing."))))

;; ============================================================
;; EXECUTION ENTRY
;; ============================================================

(defun main-sequence ()
  (log "=== PLAYER ACTION: Dialogue with Dog ===")
  (process-player-dialogue *player* :dog "there, there. be a good dog, now!"))
