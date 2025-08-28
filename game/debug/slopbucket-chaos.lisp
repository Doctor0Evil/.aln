;; ============================================================
;; COMPOUNDED INTERNAL AI LOGIC SCRIPT
;; ============================================================
;; Extending the simulation layer into "SLOPBUCKET" debug-mode:
;; - Dog state evolves dynamically across absurd "chaos" cycles
;; - Player influence modifies loyalty vs aggression in twisted ways
;; - NPCs spawn with surreal quirks / actions
;; - Debug console produces "gorey absurd reports" of the world state
;; ============================================================

(defpackage :slopbucket-sim
  (:use :cl)
  (:export :run-slopbucket-cycle :set-dog-action))
(in-package :slopbucket-sim)

;; -------------------------
;; CORE STATE VARIABLES
;; -------------------------
(defparameter *dog-state* :chewing-toy)
(defparameter *dog-trust* 35)
(defparameter *dog-aggression* 20)
(defparameter *dog-mood* :calm)
(defparameter *dog-loyalty* 20)
(defparameter *player-influence* 0)
(defparameter *world-mood* (random-choice '(:chaos :feral :neutral :litigious :dream-state)))
(defparameter *npcs* nil)
(defparameter *log* nil)

(defun random-choice (lst)
  (nth (random (length lst)) lst))

;; -------------------------
;; LOG UTILITIES
;; -------------------------
(defun log-s (tag msg &rest args)
  (let ((entry (format nil "[~A] ~A"
                       (string-upcase (symbol-name tag))
                       (apply 'format nil msg args))))
    (push entry *log*)
    (format t "~A~%" entry)))

;; -------------------------
;; EVENT SIMULATION
;; -------------------------
(defun simulate-dog-event ()
  (let ((roll (random 100)))
    (cond
      ((< roll 25)
       (incf *dog-trust* 5)
       (log-s :dog "Dog gnaws sandal crust. Teeth scrape barnacles. Trust +5 (now ~A)." *dog-trust*))
      ((< roll 60)
       (incf *dog-aggression* 7)
       (setf *dog-mood* :feral)
       (log-s :dog "Dog growls at smoke ghost. Aggression +7, mood now ~A." *dog-mood*))
      ((< roll 90)
       (log-s :dog "Dog loops ball-lick routine; existential comfort routine engaged."))
      (t
       (setf *dog-mood* :excited)
       (log-s :dog "Dog tilts head violently. Data glitch. Mood=excited.")))))


(defun inject-chaos-npc ()
  (let* ((name (format nil "NPC-~3,'0d" (random 999)))
         (quirk (random-choice '(:flesh-tuba :bootlicker :mailbox-priest :pipe-chatter)))
         (madness (random 100))
         (line (random-choice '("Bread with veins is breakfast of champions."
                                "My quirk? Screaming at rotting knives!"
                                "Bootlicking won't save your harddrive."))))
    (log-s :npc "[~A] quirk=~A madness=~A says: ~A" name quirk madness line)))

(defun set-dog-action (action)
  (case action
    (:follow (incf *dog-loyalty* 20)
             (setf *dog-state* :following)
             (log-s :dog "Dog follows. Loyalty=~A" *dog-loyalty*))
    (:bond   (incf *dog-aggression* 10)
             (setf *dog-mood* :feral)
             (log-s :dog "Bond ritual complete. Aggression=~A, mood=~A" *dog-aggression* *dog-mood*))
    (:neutral (setf *dog-state* :neutral)
              (log-s :dog "Dog remains neutral but chill. Weak tail wag."))
    (otherwise (log-s :dog "Dog confused. Action=~A ignored." action))))

(defun run-slopbucket-cycle ()
  (log-s :init "=== SLOPBUCKET SESSION ===")
  (log-s :status "DogState=~A Trust=~A Aggression=~A Mood=~A Loyalty=~A"
         *dog-state* *dog-trust* *dog-aggression* *dog-mood* *dog-loyalty*)
  (simulate-dog-event)
  (inject-chaos-npc)
  (log-s :world "WorldMood=~A | PlayerInfluence=~A"
         *world-mood* *player-influence*)
  (log-s :finish "=== END OF CYCLE ===")
  (reverse *log*))
