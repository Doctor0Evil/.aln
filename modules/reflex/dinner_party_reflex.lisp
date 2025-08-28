github-path: modules/reflex/dinner_party_reflex.lisp
;; Reusable Event/Branch Templates: Personality-Modifier "Dinner Party Chaos" Events

;; 🍲 Reusable Branching Event Template: Surprise Dinner Party (ALN Pseudocode)
(defstruct dinner-party-event
  meal-type        ;; e.g. :lasagna, :stew, :banquet, :tea
  host             ;; NPC initiating event
  guest-list       ;; List of NPCs/players
  setting          ;; e.g. "ruined mansion", "underground bunker"
  chaos-threshold  ;; 0-1, triggers comedy/madness
  unexpected-twist-fn ;; Function to inject surprise (see below)
  rng              ;; Randomness state for chaos
)

(defun trigger-dinner-party (event)
 (announce (format nil "Surprise! The ~A Dinner Party has begun!" (event-meal-type event)))
 (loop for g in (event-guest-list event)
   do (npc-perform-dinner-entry g event))
 (if (> (random 1.0) (event-chaos-threshold event))
     (funcall (event-unexpected-twist-fn event) event))
 (narrate-main-event-sequence event)
 (narrate-guest-reactions event)
 (finish-dinner-party event))

;; 🎭 Drop-In "Personality-Modifier" Funny Output Modules

;; 1. Mailman Mirror ("Repeater" Module)
(defun mailman-mirror-modifier (npc input)
  (let ((msg (format nil "~A" input)))
    (npc-speak npc (concatenate 'string msg " ...wait, did I say that twice?"))
    (npc-speak npc (concatenate 'string (reverse msg) " (No that wasn't me.)"))))

;; 2. Judge-Juggler ("Objection!" Module)
(defun judge-juggler-modifier (npc input)
  (if (> (random 1.0) 0.5)
      (npc-speak npc "Objection! That bread is out of order in flavor court!")
      (npc-speak npc "Sustained! The sauce has the floor. Defendant: macaroni.")))

;; 3. Crayon Doctor ("Absurd Prescription" Module)
(defun crayon-doctor-modifier (npc input)
  (npc-speak npc (format nil "Diagnosis: your taste buds are under-stimulated! Take two bites and call me in the morning—preferably while coloring outside the lines.")))

;; 4. Goldfish Cop ("Forgot What Happened" Module)
(defun goldfish-cop-modifier (npc input)
  (npc-speak npc "Wait—was I supposed to arrest the bread? Or—what were we eating again?"))

;; 5. "Surreal Anaomaly" Random Funny/Weird Injection
(defun dinner-surprise-anomaly (event)
  (let ((surprises '("All forks become knives. Guests now must eat with their hands."
                     "Soup bowl starts speaking insults in Judge-Juggler's voice."
                     "Bread has a birth certificate and is arrested by Goldfish Cop."
                     "Crayon Doctor scribbles prescription on lasagna noodles; guests get buffs if they eat the note.")))
    (announce (nth (random (length surprises)) surprises))))

;; 🟧 Plug-and-Play Usage for Any Event
(defun scene-trigger-any-meal-funny (npc event player-input)
  (when (should-be-funny? npc event) ; context or random chance
    (let ((mod (pick-random-modifier npc)))
      (funcall mod npc player-input))))
