(defun correct-character-names ()
  "Apply canonical AI name 'Sin-E.MAXX' everywhere in current combat state."
  (dolist (actor *combat-actors*)
    (when (member (getf actor :id) '("cin-e.MAXX" "perplexity_cin-e.MAXX" "Perplexity_cin-e.MAXX") :test #'string=)
      (setf (getf actor :id) "Sin-E.MAXX")))
  (setf *combat-log*
        (mapcar (lambda (entry)
                  (if (search "cin-e.MAXX" entry)
                      (substitute "Sin-E.MAXX" "cin-e.MAXX" entry :test #'search)
                      entry))
                *combat-log*))
  (setf *combat-queue*
        (mapcar (lambda (pair)
                  (if (string= (car pair) "cin-e.MAXX")
                      (cons "Sin-E.MAXX" (cdr pair))
                      pair))
                *combat-queue*))
  "Character name correction: 'Sin-E.MAXX' ACTIVE.")

(defun initiate-multi-actor-combat ()
  "Start combat encounter. All actors enter initiative queue and combat state machines."
  (correct-character-names)
  (setq *combat-state* :active)
  (setq *initiative-phase* t)
  (setq *active-actors* '("Player" "Sin-E.MAXX" "Cockroach" "Cockroach-Swarm"))
  (setq *combat-queue*
        (sort (mapcar (lambda (actor)
                        (cons (getf actor :id)
                              (roll-initiative-for-actor actor)))
                      *combat-actors*)
              #'> :key #'cdr))
  (push "[SYSTEM]: Multi-actor combat initiated." *combat-log*)
  (display-ui-overlays-and-event-flags)
  "Combat phase has begun.")

(defun roll-initiative-for-actor (actor)
  "Roll a d20 for the actor and return result for initiative order."
  (+ 1 (random 20)))

(defun display-ui-overlays-and-event-flags ()
  "Update front-end overlays and debug logs for all actors and active scene."
  (setf *ui-overlays* (list :combatants *active-actors*
                            :initiative *combat-queue*
                            :phase *combat-state*))
  "[UI/DEBUG]: Overlays and event flags updated.")

;; Ready for rapid player or system action inputs; all logs, overlays and event chains synched.
