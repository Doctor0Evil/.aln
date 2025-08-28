;; filepath: /src/core/npc-engine/npc-safe-state.lisp
;; repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun npc-safe-state-handler (world state)
  "Ensures NPCs are activated safely without letting player fully exit scene."
  (let* ((npc-state (getf state :npc.state))
         (character-mood (getf state :mood))
         (exit-flag (getf state :exit-allowed))
         (log '()))

    ;; Trigger NPC active state
    (setf npc-state 'active)
    (push '(:npc . activated) log)

    ;; Enforce safe NPC state
    (setf npc-state 'safe)
    (push '(:npc . safe-mode) log)

    ;; Block exit if directive forbids leaving
    (when (not exit-flag)
      (push :exit-blocked log)
      (setf state (plist-put state :character.presence 'in-scene)))

    ;; Return updated states
    (list :npc-state npc-state
          :character-mood character-mood
          :world-state world
          :log log)))
