;; ============================================================
;; MASTER SYSTEM LOGIC LISP SCRIPT
;; ============================================================
;; This compounds *all action-flows* in the simulation, including:
;; - Global world mood cycles
;; - NPC state updates and madness drift
;; - Active disaster injection
;; - Chaotic narrative disruptions
;; - Logging for debug-level world evaluation
;; ============================================================
(defun system-daily-cycle ()
  (let ((*world-mood* (random-choice '(:damp :crumbled :recursive :litigious :manic :unspeakable)))
        (*madness-level* (random 100))
        (*active-disasters* (list (random-choice '(:WolfmanMootCourt :CrayonTyphoon :SausageStrike :MailboxPanic))
                                  (random-choice '(:JudgeAudit :GarbageFlood :NPCUprising :BreadWhisperer))))
        (*contradiction-log* (make-hash-table))
        (*npcs* (random-npc 5))) ;; generates 5 random NPCs
    ;; --- Start Log ---
    (log:info gutter "*** SYSTEM: DAILY SIM-CYCLE START ***")
    (log:info gutter "World: ~a / Madness: ~A / Disasters: ~S"
              *world-mood* *madness-level* *active-disasters*)
    ;; --- NPC Loop & Madness Progression ---
    (dolist (n *npcs*)
      (with-guts-logging (npc-cycle n)
        ;; madness escalation & argument scoring
        (incf (npc-argument-score n) (random 6))
        (setf (npc-madness n) (+ (npc-madness n) (random 10)))
        ;; emotional state shifts with chaotic flavor
        (setf (npc-emotion-state n)
              (random-choice '(:manic :giggling :devoured :broken :glorious))))
      (log:debug gutter "NPC ~a:: Madness=~A | Score=~A | State=~A | Quirk=~A | Fallback=~A"
                 (npc-name n) (npc-madness n) (npc-argument-score n)
                 (npc-emotion-state n) (npc-quirk n) (npc-fallback-sanity n))))
    ;; --- Inject Chaos Into Narrative ---
    (inject-chaos-narrative)
    ;; --- End-day Evaluation ---
    (log:info gutter "*** SYSTEM END-DUMP: FLOWS COMPLETED ***")
    (log:info gutter "Final World Mood: ~A | Peak Madness: ~A"
              *world-mood* (apply #'max (mapcar #'npc-madness *npcs*)))
    (log:info gutter "~~~ RETURN TOMORROW FOR NEXT ITERATION ~~~")
    (values)))
