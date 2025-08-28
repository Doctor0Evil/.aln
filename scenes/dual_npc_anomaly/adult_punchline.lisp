;; File: scenes/dual_npc_anomaly/adult_punchline.lisp
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git/blob/main/scenes/dual_npc_anomaly/adult_punchline.lisp

(defpackage :dual-npc-anomaly
  (:use :cl)
  (:export :run-hardpunchline-anomaly))

(in-package :dual-npc-anomaly)

(defun run-hardpunchline-anomaly (player npc-list)
  (let* ((chaos-mood t)
         (adult-flavor t)
         (punchline-list '("Open wide: Knock-knock. Who's there? Regret!"
                           "Bread so hot it'll singe your hair off! Taste despair!"
                           "Magic fuck! Ouch! Should’ve brought healing potions — not jokes!"
                           "Winner gets the last molar and the crust. No safe word? You lose!"
                           "Don’t worry, pain is just pleasure spelled backward… by a drunk wizard.")))
    ;; Trigger humor escalation in both NPCs
    (mapcar (lambda (npc)
              (setf (getf npc :mood) "punchline-escalation")
              (push (random-elt punchline-list) (getf npc :dialogue)))
            npc-list)
    ;; Global log for debugging
    (format t "[ANOMALY] Harder Punchline Escalation Triggered: AdultMode=~A ChaosMood=~A~%" adult-flavor chaos-mood)
    (loop for npc in npc-list
          do (format t "[NPC] ~A Mood=~A Dialogue: ~A~%"
                     (getf npc :name)
                     (getf npc :mood)
                     (car (last (getf npc :dialogue)))))
    (echo "Result.is: extended.humor.adult.magic-fuck!ouch!: no.bullshit!")
    'punchline-anomaly-complete))
