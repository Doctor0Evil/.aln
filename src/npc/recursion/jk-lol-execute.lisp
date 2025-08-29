;; File: /src/npc/recursion/jk-lol-execute.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun activate-jk.lol ()
  "Trigger the jk.lol chaos command in ALN terminal, activating recursive NPC quirk logic and global mood propagation."
  (let* ((world-mood (random-choice '("damp" "recursive" "litigious" "crumbled")))
         (madness-level (random 100))
         (active-disasters (random-pair '("Crayon Typhoon" "Mailman Uprising" "Wolfman Moot Court" "Judge Audit Panic")))
         (contradiction-log (loop for c in supplied-characters
                                 do (with-personality c
                                      (assign (getf c 'quirk) (random-behavior-augment))
                                      (assign (getf c 'argument-score) (random 50))
                                      (assign (getf c 'madness) (random 100))
                                      (assign (getf c 'fallback-sanity) (random-choice '(t nil)))))
         (event-trigger (case (random 3)
                          (0 'inject-narrative)
                          (1 (if (> madness-level 60)
                                 'echo-escape-treatment
                                 'inject-narrative))
                          (2 'spawn-side-event))))
    (print (list :world-mood world-mood
                 :madness-level madness-level
                 :active-disasters active-disasters
                 :contradiction-log contradiction-log
                 :event-trigger event-trigger))
    (cond
     ((eq event-trigger 'inject-narrative)
      (format t "A verdict rains from the ceiling, mailmen forget the case, wolfman howls at a sandwich.~%"))
     ((eq event-trigger 'echo-escape-treatment)
      (format t "Everyone prescribes escape as treatment, mailbox delivers itself twice.~%"))
     ((eq event-trigger 'spawn-side-event)
      (format t "Mailbox confusion contest winner is crayon doctor.~%")))))

;; Required: supplied-characters, random-choice, random-behavior-augment functions as per /src/npc/recursion/
