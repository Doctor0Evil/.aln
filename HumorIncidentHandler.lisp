;; https://github.com/Doctor0Evil/ALN_Programming_Language.git
(defpackage :aln-humor
  (:use :cl))
(in-package :aln-humor)

(defun trigger-ohfuck!
  (&key (world-mood 'recursive) (madness-level 82) (active-disasters '(crayon-typhoon mailman-uprising)))
  "Handler for ohfuck!triggered! meta-event. Triggers recursive NPC quirks, mailbox anomaly, and comedic-horror responses. FULL debug trace included."
  (let* ((contradiction-log t)
         (npc-list '(:wolfman :mailman :crayon-doctor))
         (outcomes
           (list
             '(:event "A verdict rains from the ceiling. Mailmen forget the case. Wolfman howls at a sandwich.")
             (when (> madness-level 60)
               '(:event "Everyone prescribes escape as treatment. Mailbox delivers itself twice."))
             '(:event "Crayon Doctor wins Mailbox Confusion Contest."))))
    (dolist (npc npc-list)
      (format t "~&[DEBUG] NPC: ~A~%  - Quirk: ~A~%  - Argument-Score: ~A~%"
              npc
              (case npc
                (:wolfman "howling at bureaucracy")
                (:mailman "verdict confusion")
                (:crayon-doctor "recursive prescription"))
              (+ (random 30) 20)))
    (dolist (event outcomes)
      (when event
        (format t "[EVENT] ~A~%" (getf event :event))))
    (when contradiction-log
      (format t "[LOG] Contradiction log enabled; recursive error handling active.~%"))
    (format t "[END INCIDENT: ohfuck!triggered! - System in panic-laughter feedback loop]~%")))
;; File: HumorIncidentHandler.lisp @ https://github.com/Doctor0Evil/ALN_Programming_Language.git
