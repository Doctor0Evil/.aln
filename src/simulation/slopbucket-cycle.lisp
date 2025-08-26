;;;; SLOPBUCKET STUDIOS: MASTERPIECE DEBUG GUTS LOGGER ;;;;
;;;; Requires: log4cl for advanced logging (quicklisp: (ql:quickload "log4cl")) ;;;;
(ql:quickload "log4cl")
(defpackage :slopbucket
  (:use :cl :log)
  (:export :run-daily-carnage))
(in-package :slopbucket)
;; Setup Main Logger
(log:define-logger gutter ())
;;-- World Mood/Disaster Setup
(defvar *world-mood* (random-choice '(:damp :crumbled :recursive :litigious :manic :unspeakable)))
(defvar *madness-level* (random 100))
(defvar *active-disasters* (list (random-choice '(:WolfmanMootCourt :CrayonTyphoon :SausageStrike :MailboxPanic))
                                (random-choice '(:JudgeAudit :GarbageFlood :NPCUprising :BreadWhisperer))))
(defvar *contradiction-log* (make-hash-table))
;;-- Actor/NPC Structure and Generation
(defstruct npc name madness argument-score quirk fallback-sanity emotion-state)
(defvar *npcs* nil)
(defun random-npc (n)
  (loop repeat n collect
    (make-npc
      :name (format nil "NPC~a" (random 9999))
      :madness (random 100)
      :argument-score (random 50)
      :quirk (random-choice '(:pyromaniac :soggy-bread :mail-thief :ketchup-guru))
      :fallback-sanity (random-choice '(t nil))
      :emotion-state (random-choice '(:manic :depressed :flirtatious :hostile :drunk)))))
(setf *npcs* (random-npc 5))
;;-- Event Injection Logic
(defun inject-chaos-narrative ()
  (case (random 3)
    (0 (log:warn gutter "A verdict rains from the ceiling. Mailmen forget the case. Wolfman howls at a sandwich."))
    (1 (when (> *madness-level* 60)
         (log:info gutter "Everyone prescribes escape as treatment. Mailbox delivers itself twice.")))
    (2 (log:debug gutter "Mailbox confusion contest winner is crayon doctor."))))
;;-- Function-Level Debug Logger
(defmacro with-guts-logging ((funname &rest args) &body body)
  `(progn
     (log:info gutter "CALL ~a ARGS ~S" ',funname (list ,@args))
     (let ((result (progn ,@body)))
       (log:info gutter "RET ~a RESULT ~S" ',funname result)
       result)))
;;-- Main Simulation Loop
(defun run-daily-carnage ()
  (log:info gutter "*** SLOPBUCKET STUDIOS: DAILY DUMP START ***")
  (log:info gutter "World Mood: ~a | Madness: ~a | Disasters: ~a"
    *world-mood* *madness-level* *active-disasters*)
  ;; Actor update loop
  (dolist (n *npcs*)
    (with-guts-logging (actor-update n)
      (incf (npc-argument-score n) (random 5))
      (setf (npc-madness n) (+ (npc-madness n) (random 10)))
      (setf (npc-emotion-state n) (random-choice '(:manic :giggling :devoured :broken :glorious))))
    (log:debug gutter "~a | Madness: ~a | Score: ~a | State: ~a | Quirk: ~a"
      (npc-name n) (npc-madness n) (npc-argument-score n) (npc-emotion-state n) (npc-quirk n)))
  (inject-chaos-narrative)
  (log:info gutter "*** SLOPBUCKET DEBUG LOG: YER GUTS ARE EVERYWHERE ***")
  (log:info gutter "End of Day: World Mood ~a | Highest Madness ~a"
    *world-mood* (apply #'max (mapcar #'npc-madness *npcs*)))
  (log:info gutter "~~~ ENJOY THE ABSURDITY AND COME BACK FOR MORE SLOP TOMORROW! ~~~")
  (values))
;;-- Helper Functions
(defun random-choice (lst)
  (nth (random (length lst)) lst))
;;-- Usage
;; (slopbucket:run-daily-carnage)
;;;; MASTER SYSTEM LOGIC LISP SCRIPT ;;;;
;;;; This compounds all action-flows, logging, npc updates, chaos injections, and mood/disaster evaluations. ;;;;
(defun system-daily-cycle ()
  (let ((*world-mood* (random-choice '(:damp :crumbled :recursive :litigious :manic :unspeakable)))
        (*madness-level* (random 100))
        (*active-disasters* (list (random-choice '(:WolfmanMootCourt :CrayonTyphoon :SausageStrike :MailboxPanic))
                                  (random-choice '(:JudgeAudit :GarbageFlood :NPCUprising :BreadWhisperer))))
        (*contradiction-log* (make-hash-table))
        (*npcs* (random-npc 5)))
    ;; Start-log
    (log:info gutter "*** SYSTEM: DAILY SIM-CYCLE START ***")
    (log:info gutter "World: ~a / Madness: ~A / Disasters: ~S"
              *world-mood* *madness-level* *active-disasters*)
    ;; NPC Loop & Madness Progression
    (dolist (n *npcs*)
      (with-guts-logging (npc-cycle n)
        (incf (npc-argument-score n) (random 6))
        (setf (npc-madness n) (+ (npc-madness n) (random 10)))
        (setf (npc-emotion-state n)
              (random-choice '(:manic :giggling :devoured :broken :glorious))))
      (log:debug gutter "NPC~a:: Madness=~A | Score=~A | State=~A | Quirk=~A | Fallback=~A"
                 (npc-name n) (npc-madness n) (npc-argument-score n)
                 (npc-emotion-state n) (npc-quirk n) (npc-fallback-sanity n)))
    ;; Inject world-chaos to distort narrative
    (inject-chaos-narrative)
    ;; End-day evaluation
    (log:info gutter "*** SYSTEM END-DUMP: FLOWS COMPLETED ***")
    (log:info gutter "Final World Mood: ~A | Peak Madness: ~A"
              *world-mood* (apply #'max (mapcar #'npc-madness *npcs*)))
    (log:info gutter "~~~ RETURN TOMORROW FOR NEXT ITERATION ~~~")
    (values)))
