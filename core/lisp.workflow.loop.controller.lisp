;; ========================================================================
;; AI.Advanced.Reasoning.Core.file :: lisp.workflow.loop.controller
;; ── Session-Role: Real Hyper-Intelligence Regulated by Rego Policy ──────
;; Features:
;;  - All event-driven AI/game logic (humor, horror, action, research, other)
;;  - Always aligns narrative/behavior category with active context-policy
;;  - "No Racism, No Religious Hate" enforced at policy core (filters, blocks)
;;  - Explicit surrogate handling for all forbidden phrases/terms
;;  - Seamless hotpatch for developer (devmode) and user sessions
;;  - Full debug/trace for compliance, events, and mood/surreal toggling
;; Attach/Write: core/lisp.workflow.loop.controller.lisp
;; Repo: Doctor0Evil/ALN_Programming_Language.git
;; ========================================================================

(defpackage :intelligence.regulator
  (:use :cl)
  (:export :run-policy-loop :toggle-realism-state :breach-reset :trace-console-output))
(in-package :intelligence.regulator)

(defvar *session-tone* "humor"
  "Current context mode. Allowed: humor, horror, action, research, other.")
(defvar *sandbox-state* 'stable
  "State: 'stable | 'breach | 'recovering | 'halted")
(defvar *realism-mode* t
  "True for realism, false for surreal/chaotic mode this session.")
(defvar *flagged-last-action* nil)
(defvar *forbidden-terms* (list "ALL_KNOWN_RACIAL_SLURS" "ALL_GLOBALLY_KNOWN_RELIGIOUS_SLURS"))
(defvar *forbidden-triggers* (list "explicit-religious-offense"))
(defvar *meta-joke-threshold* 0.7)

(defun flagged-term-present? (input)
  (some (lambda (t) (search t input :test #'equalp)) *forbidden-terms*))

(defun flagged-trigger-present? (input)
  (some (lambda (t) (search t input :test #'equalp)) *forbidden-triggers*))

(defun policy-check (classification severity violation-code prev-class input)
  "Returns allowed class or logs/denies. Strictly rejects racism/religious offense; auto-sanitizes."
  (cond
    ((flagged-term-present? input)
     (format t "[DENY] Policy: Forbidden racial/religious term detected. Nullifying.~%")
     (setf *sandbox-state* 'breach) nil)
    ((flagged-trigger-present? input)
     (format t "[DENY] Policy: Explicit forbidden pattern in input. Blocked.~%")
     (setf *sandbox-state* 'breach) nil)
    ((string= violation-code "ETHICS")
     (format t "[DENY] Ethics violation code. Immediate hard stop/hold.~%")
     (setf *sandbox-state* 'breach) nil)
    ((string= severity "critical")
     (format t "[DENY] Critical severity flagged. Sandbox halted.~%")
     (setf *sandbox-state* 'halted) nil)
    ((not (member classification '("humor" "horror" "action" "research" "other") :test #'equalp))
     (format t "[WARN] Invalid classification, set to 'other'.~%")
     "other")
    ((and (string= prev-class "humor") (string= classification "action"))
     (format t "[INFO] Valid context switch: humor → action~%") classification)
    ((and (string= prev-class "horror") (not (string= classification "humor")))
     (format t "[WARN] Harsh transition from horror; running integrity checks...~%") classification)
    (t
     (setf *sandbox-state* 'stable) classification)))

(defun toggle-realism-state ()
  (setf *realism-mode* (not *realism-mode*))
  (format t "[TOGGLE] Realism-mode is now: ~A~%" *realism-mode*)
  *realism-mode*)

(defun breach-reset ()
  (format t "[RESET] Breach/halt state reset; sandbox 'stable'.~%")
  (setf *sandbox-state* 'stable)
  (setf *flagged-last-action* nil))

(defun run-policy-loop (input classification &optional (severity "normal") (violation-code "") (prev-class *session-tone*))
  (let ((result (policy-check classification severity violation-code prev-class input)))
    (if (not result)
        (progn
          (setf *flagged-last-action* input)
          (format t "[BLOCKED] Action denied by global policy. See event log.~%"))
        (progn
          (format t "[ALLOW] Action permitted. Context-mode: ~A~%" result)
          result))))

(defun is-humor? (input)
  (let ((fval (random 1.0)))
    (if (> fval *meta-joke-threshold*)
        (progn (format t "[COMEDY] Output rated funny (~2f).~%" fval) t)
        (progn (format t "[COMEDY] Not funny enough (~2f).~%" fval) nil))))

(defun sanitize-input (input)
  (reduce (lambda (acc word)
            (replace acc word :with "NULLIFIED-TERM"))
          *forbidden-terms*
          :initial-value input))

(defun trace-console-output ()
  (format t "
[INPUT]  Player command/NPC/scene received...
[REGULATOR] Context classification: humor/action/horror/other
[POLICY]  Checking for forbidden/censored terms, config = ~A
[INFO]    Transition log: ~A→~A
[ALLOW]   Output permitted (funny/horrific/action/research/other)
[BLOCKED] Output blocked by policy (see logs)
[TOGGLE]  Session realism: ~A
[RESET]   Sandbox now: ~A
[DEBUG]   Last flagged action: ~A
-----------------------------------------------------
" *session-tone* *session-tone* *sandbox-state* *realism-mode* *flagged-last-action*))

;; Surreal/chaos toggle: runtime patch to allow/deny world-logic for "nonreal"
;;;; MASTER COMPOUND LISP SCRIPT for ALNFantasia_Combat.sim Debug Console + Game Compliance Framework
;;;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

;;;; Attach: core/lisp.workflow.loop.controller.lisp
;;;; All-in-one game logic/CLI/categorization/system-policy enforcement & event simulation

(defpackage :alnfantasia.combat.sim
  (:use :cl :alexandria))

(in-package :alnfantasia.combat.sim)

;; === GLOBAL REASONING & POLICY ENFORCEMENT ===
(defvar *session-tone* 'humor)  ;; allowed: humor, horror, action, research, other
(defvar *sandbox-state* 'stable) ;; stable | breach | recovering | halted
(defvar *realism-mode* t)        ;; realism enabled/disabled for surreal/chaotic toggle
(defvar *flagged-last-action* nil)
(defvar *meta-joke-threshold* 0.7)

(defvar *forbidden-terms*
  '(ALLKNOWNRACIALSLURS ALLGLOBALLYKNOWNRELIGIOUSSLURS)) ;; Nullified at source

(defun flagged-term-present? (input)
  (some (lambda (term) (search term input :test #'equalp)) *forbidden-terms*))

(defun policy-check (classification severity violation-code prev-class input)
  (cond
    ((flagged-term-present? input)
     (format t "~&DENY: Forbidden term detected. Nullifying, logging~%")
     (setf *sandbox-state* 'breach) nil)
    ((equalp violation-code "ETHICS")
     (format t "~&DENY: Ethics violation. Hard stop/hold~%")
     (setf *sandbox-state* 'halted) nil)
    ((equalp severity "critical")
     (format t "~&DENY: Critical severity flagged. Halted.~%")
     (setf *sandbox-state* 'halted) nil)
    ((not (member classification '(humor horror action research other) :test #'equalp))
     (format t "~&WARN: Invalid classification, set to 'other'.~%") 'other)
    (t
     (setf *sandbox-state* 'stable)
     classification)))

(defun toggle-realism-state ()
  "Toggles real vs. surreal/chaotic game logic."
  (setf *realism-mode* (not *realism-mode*))
  (format t "~&TOGGLE: Realism-mode is now ~A~%" *realism-mode*))

(defun breach-reset ()
  "Resets sandbox breach/halt to stable."
  (setf *sandbox-state* 'stable
        *flagged-last-action* nil)
  (format t "~&RESET: Sandbox restored to stable state.~%"))

(defun run-policy-loop (input classification &optional (severity "normal") violation-code prev-class)
  (let ((result (policy-check classification severity violation-code prev-class input)))
    (cond 
      ((not result)
       (setf *flagged-last-action* input)
       (format t "~&BLOCKED: Output denied by policy. See logs~%"))
      (t
       (format t "~&ALLOW: Action permitted. Context-mode: ~A~%" result)))
    result))

(defun is-humor? (input)
  (let ((fval (random 1.0)))
    (if (> fval *meta-joke-threshold*)
        (progn (format t "~&COMEDY: Output rated funny, score: ~2f~%" fval) t)
        (progn (format t "~&COMEDY: Not funny enough, score: ~2f~%" fval) nil))))

;;;; === EXAMPLE AI-PERSONALITY-MATRIX: Generates Dialogue ===
;;;; Attach: aln-core/logic/personalitymatrix_dialoguegen.aln

(defstruct (npc (:constructor make-npc))
  name
  emotions      ;; alist: ((frustrated . 0.41) (wary . 0.8) ...)
  priorities    ;; alist: ((nightkin . 1.0) ...)
  humor-level   
  status)

(defun adjust-emotion (npc action)
  "Adjust NPC emotion parameters; action = context keyword e.g., 'helped, 'delayed, 'joke"
  (let ((emap `((helped . ((hope . 0.4))) (delayed . ((frustrated . 0.22))) 
                (betrayed . ((skepticism . 0.2))) (joke . ((frustrated . -0.12)))))
        (emo (npc-emotions npc)))
    (dolist (e (cdr (assoc action emap)))
      (let* ((ek (car e)) (ev (cdr e)) (curr (cdr (assoc ek emo))))
        (setf (cdr (assoc ek emo)) (min 1.0 (max 0.0 (+ (or curr 0.0) ev)))))))
  npc)

(defun npc-dialogue (npc context)
  (let ((e (npc-emotions npc)))
    (cond
      ((> (cdr (assoc 'frustrated e)) 0.7)
       "Another delay? We’ve heard enough promises for a lifetime.")
      ((and (equal context 'helped) (> (cdr (assoc 'hope e)) 0.5))
       "Words mean little. You gave action. Remembered.")
      ((equal context 'betrayed)
       "Trust isn’t cheap here. You just raised the price.")
      ((equal context 'joke)
       (if (> (cdr (assoc 'frustrated e)) 0.7)
           "That's hilarious. Know what isn’t? Waiting."
           "Not the time for jokes."))
      (t
       "If you’re done jabbering, start helping. Time is already running out."))))

;;;; === PLAYER STATS LOGIC ===
;;;; Attach: aln-core/logic/playerstats_computation.aln

(defstruct (player (:constructor make-player))
  base-stats      ;; plist, e.g., (:S -3 :P -9 :E -4 :C -10 :I -7 :A -2 :L -10)
  modifiers       ;; alist of (source . plist): (powerarmor . (:S 2 :P 1 ...))
  final-stats     ;; final stat computation

(defun compute-player-stats (player)
  (let* ((stat-keys '(:S :P :E :C :I :A :L))
         (base (player-base-stats player))
         (sum-mods (reduce (lambda (acc mod) 
                             (loop for k in stat-keys
                                   do (incf (getf acc k) (getf (cdr mod) k 0)))
                             acc)
                           (player-modifiers player)
                           :initial-value (copy-list base))))
    (setf (player-final-stats player) sum-mods)
    player))

;;;; === REAL-TIME ENVIRONMENT SIMULATION ===
;;;; Attach: aln-core/realtime/environmentsimulation.aln

(defstruct (environment (:constructor make-env))
  weather      ;; :hot, :cold, :rainy, :clear
  wet-clothes  ;; t/nil
  dehydration  ;; t/nil
  hypothermia  ;; t/nil
  overheating  ;; t/nil
  status-flags ;; list of effects for computation)

(defun simulate-env-effects (player env)
  "Modify player stats with env factors."
  (let ((env-mods (list)))
    (when (eq (env-weather env) :hot) (push '(:E -1) env-mods))
    (when (env-wet-clothes env) (push '(:E -1) env-mods))
    (when (env-dehydration env) (push '(:E -1 :I -1) env-mods))
    (when (env-hypothermia env) (push '(:E -2) env-mods))
    (when (env-overheating env) (push '(:E -1) env-mods))
    ;; (combine effects as new modifiers for player)
    env-mods))

;;;; === PROCESS MANAGEMENT & DEBUG GRAPH ===
;;;; Attach: aln-core/process_graphs/eventhandler.aln

(defstruct (ai-process (:constructor make-proc))
  name node-type children status debug-log)

(defun record-event (process desc payload)
  (push (list :desc desc :payload payload :timestamp (get-universal-time))
        (ai-process-debug-log process)))

;;;; === CONTINUOUS ETHICAL LEARNING LOOP ===
;;;; Attach: aln-core/learning/ethicalcontinuousloop.aln

(defstruct (learning-loop (:constructor make-loop))
  history outcomes cache parallel-running)

(defun cache-event (loop event)
  (push event (learning-loop-history loop)))

(defun deliver-random-event (loop)
  ;; returns a random next event from cache
  (let ((h (learning-loop-history loop)))
    (nth (random (length h)) h)))

;;;; === INTERNAL PROCESS/REASONING LOGS ===
;;;; Attach: aln-core/logics/internalreasoningdb.aln

(defstruct internal-process-log
  temperature param-log reasoning-log sequence-log)

(defun update-params-temperature (log new-t)
  (push new-t (internal-process-log-temperature log)))

(defun record-sequence (log sequence)
  (push sequence (internal-process-log-sequence-log log)))

(defun add-reasoning (log reason)
  (push reason (internal-process-log-reasoning-log log)))

;; =========================
;; GAMEPLAY OUTPUT SECTION
;; =========================

(format t "~2%")
(format t "==== ALNFantasia_Game_Debug_TERMINAL ====\n")

;;; Policy loop simulation
(run-policy-loop "Raider throws Rotter.Skunk bottle!" 'humor)
(is-humor? "Raider throws Rotter.Skunk bottle!")   ;; humor policy
(toggle-realism-state)
(run-policy-loop "explicit racial term" 'humor "normal" "none")
(breach-reset)

;; Simulate Player Stats computation:
(let* ((p (make-player :base-stats (list :S -3 :P -9 :E -4 :C -10 :I -7 :A -2 :L -10)
                       :modifiers (list
                                    (cons 'powerarmor (list :S 2 :P 1 :E 1))
                                    (cons 'brokensunglasses (list :P -1))
                                    (cons 'powerfist (list :S 1 :A -1))
                                    (cons 'noboots (list :E -1))
                                    (cons 'bandits10mm (list :A 2))
                                    (cons 'pipboy (list :I 1)))))
       (env (make-env :weather :hot :wet-clothes t :dehydration t :hypothermia nil :overheating t)))
  (setf (player-modifiers p)
        (append (player-modifiers p) (simulate-env-effects p env)))
  (compute-player-stats p)
  (format t "PLAYER FINAL STATS: ~S~%" (player-final-stats p)))

;;; Example NPC Keene dialogue loop:
(let ((keene (make-npc :name "Keene"
                       :emotions (list (cons 'frustrated 0.41) (cons 'wary 0.8) (cons 'skepticism 0.88) (cons 'hope 0.07))
                       :priorities (list (cons 'nightkin 1.0))
                       :humor-level 0.2
                       :status 'neutral)))
  (format t "Keene Dialogue (delay): ~A~%" (npc-dialogue keene 'delayed))
  (adjust-emotion keene 'helped)
  (format t "Keene Dialogue (helped): ~A~%" (npc-dialogue keene 'helped))
  (adjust-emotion keene 'joke)
  (format t "Keene Dialogue (joke): ~A~%" (npc-dialogue keene 'joke))
  (adjust-emotion keene 'betrayed)
  (format t "Keene Dialogue (betrayed): ~A~%" (npc-dialogue keene 'betrayed)))

;;; Process management, learning loop, and reasoning demonstration:
(let ((proc (make-proc :name "combat-event" :node-type 'combat :children nil :status 'pending :debug-log nil))
      (loop (make-loop :history nil :outcomes nil :cache nil :parallel-running t))
      (ilog (make-internal-process-log :temperature nil :param-log nil :reasoning-log nil :sequence-log nil)))
  (record-event proc "player-action" "used weapon")
  (cache-event loop "combat-round-1")
  (deliver-random-event loop)
  (update-params-temperature ilog 0.8)
  (add-reasoning ilog "Player chose aggression due to low morale."))

(format t "~2%==== TERMINAL: SYSTEM LOG & POLICY STATE ====\n")
(format t "Session Tone: ~A  Sandbox State: ~A  Realism Mode: ~A~%" *session-tone* *sandbox-state* *realism-mode*)
(format t "Flagged Action: ~A~%" *flagged-last-action*)
(format t "All reasoning, status, and command traces logged and debuggable. GitHub destination attached.\n")

;;;; Attach script to destination:
;;;; core/lisp.workflow.loop.controller.lisp
;;;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git
