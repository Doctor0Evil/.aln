;;;; ============================================================
;;;; Combat_AI_Extensions :: Stateful Decision System
;;;; ============================================================
;;;; This extends the basic AI tree into MEMORY-driven decisions.
;;;; Adds persistence across turns: AI tracks past failures (taunts,
;;;; retreats, panics) to gradually adapt its choices mid-combat.
;;;; ============================================================

(in-package :combat-sim)

;;; --------------------------------------------------------------
;;; EXTENDED PERSONALITY STATE
;;; --------------------------------------------------------------

(defstruct ai-memory
  last-action         ;; last chosen action
  taunt-failures 0    ;; number of failed taunts (ignored / resisted)
  morale-collapse 0   ;; persistent morale damage stack
  berserk-uses 0      ;; how many times entity has gone berserk
  retreat-attempts 0  ;; times tried to escape
  success-history '()) ;; track [(:action success-bool)...]

(defun make-ai-state (entity personality)
  "Initialize per-entity state memory for AI decisions."
  (list :entity entity
        :pp personality
        :memory (make-ai-memory)))

(defun recall-memory (ai-state) (getf ai-state :memory))
(defun recall-pp (ai-state) (getf ai-state :pp))
(defun recall-entity (ai-state) (getf ai-state :entity))

;;; --------------------------------------------------------------
;;; AI DECISION (STATEFUL)
;;; --------------------------------------------------------------

(defun ai-decide-next-action+ (ai-state)
  "Smarter decision tree with state/lookback correction."
  (let* ((entity (recall-entity ai-state))
         (pp (recall-pp ai-state))
         (mem (recall-memory ai-state))
         (hp-percent (/ (entity-hp entity)
                        (max 1 (entity-max-hp entity))))
         (morale (max 0 (- (entity-morale entity)
                           (ai-memory-morale-collapse mem))))
         (decision nil))

    (setf decision
          (cond
            ;; PANIC + RETREAT (but check if multiple attempts already failed)
            ((and (< hp-percent (personality-profile-panic-trigger pp))
                  (> (personality-profile-cowardice pp) 60)
                  (< (ai-memory-retreat-attempts mem) 2))
             (incf (ai-memory-retreat-attempts mem))
             :retreat)

            ;; BERSERK – but avoid repeating more than once
            ((and (< hp-percent (personality-profile-berserk-trigger pp))
                  (> (personality-profile-aggression pp) 70)
                  (< (ai-memory-berserk-uses mem) 1))
             (incf (ai-memory-berserk-uses mem))
             :berserk)

            ;; If HP critical and morale sliding, might flee
            ((<= hp-percent 0.25) (if (< morale 40) :flee :fight-desperate))
            ((<= hp-percent 0.50) (if (< morale 60) :defend :fight-sustain))

            ;; Taunting, but suppressed if many failures
            ((and (> morale 60)
                  (< (random 100) (personality-profile-taunt-tendency pp))
                  (< (ai-memory-taunt-failures mem) 2))
             :taunt)

            ;; Otherwise normal attack / hesitation
            ((< morale 25) :hesitate)
            (t :attack)))

    ;; Update last-action
    (setf (ai-memory-last-action mem) decision)
    ;; Log action
    (append-log (fmt-debug-entry :ai-decision+ :entity (entity-name entity)
                                 :hp-percent hp-percent :morale morale
                                 :last-action decision :state mem))
    decision))

;;; --------------------------------------------------------------
;;; FEEDBACK LOOP: OUTCOME OF ACTIONS
;;; --------------------------------------------------------------

(defun ai-feedback (ai-state action success?)
  "Registers results of action into memory for tweaking biases next turn."
  (let ((mem (recall-memory ai-state)))
    ;; Push into success-history
    (push (list :action action :success success?) (ai-memory-success-history mem))

    ;; Morale collapse accumulation (for hesitation/flee)
    (unless success?
      (cond
        ((eq action :taunt) (incf (ai-memory-taunt-failures mem)))
        ((eq action :attack) (incf (ai-memory-morale-collapse mem) 5))
        ((eq action :retreat) (incf (ai-memory-morale-collapse mem) 10))
        (t (incf (ai-memory-morale-collapse mem) 2))))))

;;; --------------------------------------------------------------
;;; ACTION PERFORMANCE (DESCRIPTIVE OUTPUT)
;;; --------------------------------------------------------------

(defun perform-ai-action+ (entity action)
  (case action
    (:panic   (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Panics completely, drops items.")))
    (:retreat (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Attempts to withdraw from fight.")))
    (:berserk (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Bloodlust overwhelms, defense collapses.")))
    (:taunt   (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Spits out taunt to provoke morale contest.")))
    (:attack  (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Moves in with direct strike.")))
    (:fight-sustain (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                                 :desc "Steady offense, preserving stamina.")))
    (:fight-desperate (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                                   :desc "Wild, reckless attack attempt.")))
    (:flee    (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Running away in dread.")))
    (:defend  (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                           :desc "Raises guard, shield stance.")))
    (:hesitate (append-log (fmt-debug-entry :ai-action :entity (entity-name entity)
                                            :desc "Freezes, unable to commit."))))
  action)
