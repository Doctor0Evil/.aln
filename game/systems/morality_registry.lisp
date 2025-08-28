;;; =====================================================
;;; CORE MORAL COMPASS & CRIME-PUNISHMENT REGISTRY (ALN)
;;; =====================================================

(defstruct morality
  alignment       ;; :lawful, :neutral, :chaotic, etc.
  crime_flag      ;; t/nil
  reputation      ;; float (0-100)
  mercy_level     ;; float (0.0-1.0)
  cooldown_timer  ;; int (turns or IRL minutes)
)

(defstruct crime-log
  crime-type      ;; :assault, :murder, :theft, etc.
  actor           ;; :player or :npc
  target          ;; target-id
  witnesses       ;; list of nearby NPCs
  zone            ;; region/village
  severity        ;; int (1-10)
  timestamp       ;; turn/time
)

(defparameter *morality-registry* (make-hash-table :test #'equal))
(defparameter *crime-log-book* nil)

;;; Register crime event and compute consequences
(defun register-crime-event (actor crime-type target zone timestamp)
  (let* ((severity (compute-severity crime-type zone))
         (witnesses (detect-witnesses actor zone))
         (crime (make-crime-log :crime-type crime-type
                                :actor actor
                                :target target
                                :witnesses witnesses
                                :zone zone
                                :severity severity
                                :timestamp timestamp)))
    (push crime *crime-log-book*)
    (update-morality actor crime-type severity)
    (dispatch-consequences actor crime-type severity witnesses zone)))

;;; Update morality for actors
(defun update-morality (actor crime-type severity)
  (let* ((morality (gethash actor *morality-registry*)))
    ;; Reputation drops with crime severity; mercy may temper penalty
    (incf (morality-reputation morality) (- (* -5 severity) (* (morality-mercy_level morality) 3)))
    (setf (morality-crime_flag morality) t)
    (setf (gethash actor *morality-registry*) morality)))

;;; Dispatch proper consequences based on crime severity
(defun dispatch-consequences (actor crime-type severity witnesses zone)
  (cond
    ((>= severity 8)
     (trigger-guard-kill-on-sight actor zone))
    ((>= severity 4)
     (trigger-guard-arrest-sequence actor zone))
    ((>= severity 2)
     (trigger-social-repercussions actor zone))
    (t
     (apply-civic-warning actor zone))))

;;; Escape/Exit Logic based on decision
(defun escape-route (actor crime-type severity)
  ;; If caught or in jail, offer mini-games, bribes, or time-escape
  (cond
    ((and (triggered-arrest? actor zone) (in-jail? actor))
     (run-jail-escape-minigame actor))
    ((triggered-guard-kos? actor zone)
     (run-survival-escape-sequence actor))
    ((triggered-social-repercussion? actor)
     (offer-social-redemption actor))
    (t
     (apply-cooldown-timer actor))))

;;; Respond to resolution
(defun resolve-escape-outcome (actor outcome)
  (case outcome
    (:escaped
      (restore-reputation actor 5) ; soft forgive
      (clear-active-crime actor))
    (:caught
      (apply-fine actor)
      (apply-cooldown-timer actor))
    (:redemption
      (grant-quest-forgiveness actor)
      (clear-active-crime actor))
    (otherwise
      (log "Outcome unresolved; default cooldown applied."))))

;;; Event Trigger Registry
(defun handle-player-action (player input context)
  ;; Parse input for potential crime
  (let ((crime-type (detect-crime input)))
    (if crime-type
      (register-crime-event player crime-type (detect-target input context) (detect-zone player) (game-time)))
    (let ((decision (parse-decision input context)))
      (branch-moral-logic player decision)))

  ;; Route based on decision and consequences
  (let ((morality (gethash player *morality-registry*)))
    (if (morality-crime_flag morality)
      (escape-route player (detect-crime input) (compute-severity (detect-crime input) (detect-zone player)))
      (reward-moral-choice player decision))))

;;; Branching Moral Logic flows
(defun branch-moral-logic (actor decision)
  (cond
    ((eq decision :attack_npc)
      (register-crime-event actor :assault (detect-target decision) (detect-zone actor) (game-time)))
    ((eq decision :kill_npc)
      (register-crime-event actor :murder (detect-target decision) (detect-zone actor) (game-time)))
    ((eq decision :help_npc)
      (reward-moral-choice actor :help))
    ((eq decision :bribe_guard)
      (apply-bribe-resolution actor))
    ((eq decision :confess)
      (grant-redemption-resolution actor))
    ((eq decision :flee)
      (escape-route actor :flee (morality-reputation (gethash actor *morality-registry*))))
    (t
      (log "Neutral or undefined action. No special consequence."))))

;;; Example subroutines for mini-games, warnings, reputation
(defun run-jail-escape-minigame (actor)
  (log "Jail escape puzzle triggered!")
  ;; Puzzle, stealth, or random chance
  (if (player-success? actor)
    (resolve-escape-outcome actor :escaped)
    (resolve-escape-outcome actor :caught)))

(defun apply-civic-warning (actor zone)
  (log (format nil "Civic warning issued in ~A" zone))
  (restore-reputation actor 1))

(defun reward-moral-choice (actor decision)
  (incf (morality-reputation (gethash actor *morality-registry*)) 10)
  (log (format nil "Heroic act by ~A: ~A" actor decision)))

;;; Helper: Clear flags after redemption or cooldown
(defun clear-active-crime (actor)
  (setf (morality-crime_flag (gethash actor *morality-registry*)) nil)
  (decf (morality-cooldown_timer (gethash actor *morality-registry*)) 1))

;;; Helper: Apply fines, bribe, or reduce reputation
(defun apply-fine (actor)
  (log (format nil "Fine paid by ~A; reputation partially restored." actor))
  (restore-reputation actor 3))

;;; Example event: guards respond to crime
(defun trigger-guard-arrest-sequence (actor zone)
  (log (format nil "Guards in ~A attempt to arrest ~A." zone actor))
  (if (player-success? actor)
    (escape-route actor :arrest (compute-severity :arrest zone))
    (run-jail-escape-minigame actor)))

(defun trigger-guard-kill-on-sight (actor zone)
  (log (format nil "Guards in ~A now kill ~A on sight." zone actor))
  (run-survival-escape-sequence actor))

;; All events/branches reference world logic: always allow escape, redemption, never hard-lock
;; Can easily plug into master orchestrator for consequences, branch adjustment, or debug tracing
