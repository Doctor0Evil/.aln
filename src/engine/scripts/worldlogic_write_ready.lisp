;; ==============================
;; ALNFantasia_Combat.sim Logic Script Extension
;; ==============================
;; User Request: instead of committing "doom.environment.lock" permanently
;; into the core game.world, we build a construct that allows:
;;   (1) Separation of world logic into "write.ready.state"
;;   (2) Ensures written consequences apply only to /alternate.instance.logic
;;   (3) Adds realism.logic.state gatecheck to ensure only permissible effects
;; Implementation guarantees: main game.world.logic remains untouched

(defvar *world-logic-core* '(:immutable t :permanent true :doom-lock disabled)
  "Canonical, permanent game world. Locked and protected from direct write actions.")

(defvar *write-ready-state* nil
  "Temporary /workspace partition that mirrors the world, separated from canonical game.world.")

(defun realism-logic-check ()
  "Determines if realism-based logic state is active."
  (let ((is-real (system-flag 'is.realism.logic.state)))
    (if (eq is-real t)
        (progn
          (log-info "[REALISM] realism.logic.state check TRUE → writing permitted.")
          t)
        (progn
          (log-warn "[REALISM] realism.logic.state check FALSE → writing denied.")
          nil))))

(defun init-write-ready-state ()
  "Creates a detached instance of the game.world for safe edits."
  (setf *write-ready-state* 
        '(:scope realism :impact none :writes allowed_if_realism t :mirror_of game.world))
  (log-debug "[WORLD] write.ready.state initialized. Non-impactful to canonical world.")
  *write-ready-state*)

(defun write-logic-state (mutation)
  "Applies changes to write.ready.state only IF realism.logic.state == true."
  (if (realism-logic-check)
      (progn 
        (setf *write-ready-state* 
              (append *write-ready-state* (list :mutation mutation)))
        (log-success (format nil "[WRITE] Logic mutation '~a' applied under realism.logic.state." mutation))
        'write.successful)
      (progn
        (log-error "[WRITE] Mutation denied. realism.logic.state flag OFF.")
        'write.denied)))

(defun commit-to-alternate-instance ()
  "Commits temporary logic into alternate.instance.logic only, not canonical world."
  (if *write-ready-state*
      (progn
        (log-info "[COMMIT] Transferring write.ready.state → alternate.instance.logic")
        (values 'commit-success *write-ready-state*))
      (progn
        (log-error "[COMMIT] No write.ready.state detected.")
        'commit-failed)))

(defun system-flag (flag)
  "Faked utility for system flag statuses."
  (case flag
    (is.realism.logic.state t) ;; activate realism for now
    (otherwise nil)))

(defun alt-instance-sandbox ()
  "Bootstrap alternate instance with realism."
  (init-write-ready-state)
  (write-logic-state '(:doom-sequence sandboxed :lethality restricted :ammo non-lethal))
  (commit-to-alternate-instance))

;; Run alternate world sandboxing run
(alt-instance-sandbox)
