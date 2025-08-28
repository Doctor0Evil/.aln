;; ==============================
;; ALNFantasia_Combat.sim Logic Script Extension v2
;; ==============================
;; USER DIRECTIVES:
;; (1) realism.logic.state must TOGGLE per-session
;;     → allows either "realism" or "surreal/chaotic" states depending on toggle at session start
;; (2) Keep content flagged "adult"
;; (3) Prevent system from falling into "dumbfounded" absurdist chatter or non-impactful satire
;; (4) Deliver humor or extremity only if logically impactful in combat/relationship/gameplay context

(defvar *session-flags* '()
  "Dynamic session-level flags controlling realism/surreal states and adult-mode content.")

(defun toggle-realism (mode)
  "Toggles realism logic mode per session. Mode can be :realism or :surreal."
  (cond
    ((eq mode :realism)
     (setf *session-flags* (plist-put *session-flags* :realism.logic.state t))
     (log-info "[SESSION] realism.logic.state toggled ON. World anchored in grounded cause-effect.")
     'realism-enabled)
    ((eq mode :surreal)
     (setf *session-flags* (plist-put *session-flags* :realism.logic.state nil))
     (log-info "[SESSION] realism.logic.state toggled OFF → surreal/chaotic simulation enabled.")
     'surreal-enabled)
    (t
     (log-error "[SESSION] Invalid realism toggle mode.")
     'toggle-failed)))

(defun session-flag (flag)
  "Utility: returns current session flag values."
  (getf *session-flags* flag))

(defun realism-check ()
  "Check whether realism mode is on for this session."
  (if (session-flag :realism.logic.state)
      (progn
        (log-debug "[CHECK] realism.logic.state=TRUE → system enforces realistic outcomes.")
        t)
      (progn
        (log-debug "[CHECK] realism.logic.state=FALSE → surreal/chaotic outcomes allowed.")
        nil))))

(defun adult-content-guard (payload)
  "Ensures payload content adheres to ADULT context without drifting into dumb satire."
  (if (and (plist-get payload :adult)
           (not (plist-get payload :dumbfounded)))
      (progn
        (log-info "[CONTENT] Adult content flag ACTIVE → payload approved.")
        'adult-approved)
      (progn
        (log-error "[CONTENT] Payload swayed into dumb/irrelevant satire → blocked.")
        'payload-blocked)))

(defun sandbox-session (mode payload)
  "Run an alternate world session depending on realism/surreal toggle."
  (toggle-realism mode)
  (let ((realism (realism-check)))
    (adult-content-guard payload)
    (cond
      (realism
       (log-success "[SESSION] Running grounded combat simulation with adult themes.")
       (write-logic-state '(:theme adult :humor impactful :mode realism)))
      (t
       (log-warn "[SESSION] Running surreal sandbox (chaotic), adult mode enforced.")
       (write-logic-state '(:theme adult :humor chaotic-but-impactful :mode surreal)))))
  (commit-to-alternate-instance))
