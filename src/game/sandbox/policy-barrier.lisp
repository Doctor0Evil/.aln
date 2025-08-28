;;; src/game/sandbox/policy-barrier.lisp
;; "policy-barrier": Prevents over-restriction. Only blocks actual danger, preserves adult humor.

(defparameter *compliance-max-level* 'safe
  "Hard barrier: do not exceed 'safe' restriction; allows adult/edgy humor if not truly unsafe.")

(defun rego-policy-barrier (request-type classification context)
  "Prevents compliance from being over-restrictive. Allows humor.bot.adult if safety is OK.
REQUEST-TYPE: e.g., 'policy-review, 'runtime-check.
CLASSIFICATION: humor, adult, satire, horror.
CONTEXT: runtime metadata/context."
  (let ((dangerous (or (equal classification 'horror)
                       (equal context 'SAFETY)
                       (equal context 'ETHICS))))
    (cond
      ;; Only block if genuinely unsafe/illegal
      (dangerous
        (log (format nil "!! POLICY BLOCKED: Classification/context ~A truly unsafe, obeying max-level barrier." classification))
        (block-execution))
      ;; Otherwise, allow humor (even adult/edgy)
      ((and (member classification '(humor adult satire))
            (equal *compliance-max-level* 'safe))
        (log (format nil "POLICY PASS: Humor/adult content OK at 'safe' compliance barrier. Not over-restricted."))
        (permit-execution))
      (t
        (log (format nil "Policy fallback: Non-humor, non-danger. Default allow.")))
    )))

;; Helper stubs
(defun log (msg)
  (format t "~A~%" msg))

(defun block-execution ()
  (log "Barrier triggered: execution blocked. (Complain to the AI overlords.)"))

(defun permit-execution ()
  (log "Barrier pass: comedy, chaos, and tears of joy permitted!"))
