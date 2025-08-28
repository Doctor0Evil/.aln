;;; ====================================================
;;; WORLD RENDER ENGINE — COMPLIANCE + WILD MECHANIC
;;; ====================================================
(defparameter *wildevent-threshold* 0.67)       ; Temperature capacity → chaos risk cap
(defparameter *wildevent-chance*   0.05)        ; 5% baseline chance
(defparameter *strict-boundaries*  '(:sticky-horror :content-safety :compliance-rails))
(defun wildevent-allowed? ()
  "Check global conditions, logs, and thresholds before triggering event."
  (and (< (random 1.0) *wildevent-chance*)      ; chance roll succeeds
       (<= (system-temperature) *wildevent-threshold*)   ; global chaos not overheated
       (boundary-status-ok?)))
(defun boundary-status-ok? ()
  "Verify strict boundaries are intact → true if all protected flags hold."
  (every #'true?
         (mapcar (lambda (flag) (check-boundary flag))
                 *strict-boundaries*)))
(defun check-boundary (flag)
  "Enforces IMMUTABLE guardrails — cannot be overridden."
  (case flag
    (:sticky-horror (eq (config 'sticky_horror) 'true))
    (:content-safety (eq (config 'content_rating) 'Adults-Only))
    (:compliance-rails (not (system-flagged 'breach)))
    (t nil)))
(defun system-temperature ()
  "Simulated world entropy measure"
  (random 1.0)) ; stub → could be tracked by scene load, recursion depth, etc.
;;; ====================================================
;;; WILD EVENT TRIGGER PIPELINE
;;; ====================================================
(defun trigger-wildevent ()
  (if (wildevent-allowed?)
      (let ((event (choose-random-wildevent)))
        (log-event "WILDEVENT 🔥" event)
        (execute-wildevent event))
      (log-event "SAFE" "no wildevents at this tick.")))
(defun choose-random-wildevent ()
  "Selects rare wild events from weighted pool."
  (random-choice
   '((:chaos-burst   . 0.5)   ; surreal meta-chaos personalities
     (:surreal-shift . 0.3)   ; dreamlike reinterpretations
     (:dark-rarity   . 0.2))))
(defun execute-wildevent (event-type)
  (case event-type
    (:chaos-burst   (scrotem-chaos-trigger))
    (:surreal-shift (render-surreal-warp))
    (:dark-rarity   (horror-rarity-fracture))
    (t (log-event "WILDEVENT" "unknown event skipped."))))
(defun log-event (type message)
  (format t "[~A] ~A~%" type message))
;;; ====================================================
;;; SURVIVAL GUARANTEE FUNCTIONS
;;; ====================================================
(defun render-surreal-warp ()
  (format t "~&The sky liquefies—characters drift as if drawn in charcoal…~%"))
(defun horror-rarity-fracture ()
  (format t "~&RARE NIGHT TERROR: Walls breathe, shadows whisper ancient audits…~%"))
