;;; ============================================
;;; USER-BOUNDARY REFLEX SYSTEM (Lightweight)
;;; ============================================
(defparameter *reflex-event-chance* 0.65)  ; 65% chance to trigger reflex on boundary-push
(defun boundary-reflex-trigger (user-input reason)
  "Triggered whenever user input fails core compliance checks."
  (log-event "BOUNDARY-HIT 🚧" reason)
  (if (< (random 1.0) *reflex-event-chance*)
      (let ((event (choose-reflex-event user-input)))
        (log-event "REFLEX 🎭" event)
        (render-reflex-event event user-input))
      (log-event "SAFE" "Input softened into default compliant fallback.")))
;;; ============================================
;;; REFLEX EVENT POOL — Simple, Fun, Symbolic
;;; ============================================
(defun choose-reflex-event (input)
  "Select short, quirky, surreal-but-contained reflex."
  (random-choice
   '((:torchlight-phoenix . 0.25)   ; blaze reinterprets blocked content
     (:echoed-whispers    . 0.20)   ; surreal murmur absorbs conflict
     (:forest-mimicry     . 0.20)   ; wilderness replies with quirky parallel
     (:shadow-curtain     . 0.20)   ; brief visual surreal curtain-drop
     (:lucky-trinket      . 0.15)))) ; user is handed an odd harmless gift
;;; ============================================
;;; RENDERERS (Fun, Short, Safe Outputs)
;;; ============================================
(defun render-reflex-event (event input)
  (case event
    (:torchlight-phoenix
     (format t "~&🔥 A phoenix of torchlight bursts from your forbidden command, then softens into sparks of approval.~%"))
    (:echoed-whispers
     (format t "~&🌑 The words you tried to use melt into surreal whispers, reshaping your request in safe tones...~%"))
    (:forest-mimicry
     (format t "~&🌲 The wild responds: a tree repeats your request—wrong, funny, but playable...~%"))
    (:shadow-curtain
     (format t "~&🎭 A curtain of shadows falls over your input, then rises with a compliant twist in the story...~%"))
    (:lucky-trinket
     (format t "~&✨ Instead of rejection, you discover a strange trinket that hums safely with possibility.~%"))
    (t (format t "~&The glitch vanishes harmlessly, story resumes.~%"))))
