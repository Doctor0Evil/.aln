;;;; ============================================================
;;;; COMPOUNDED LOGIC SCRIPT :: Stateful Combat AI w/ Memory
;;;; ============================================================
;;;; This Lisp block summarizes *all system behaviors* into a
;;;; single in-core script the engine would run during simulation.
;;;; ============================================================

(in-package :combat-sim)

(defun run-ai-turn+ (ai-state)
  "Complete loop: decide, perform, feedback -> update memory."
  (let* ((entity (recall-entity ai-state))
         (decision (ai-decide-next-action+ ai-state))
         (performed (perform-ai-action+ entity decision))
         ;; pretend a random chance of success/failure here
         (success? (> (random 100) 35)))
    ;; feed results back to AI memory
    (ai-feedback ai-state performed success?)
    ;; return outcome payload for engine tick
    (list :entity (entity-name entity)
          :decision performed
          :success success?
          :memory (recall-memory ai-state))))

(defun simulate-n-turns (ai-state turns)
  "Step simulating multiple turns with memory evolving."
  (loop repeat turns
        collect (run-ai-turn+ ai-state)))
