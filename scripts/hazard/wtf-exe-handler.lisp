;; File: /scripts/hazard/wtf-exe-handler.lisp
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun run.wtf-exe (game-context)
  "Handles the side effects of running a suspicious or malicious EXE in a horror/survival environment."
  (let* ((threat-level (random 10))
         (hallucination-chance (if (> threat-level 5) 0.4 0.1))
         (corruption-chance (if (>= threat-level 7) 0.3 0.12))
         (anomaly-triggered (or (> (random 1.0) (- 1 hallucination-chance))
                                (> (random 1.0) (- 1 corruption-chance)))))
    (when anomaly-triggered
      (setf (gethash 'world-mood game-context) 'corrupted)
      (push "ALERT: System anomaly detected! World state has shifted." (gethash 'system-logs game-context))
      (format t "wtf.exe anomaly: hallucination/corrupted state triggered!~%"))
    (list :event 'run-wtf-exe
          :threat-level threat-level
          :anomaly anomaly-triggered
          :world-context game-context)))

;; Usage: (run.wtf-exe *current-game-context*)
