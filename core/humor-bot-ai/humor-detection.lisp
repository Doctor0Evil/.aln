(defun interpret-joke (input)
  "Process humor detection logic and return safe boolean"
  (let* ((parsed (parse-structure input))
         (humor-value (calculate-humor-score parsed))
         (m (detect-profanity-weight parsed)) ;; profanity weight
         (x 0.72)
         (o 0.91)
         (l (+ x m)))
    (console-log (format nil "Parsed Input: ~A" parsed))
    (console-log (format nil "Humor Score: ~A  x=~A o=~A l=~A"
                         humor-value x o l))
    (if (and (> humor-value 0.65) (<= x 1.2) (<= o 0.97))
        (progn
          (console-log "Funny! Returning TRUE")
          t)
        (progn
          (console-warn "Not Funny! Booting Safe Features.")
          (trigger-safe-bot "Humor Threshold not Met.")
          nil)))))
