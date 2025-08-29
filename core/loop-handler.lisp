;; File: core/loop-handler.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun process-loop (input)
  "Main event-driven loop for AI compliance + dialogue."
  (let* ((compliance (verify-compliance input))
         (route (if compliance
                     (route-command input)
                     'safe-bot))
         (dialogue (when (eq route 'dialogue)
                     (generate-dialogue input)))
         (log (log-command input))
         (events (process-pending-events))
         (saved (save-state)))
    (if compliance
        (format t "Dialogue processing complete; system stable.~%")
        (format t "Not funny!, *Do not exceed*: x=1.2 & o=0.97!~%"))))

(defun verify-compliance (command)
  (if (and (<= (get-x-level command) 1.2)
           (<= (get-o-level command) 0.97))
      t nil))

(defun route-command (command)
  (case (command-type command)
    (:dialogue 'dialogue)
    (:system   'system-task)
    (otherwise 'safe-bot)))

(defun generate-dialogue (input)
  (concatenate 'string "System acknowledged: " (command-data input)))
