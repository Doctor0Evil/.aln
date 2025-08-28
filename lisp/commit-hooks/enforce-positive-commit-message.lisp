(defun enforce-positive-commit-message (commit-file)
  "Enforces positivity in commit messages by blocking negative words
   and suggesting constructive alternatives."
  (let* ((msg (with-open-file (stream commit-file :direction :input)
                (let ((lines '()))
                  (loop for line = (read-line stream nil)
                        while line
                        do (push line lines))
                  (format nil "~{~A~%~}" (reverse lines)))))
         ;; Forbidden words mapped to suggested alternatives
         (negatives '(("fix" . "improve")
                      ("bug" . "issue")
                      ("fail" . "challenge")
                      ("error" . "inconsistency")
                      ("broken" . "unstable")))
         (matched (remove-if-not
                   (lambda (pair)
                     (search (string-downcase (car pair))
                             (string-downcase msg)))
                   negatives)))
    (if matched
        (progn
          (format t "😢 Sad words detected in commit message:~%")
          (loop for (bad . suggestion) in matched
                do (format t " - ~A → Consider using: ~A~%" bad suggestion))
          (format t "~%Please rephrase with constructive or positive wording.~%")
          (uiop:quit 1))
        (progn
          (format t "✅ Commit message is positive and constructive! Proceeding...~%")
          (uiop:quit 0)))))
