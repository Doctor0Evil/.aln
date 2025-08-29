;; src/anomaly-parser.lisp
(defun parse-anomaly (input)
  (let* ((tokens (split-sequence #\Space (substitute #\Space #\. input)))
         (mapped (mapcar #'classify-token tokens)))
    (cond
      ((and (member "fuck" tokens :test #'string=)
            (member "glitch" tokens :test #'string=))
       (format t "~&[WARN]: Profane glitch detected! Yikes-mode engaged."))
      (t
       (format t "~&[INFO]: Misc input: ~A" tokens)))
    mapped))

(defun classify-token (tok)
  (cond
    ((string= tok "beep") 'audio-event)
    ((string= tok "fuck") 'profane-marker)
    ((string= tok "glitch") 'error-descriptor)
    ((string= tok "yikes!") 'user-reaction)
    (t 'unknown)))
