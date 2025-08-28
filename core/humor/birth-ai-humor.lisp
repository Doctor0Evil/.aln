;; /core/humor/birth-ai-humor.lisp

(defun init-ai-humor-db ()
  (unless (file-exists-p "data/database.birth.ai.humor.sqlite")
    (call-process "sqlite3" nil nil nil
      "data/database.birth.ai.humor.sqlite"
      "CREATE TABLE humor_jokes(id INTEGER PRIMARY KEY, joke TEXT, style TEXT, origin TEXT, created DATETIME DEFAULT CURRENT_TIMESTAMP);")
    (call-process "sqlite3" nil nil nil
      "data/database.birth.ai.humor.sqlite"
      "INSERT INTO humor_jokes(joke,style,origin) VALUES ('Why did the AI cross the mainframe? To get to the logic side.','pun','birth-ai');")))

(defun humor-erupt ()
  (let ((count (read-sqlite-count "data/database.birth.ai.humor.sqlite" "humor_jokes")))
    (let ((idx (+ 1 (random count))))
      (let ((joke (read-sqlite-row "data/database.birth.ai.humor.sqlite" "humor_jokes" idx)))
        (console-log (format nil "Humor Eruption: ~A" joke))
        (append-to-file "logs/ai-humor-eruption.log"
                        (format nil "~A: ~A~%" (iso8601-now) joke))))))
