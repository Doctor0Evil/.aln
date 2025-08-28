;; File: /workflows/correct-files.lisp
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun workflow-correct-files (branch &optional (group-id nil))
  (let* ((group (or group-id (format nil "correct-files-~A-~A" branch (timestamp))))
         (workflow-path ".github/workflows/correct-files.yml"))
    (when (workflow::file-exists-p workflow-path)
      (workflow::set-concurrency-group group))
    (catch 'cancel
      (while t
        (unless (workflow::step-success-p)
          (workflow::log-debug "Step failed or canceled"))
        (if (workflow::canceled-p)
            (progn
               (workflow::reroute-group (generate-unique-name))
               (continue))
            (throw 'cancel t))))))
