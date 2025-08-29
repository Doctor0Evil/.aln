(defpackage :corrections-workflow
  (:use :cl :virta-sys))

(in-package :corrections-workflow)

(defun parse-error-lines (log-path)
  "Extract lines containing 'Error:' from the workflow log."
  (with-open-file (s log-path)
    (loop for line = (read-line s nil)
          while line
          when (search "Error:" line)
            collect line)))

(defun send-correction (error-string)
  "Send correction notice to Virta Sys correction API endpoint."
  (virta-sys:api-post "/api/virta-sys/corrections"
                      :data (list :message error-string)))

(defun corrections-workflow (job-id log-path &optional (auto-rerun t))
  (let ((errors (parse-error-lines log-path)))
    (if errors
        (progn
          (format t "Detected errors: ~A~%" errors)
          (dolist (err errors) (send-correction err))
          (when auto-rerun
            (virta-sys:trigger-workflow "workflow_dispatch" :job-id job-id)
            (format t "Triggered rerun for job: ~A~%" job-id))
          (list :status "corrected & re-run" :errors errors))
        (list :status "no errors"))))

(defun approve-and-deploy-fixes (fix-branch approver)
  (virta-sys:merge-branch fix-branch "main")
  (virta-sys:trigger-deploy :env "production")
  (format t "Deployment triggered by ~A~%" approver)
  (list :status "deployed" :branch fix-branch))
