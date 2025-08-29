;;;; file: src/lisp/workflow_path_fix.lisp
;;;; repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :workflow-path-fix
  (:use :cl))
(in-package :workflow-path-fix)

(defun invalid-workflow-path-p (path)
  "Detects if a workflow path is Windows-style (invalid)."
  (search "\\\\" path))  ;; look for \ backslash

(defun normalize-workflow-path (path)
  "Converts Windows path to Unix path."
  (substitute #\/ #\\ path))

(defun simulate-github-run (path)
  "Simulate GitHub Action path resolution and fix if needed."
  (format t "~%[DEBUG] Checking workflow: ~A~%" path)
  (if (invalid-workflow-path-p path)
      (let ((fixed (normalize-workflow-path path)))
        (format t "[FIX] Invalid path detected! Converting: ~A -> ~A~%" path fixed)
        fixed)
      (progn
        (format t "[OK] Workflow path already valid: ~A~%" path)
        path)))
