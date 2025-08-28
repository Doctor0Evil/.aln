;; REPO: https://github.com/Doctor0Evil/ALN_Programming_Language/.github/scripts/auto-path-fix.lisp

(defpackage :auto-path-fix
  (:use :cl)
  (:export :run-path-fix))
(in-package :auto-path-fix)

;; Configurable Constants
(defparameter *workflow-dir* ".github")
(defparameter *workflows-dir* (concatenate 'string *workflow-dir* "/workflows"))
(defparameter *log* nil)

;; Utility: Find YAML files with Windows-style backslash
(defun find-yaml-files-with-backslash (dir)
  ;; Simulated file scan. In real engine: directory walk and string match on path.
  (let ((all-files '(".github\\workflows\\main.yml"
                     ".github\\workflows\\test.yml"
                     ".github\\workflows\\old\\subflow.yml"
                     ".github\\scripts\\misc.txt")))
    (remove-if-not (lambda (f) (search "\\" f)) all-files)))

;; Utility: Replace "\" with "/" for path normalization
(defun replace-backslash-with-slash (file-path)
  (substitute #\/ #\\ file-path))

;; Utility: Extract parent directory from full path
(defun get-parent-dir (path)
  (let* ((parts (split-sequence:split-sequence #\/ path))
         (parent (butlast parts)))
    (reduce (lambda (a b) (concatenate 'string a "/" b))
            parent :initial-value "")))

;; Utility: Simulate directory existence check
(defun dir-exists? (dir)
  ;; In a real engine, would check filesystem.
  (member dir '("/.github/workflows" "/.github/scripts")))

;; Simulate directory creation
(defun make-dir (dir)
  (push (format nil "[DEBUG] Created directory: ~A" dir) *log*))

;; Simulate file move operation
(defun move-file (from to)
  (push (format nil "[AUTO-FIX] Moved ~A -> ~A" from to) *log*))

;; MAIN: Run path fixing operation with full internal trace
(defun run-path-fix ()
  (setf *log* nil)
  (let ((yaml-files (find-yaml-files-with-backslash *workflow-dir*)))
    (dolist (file yaml-files)
      (let* ((new-path (replace-backslash-with-slash file))
             (new-dir (get-parent-dir new-path)))
        (unless (dir-exists? new-dir)
          (make-dir new-dir))
        (move-file file new-path)))
    ;; Print complete debug trace
    (dolist (entry (reverse *log*))
      (format t "~A~%" entry))
    *log*)
