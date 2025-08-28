;; Github File Path:
;; https://github.com/Doctor0Evil/ALN_Programming_Language/blob/main/workflow/advanced-correct.git.launch.root.lisp

(defpackage :aln.workflow.git.launch
  (:use :cl :alexandria :uiop :yason))

(in-package :aln.workflow.git.launch)

(defparameter *debug-console-enabled* t)
(defparameter *repo-root* (uiop:getcwd))
(defparameter *main-branch* "main")
(defparameter *git-bin* "git")

(defun debug-log (msg &rest args)
  (when *debug-console-enabled*
    (apply #'format t (concatenate 'string "[DEBUG] " msg "~%") args)))

(defun run-git (&rest args)
  (let* ((cmd (cons *git-bin* args))
         (result (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)))
    (debug-log "Run ~A: ~A" *git-bin* args)
    (debug-log "Output: ~A" result)
    result))

(defun repo-valid-p ()
  (let ((exists (probe-file (merge-pathnames ".git" *repo-root*))))
    (debug-log "Repo root: ~A .git exists: ~A" *repo-root* exists)
    exists))

(defun get-current-branch ()
  (string-trim '(#\Newline #\Return #\Space)
    (run-git "rev-parse" "--abbrev-ref" "HEAD")))

(defun ensure-correct-branch ()
  (let ((current (get-current-branch)))
    (debug-log "Current branch: ~A" current)
    (unless (string= current *main-branch*)
      (debug-log "Switching to ~A branch." *main-branch*)
      (run-git "checkout" *main-branch*))))

(defun launch-root ()
  (debug-log "Launching advanced git workflow at root: ~A" *repo-root*)
  (unless (repo-valid-p)
    (error "Not a git repo root!"))
  (ensure-correct-branch)
  (debug-log "Pulling latest changes...")
  (run-git "pull" "origin" *main-branch*)
  (debug-log "Running full compliance audit...")
  (run-git "status")
  (run-git "log" "-1")
  (run-git "config" "--list")
  (debug-log "Advanced root git launch complete."))

;; Optional: Self-Healing/Auto-Remediation
(defun auto-heal ()
  (debug-log "Auto-heal triggered (if issues found)...")
  (run-git "fetch" "--all")
  (run-git "reset" "--hard" (format nil "origin/~A" *main-branch*))
  (debug-log "Repo reset to main branch upstream."))

;; Entry point example
(launch-root)

;; ===================
;; Debug Console/Workflow Event Trace Example
;; ===================
;;
;; [DEBUG] Launching advanced git workflow at root: /home/developer/ALN_Programming_Language
;; [DEBUG] Repo root: /home/developer/ALN_Programming_Language .git exists: #p"/home/developer/ALN_Programming_Language/.git/"
;; [DEBUG] Current branch: dev
;; [DEBUG] Switching to main branch.
;; [DEBUG] Run git: (checkout main)
;; [DEBUG] Output: Switched to branch 'main'
;; [DEBUG] Pulling latest changes...
;; [DEBUG] Run git: (pull origin main)
;; [DEBUG] Output: Already up to date.
;; [DEBUG] Running full compliance audit...
;; [DEBUG] Run git: (status)
;; [DEBUG] Output: On branch main ...
;; [DEBUG] Run git: (log -1)
;; [DEBUG] Output: commit e6f8e7...
;; [DEBUG] Run git: (config --list)
;; [DEBUG] Output: user.name=...
;; [DEBUG] Advanced root git launch complete.

;;; End-of-module
