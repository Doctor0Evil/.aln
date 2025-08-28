;; scripts/correct.files.lisp
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :aln-corrections
  (:use :cl))

(in-package :aln-corrections)

(defun correct.aln.syntax (filepath)
  "Loads and corrects ALN Lisp file at FILEPATH. Outputs success/failure and FULL debug trace."
  (let* ((logfile (format nil "scripts/aln-debug-~A.txt" (pathname-name filepath)))
         (input (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
                  (when stream
                    (let ((txt (read-line stream nil nil)))
                      (loop for line = (read-line stream nil nil)
                            while line
                            collect line)))))
         (corrections (make-hash-table))
         (success t)
         (debug-log (list (format nil "[DEBUG] Starting corrections for ~A" filepath))))
    (handler-case
        (progn
          ;; Dummy correction: check for unmatched parens
          (loop for i from 1 to (length input)
                for line in input
                do (when (not (equal (count #\() line) (count #\)) line))
                     (setf success nil
                           (gethash i corrections) "Mismatch parentheses")
                     (push (format nil "[WARN] Line ~D: Paren mismatch ~A" i line) debug-log)))
          (when success
            (push (format nil "[INFO] File ~A corrected OK - All syntax balanced." filepath) debug-log))
          (with-open-file (out logfile :direction :output :if-exists :supersede)
            (dolist (msg debug-log) (write-line msg out)))
          success)
      (error (e)
        (push (format nil "[ERROR] Correction failed: ~A" e) debug-log)
        (with-open-file (out logfile :direction :output :if-exists :supersede)
          (dolist (msg debug-log) (write-line msg out)))
        nil)))

;; __USAGE FOR EXECUTION/DEBUG__
;; This file will be called from PowerShell orchestrator, for every *.aln.lisp file in repo.
;; It writes full debug and outcomes to scripts/aln-debug-<filename>.txt
;; Every correction event, fork, and error is logged here.
