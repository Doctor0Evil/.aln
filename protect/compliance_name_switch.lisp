;;below is an example to "meet compliance";
; FILE: github.com/Doctor0Evil/ALN_Programming_Language.git/protect/compliance_name_switch.lisp

(defpackage :name-protect-compliance
  (:use :common-lisp))
(in-package :name-protect-compliance)

(defparameter *protected-names*
  '(("tommy.trust.zone" . "custom.auto")
    ("toilet.mouth" . "wanna-get-flushed")   ; Example: "protected.name":'remove'
    ;; Add more pairs here...
    ))

(defun find-protected-alias (string)
  "Returns the compliant alias or original if not protected"
  (let ((lower-string (string-downcase string)))
    (or (cdr (assoc lower-string *protected-names* :test #'string=))
        string)))

(defun compliance-check-transform (text)
  "Scans & transforms any protected names in input text."
  (let ((tokens (cl-ppcre:split "\\b" text)))
    (with-output-to-string (out)
      (dolist (part tokens)
        (princ (find-protected-alias part) out)))))

(defun process-dialogue-input (input)
  "Entry point for dialogue, script or file import workflow, run on ALL user/materials entry."
  (let ((compliance-text (compliance-check-transform input)))
    (log-event (format nil "PROTECT: Input checked: '~A' => '~A'" input compliance-text))
    compliance-text))

;; Utility and Logging
(defun log-event (msg)
  (format t "~&[NAME-COMPLIANCE] ~A~%" msg))

;; Usage/Example
;; (process-dialogue-input "Hi Tommy Vercetti and Towlie, wanna get high?")
;; => will output: "Hi custom.auto and wanna-get-high, wanna get high?"

;; END OF FILE (GITHUB: /protect/compliance_name_switch.lisp)
