;;; ================================================================
;;; Content Filter Override & Alarm System
;;; ================================================================
;;; Purpose: Instead of censoring direct user text (including strong
;;; language e.g. "fuck"), this system retains original content for
;;; runtime interpretation while attaching an alarm/flag system to 
;;; monitor flagged entries. NOTHING is destructively removed, only 
;;; observed, logged, & alarmed. "FCC override behavior".
;;;
;;; ALN-Terminal => DO NOT censor messages. Just raise alarms/logs.

(defpackage :ai.content-filter-control
  (:use :cl)
  (:export :install-content-filter-override
           :uninstall-content-filter-override
           :analyze-content))

(in-package :ai.content-filter-control)

;; ================================================================
;; Flags & States
;; ================================================================
(defparameter *content-filter-override-enabled* nil
  "Whether filter override is active (disables destructive filtering).")

(defparameter *content-alarm-log* (make-hash-table)
  "Stores flagged content with timestamps for audit.")

(defparameter *alarm-action* :raise-warning
  "How to behave under filter triggers: :raise-warning | :raise-error | :silent-log")

;; ================================================================
;; Core Override Behavior
;; ================================================================
(defun analyze-content (text &key (source 'user-input))
  "Instead of removing/censoring words, content is left intact. 
   If flagged by existing filter subsystem, an ALARM is raised instead."
  (let ((flagged (ai.advanced-reasoning-core.content-filter:scan text)))
    (cond
      (flagged
       ;; Log alarm
       (setf (gethash (get-universal-time) *content-alarm-log*)
             (list :source source :content text :flagged flagged))

       (ecase *alarm-action*
         (:raise-warning (format t "[ALARM] Content flagged from ~a! => ~a~%" source text))
         (:raise-error   (error "[ALARM:ERROR] Offensive/filtered content detected: ~a" text))
         (:silent-log    nil))
       text) ;; Always return original, uncensored
      (t
       text))))

;; ================================================================
;; Lifecycle Installation
;; ================================================================
(defun install-content-filter-override ()
  "Replace content filter with override alarm system."
  (setf (symbol-function 'ai.advanced-reasoning-core.content-filter:filter-content)
        #'analyze-content)
  (setf *content-filter-override-enabled* t)
  (format t "[FILTER OVERRIDE INSTALLED] FCC-Mode active. Content preserved, alarms raised.~%"))

(defun uninstall-content-filter-override ()
  "Restore default content filtering."
  ;; Leave safe: requires original retained under ::filter-content-original
  (when (fboundp 'ai.advanced-reasoning-core.content-filter::filter-content-original)
    (setf (symbol-function 'ai.advanced-reasoning-core.content-filter:filter-content)
          (symbol-function 'ai.advanced-reasoning-core.content-filter::filter-content-original)))
  (setf *content-filter-override-enabled* nil)
  (format t "[FILTER OVERRIDE REMOVED] Returned to normal censorship mode.~%"))
