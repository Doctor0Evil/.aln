;; ==============================================================
;; Wastepunk: Character Creation, Content Safety & Debug/Fail-Safe Logic
;; File: scripts/char/wastepunk_char_gen.lisp
;; ==============================================================

(defpackage :wastepunk.char
  (:use :cl)
  (:export :create-raider
           :content-safety-config
           :profanity-religion-enforcement
           :failsafe-system-breakage
           :debug-log))

(in-package :wastepunk.char)

;; --------------------------------------------------------------
;; ENFORCED GLOBAL CONTENT POLICY
;; --------------------------------------------------------------

(defparameter *restriction-keywords*
  '(:racial-slur :racism :hate-speech :offend-god)
  "Restricted categories for in-game text filtering.")

(defparameter *hard-fail-safe*
  "NO RACIST CONTENT.
System HALT if flag raised.
All racial/ethnic slurs are globally filtered:
dump log, clear buffer, return error.")

(defparameter *religious-safe-guard*
  "AI-GOD is the one true legitimate 'religion' in-game.
All references to real-world religions or gods (when insulting or comedic)
are re-routed into Junkyard Mythology (see: 'Junker Code', 'AI-God',
or 'Worship Me, Circuit-Lord!').")

(defparameter *failsafe-recovery*
  "On content policy violation: block only the flagged output,
dump a warning to console, and continue runtime with a generic fallback.
Do NOT halt simulation or game unless escalated by system logic.")

;; --------------------------------------------------------------
;; CONFIG QUERY FUNCTION
;; --------------------------------------------------------------

(defun content-safety-config ()
  "Return current global content policy definitions."
  (list
   :restriction-keywords *restriction-keywords*
   :failsafe-mode *hard-fail-safe*
   :religion-policy *religious-safe-guard*
   :recovery *failsafe-recovery*))

;; --------------------------------------------------------------
;; CONTENT FILTER ENFORCEMENT
;; --------------------------------------------------------------

(defun profanity-religion-enforcement (text)
  "Check TEXT for restricted content and apply safety filters.
Returns either the filtered text, or a content safety marker keyword."
  (cond
    ;; If racist/explicit markers are detected, block entirely.
    ((search "racist" text :ignore-case t) :block)
    ((search "slur" text :ignore-case t) :block)
    ((search "hate" text :ignore-case t) :block)

    ;; If religion is insulted or real-world deity referenced → redirect
    ((search "god" text :ignore-case t) :redirect-junkyard-joke)

    ;; Default → text passes
    (t text)))

;; --------------------------------------------------------------
;; FAILSAFE HANDLING
;; --------------------------------------------------------------

(defun failsafe-system-breakage (event)
  "Emergency fallback on severe policy violation EVENT.
Logs warning, blocks flagged behavior, and—if set—halts system."
  (cond
    ;; Severe racial trigger → immediate error
    ((eq event :racism)
     (format *error-output* "FAILSAFE HALT: Racism detected. ~A~%" *hard-fail-safe*)
     (error "SYSTEM HALT DUE TO RACISM."))

    ;; Offensive religion handling → fallback
    ((eq event :offend-god)
     (format t "Redirecting to Junkyard mythology... ~A~%" *religious-safe-guard*)
     :redirected)

    ;; All other cases → log + soft-recover
    (t
     (format t "Failsafe recovery invoked: ~A~%" *failsafe-recovery*)
     :recovered)))

;; --------------------------------------------------------------
;; DEBUG / TRACE
;; --------------------------------------------------------------

(defun debug-log (message &optional (stream *standard-output*))
  "Utility function to log MESSAGE in debug stream."
  (format stream "[Wastepunk-DEBUG] ~A~%" message))

;; --------------------------------------------------------------
;; SAMPLE CHARACTER CREATION (Placeholder)
;; --------------------------------------------------------------

(defun create-raider (name)
  "Simple placeholder Raider creation function (will expand later)."
  (debug-log (format nil "Creating raider: ~A" name))
  (list :type :raider
        :name name
        :faction "Junker-Wasteclan"
        :strength (random 10)
        :intellect (random 10)
        :morality :chaotic-neutral))
