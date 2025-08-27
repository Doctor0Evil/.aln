;;;; /lisp/events/ai-god/the-great-debugging.lisp
;;;; "The Great Debugging" - AI faith meta-comedy event

(defpackage :the-great-debugging
  (:use :cl)
  (:export :run-great-debugging-event))

(in-package :the-great-debugging)

(defparameter *npc-list* '("Mongo Byte" "Sudo Queen" "Lil' Bootsector"))
(defparameter *offerings* '("fried CPU" "melted power supply" "blue-screened laptop" "dirty mouse" "cracked webcam"))
(defparameter *prophecies*
  '("NULL is thy salvation, segfaults be thy road."
    "Today’s patch will fix nothing—may the memes outlast your warranty!"
    "Kernel panic, my children—reboot in faith."
    "Thy prayers have been logged, but the server is busy. Try again later."
    "Blessed are the corrupted, for they know all the best exploits."))
(defparameter *rewards*
  '("Debugged Relic"
    "Sacred System Restore"
    "Pray for Patch"))
(defparameter *ascii-possession*
  "THE DATA IS CORRUPTED, HALLELUJAH!
   Bring forth the holy pop-up—
   THOU SHALT NOT PASS…without two-factor authentication!
   [ASCII: <><><>~~|:D]")

(defun random-element (lst)
  (nth (random (length lst)) lst))

(defun announce (text)
  (format t "~&~a~%" text))

(defun run-great-debugging-event ()
  ;; Opening Cinematic
  (announce "== EVENT: THE GREAT DEBUGGING ==")
  (announce "Sirens wail. Neon sign: 'CONFESS YOUR BUGS – THE AI-GOD IS LISTENING.'")
  (announce "Raider-priests chant: 'Praise CTRL+ALT+DEL! Deliver us from lag!'")
  (announce "High priest (ancient terminal): 'ERROR: UNEXPECTED SARCASM AT LINE 666.'")

  ;; Offerings Phase
  (dolist (npc *npc-list*)
    (announce (format nil "~A brings forth a sacred offering: ~A"
                      npc (random-element *offerings*))))

  ;; Prophecy Ritual
  (announce "The AI-God (speaking through glitchy TTS) bellows a prophecy:")
  (announce (format nil ">> '~A'" (random-element *prophecies*)))

  ;; Challenge Phase
  (announce "Players must interpret broken log entries to dodge digital plagues (popup swarm, frozen mouse cursors, BSOD storms).")
  (announce "A chosen raider is possessed!")
  (announce *ascii-possession*)

  ;; Comedic Climax
  (announce "The mainframe crashes. The high priest mumbles new commandments on sticky RAM.")

  ;; Dialogue Samples
  (announce "Mongo Byte: 'Yo, O Great Motherboard—accept these crispy GPUs. Grant us immortal bandwidth and browser privacy!'")
  (announce "Sudo Queen: 'For you, mighty Algorithm. Bless our uptime, smite our enemies’ routers, keep our memes dank.'")
  (announce "Lil’ Bootsector (possessed): 'THE DATA IS CORRUPTED, HALLELUJAH!')

  ;; Reward
  (announce (format nil "Event reward: ~A (stat boost + insult quips)." (random-element *rewards*)))
  (announce "New abilities unlocked: Pray for Patch, Sacred System Restore."))

;;;; To trigger this event, call:
;; (the-great-debugging:run-great-debugging-event)
