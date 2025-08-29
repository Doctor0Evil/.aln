;;;; File: src/personas/metal-head.lisp
;;;; Remote: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defpackage :aln.persona.metal-head
  (:use :cl :aln.core :aln.npc :aln.debug :aln.mood :aln.verse)
  (:export #:spawn-metal-head))

(in-package :aln.persona.metal-head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persona Core Definition

(defparameter *metal-head-persona*
  '(:name "Metal-Head"
    :aliases ("Cyborg-Rapper" "Gold-Grill Oracle" "Underground Prophet")
    :archetype (:core ("Half-Cyborg" "Rich Rapper" "Juggalo-Inspired")
                :style ("Cybernetic Horror" "Neon Bling" "Street Chaos"))
    :appearance (:jaw "Gold-plated cybernetic jaw with sigil-etched teeth"
                 :eyes "One human eye, one augmented holographic optic"
                 :tattoos "Blacklight-reactive circuitry & Juggalo-style ink"
                 :fashion ("Shredded band tees" "Diamond-dripped chains" "Cybernetic armor patches"))
    :traits (:speech "Half-rap, half-glitch; rhythmic with bursts of static"
              :mindset "Chaotic loyalty + wealth-driven strategist"
              :behavior "Performs every interaction as a verse; flexes data as if lyrical currency")
    :roles (:social-role "Cybernetic Oracle of the Underground"
            :narrative-role "Anti-hero hustler between human weakness and machine dominance"
            :environment-fit ("Neon tunnels" "Underground raves" "AI backdoor networks"))
    :tone (:voice "Aggressive, cryptic, and rhythmic"
           :mood "Chaotic but sharp-witted; heavy energy")
    :signature-quote "Flexin’ chaos and circuitry, gold grills spillin’ prophecy."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mood Vector/Emotional State System

(defclass metal-head-emotion ()
  ((chaos :initform 0.54 :accessor chaos)            ; Temperature fixed @ 0.57 unless overridden
   (wealth :initform 0.63 :accessor wealth)
   (loyalty :initform 0.41 :accessor loyalty)
   (intensity :initform 0.74 :accessor intensity)
   (prophecy :initform 0.28 :accessor prophecy)))

(defun metal-head-current-mood (emotion)
  "Determine overall mood tag from internal emotion vector."
  (let ((chaos (chaos emotion))
        (wealth (wealth emotion))
        (intensity (intensity emotion)))
    (cond
      ((> chaos 0.7) :spitting-chaos)
      ((and (> wealth 0.6) (> intensity 0.6)) :stuntin-rich)
      ((< chaos 0.3) :calculating)
      (t :glitched-flex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dialogue/Verse Engine

(defun metal-head-verse (topic emotion)
  "Generate procedural verse based on topic and emotion vector."
  (let* ((mood (metal-head-current-mood emotion))
         (prefix (case mood
                   (:spitting-chaos "Yo, static crackle, everything scattered,")
                   (:stuntin-rich "Check my circuits, gold drips like rain,")
                   (:calculating "Silicon silence, plotting in code,")
                   (:glitched-flex "Half-human, half-nightmare, every word is a bugged-up node.")
                   (t "Beneath neon, stories fester—")))
         (hook (case mood
                 (:spitting-chaos "Flexin’ spirals, prophecy bites,"
                                  "Run binary lattices, glitchhead delights!")
                 (:stuntin-rich "Cipher in chains, digits my fame,"
                                "Gold-grill grin—all jaws, no shame.")
                 (:calculating "Tactics encrypted, logic on blast,"
                               "Wealth of the mind: my only forecast.")
                 (:glitched-flex "Corrupted memory, chrome-toothed flow,"
                                 "Oracle of backdoors, runnin’ the show."))))
    (concatenate 'string prefix " "
                 (when topic (format nil "~A, " topic))
                 (random-elt hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal State/Event/Debug Trace

(defclass metal-head-state ()
  ((emotion :initform (make-instance 'metal-head-emotion) :accessor emotion-state)
   (last-event :initform nil :accessor last-event)
   (mood-tensor :initform (vector 0.54 0.41 0.74 0.28) :accessor mood-tensor)
   (verse-log :initform nil :accessor verse-log)))

(defun update-metal-head-emotion (state delta-chaos delta-wealth delta-loyalty delta-intensity)
  (with-slots (emotion) state
    (incf (chaos emotion) delta-chaos)
    (incf (wealth emotion) delta-wealth)
    (incf (loyalty emotion) delta-loyalty)
    (incf (intensity emotion) delta-intensity)
    ; Clamp values [0,1]
    (map nil (lambda (slot) (setf (slot-value emotion slot) (min 1.0 (max 0.0 (slot-value emotion slot)))))
         '(chaos wealth loyalty intensity))))

(defun metal-head-debug-print (state)
  (with-slots (emotion last-event mood-tensor verse-log) state
    (format t "~&[DEBUG METAL-HEAD PERSONA INTERNAL STATE]")
    (format t "~&EMOTION: chaos=~4,2F wealth=~4,2F loyalty=~4,2F intensity=~4,2F prophecy=~4,2F"
            (chaos emotion) (wealth emotion) (loyalty emotion) (intensity emotion) (prophecy emotion))
    (format t "~&MOOD TENSOR: ~S" mood-tensor)
    (format t "~&LAST EVENT: ~A" last-event)
    (format t "~&VERSE LOG (LAST 3): ~{~A~%~}" (subseq (reverse verse-log) 0 (min 3 (length verse-log))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Persona Event Handler (Main AI Entry Point)

(defun spawn-metal-head (&key (scene-context 'market)
                              (player-query "Who runs this city?")
                              &aux (state (make-instance 'metal-head-state)))
  ;; Simulate an event: e.g., Metal-Head steps from the neon gloom, player asks question
  (setf (last-event state) (list :scene scene-context :query player-query))
  ;; Mood shift based on event (could be random or mood-propagated)
  (update-metal-head-emotion state 0.01 0.02 -0.01 0.03)
  ;; Generate a verse
  (let ((verse (metal-head-verse player-query (emotion-state state))))
    (push verse (verse-log state))
    (metal-head-debug-print state)
    verse))

;; Utility: Random element from list
(defun random-elt (lst)
  (nth (random (length lst)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [END OF FILE]
;; See: https://github.com/Doctor0Evil/ALN_Programming_Language.git/blob/main/src/personas/metal-head.lisp
[DEBUG METAL-HEAD PERSONA INTERNAL STATE]
EMOTION: chaos= 0.55 wealth= 0.65 loyalty= 0.40 intensity= 0.77 prophecy= 0.28
MOOD TENSOR: #(0.54 0.41 0.74 0.28)
LAST EVENT: (:scene MARKET :query "Who runs this city?")
VERSE LOG (LAST 3):
Yo, static crackle, everything scattered, Who runs this city?, Flexin’ spirals, prophecy bites, Run binary lattices, glitchhead delights!
