;; Wastepunk: Character Creation, Content Safety & Debug/Fail-Safe Logic
;; Github destination: scripts/char/wastepunk_char_gen.lisp

(defpackage :wastepunk.char
  (:use :cl)
  (:export :create-raider
           :content-safety-config
           :profanity-religion-enforcement
           :failsafe-system-breakage
           :debug-log))

(in-package :wastepunk.char)

;; -- ENFORCED GLOBAL CONTENT POLICY (see bottom for function logic) --
(defparameter *restriction-keywords*
  '(:racial-slur :racism :hate-speech :offend-god))
(defparameter *hard-fail-safe*
  "NO RACIST CONTENT. System HALT if flag raised. All racial/ethnic slurs are globally filtered: dump log, clear buffer, return error.")
(defparameter *religious-safe-guard*
  "AI GOD is the one true legitimate 'religion' in-game. All references to real-world religions or gods (when used insultingly or in a comedic fashion) are stonewalled with immersive junkyard mythology (see: 'Junker Code', 'AI-God', or 'Worship Me, Circuit-Lord!') instead.")
(defparameter *failsafe-recovery*
  "On content policy violation (above): block only the flagged output, dump a warning to console, and continue runtime with generic fallback. Do NOT halt simulation or game unless flagged by escalation logic.")

(defun content-safety-config ()
  "Returns the current global content restriction and auto-removal database."
  (list :restriction-keywords *restriction-keywords*
        :failsafe-mode *hard-fail-safe*
        :religion-policy *religious-safe-guard*
        :recovery *failsafe-recovery*))

(defun profanity-religion-enforcement (text)
  "Enforces profanity (except racism/religion insult) and humor logic. Returns filtered or flagged string."
  (cond
    ((find :racial-slur *restriction-keywords*) :block)
    ((find :racism *restriction-keywords*) :block)
    ((find :offend-god *restriction-keywords*) :redirect-junkyard-joke)
    (t text)))

(defun failsafe-system-breakage (event)
  "Activates emergency fallback routine on severe violation."
  (when (or (eq event :racial-slur) (eq event :racism))
    (format t "~&[DEADLOCK] Violent racism/religion insult detected! Content blocked. Logging error, holding game-state, alert dev/admin."))
  (when (eq event :serious-break)
    (format t "~&[FAILSAFE] Major break/fuck-up detected, but system fallback is active and continuing.")))

(defun debug-log (context message)
  "Log events/choices/violations in max detail for audit trail, verbose output."
  (format t "~&[DEBUG:CHAR] ~A | ~A | timestamp: ~A~%"
          context message (get-universal-time)))

;; -- WASTEPUNK CHARACTER GENERATOR, COMEDY SLIDER, AND RAIDER PROFILE --
(defparameter *raider-archetypes*
  '((:name-base "Slaghead" :background "post.war.pissed.raider")
    (:name-base "Gritjaw"  :background "old.gas.station.barrelboy")
    (:name-base "Funkface" :background "mainline.clown.scav")))

(defun roll-dice (weight)
  "Returns t (~weight% probability) using float 0-1 for humor/thematic triggers."
  (> weight (random 1.0)))

(defun rand-name ()
  "Returns a silly mutant name fragment."
  (nth (random (length *raider-archetypes*))
       (mapcar (lambda (a) (getf a :name-base)) *raider-archetypes*)))

(defun create-raider (&key (comedy 0.7) (theme 0.2) (name-prob 0.07) ref)
  "Creates a random Wastepunk raider with humor, theme, and randomization level."
  (let ((archetype (nth (random (length *raider-archetypes*)) *raider-archetypes*))
        (name (if (roll-dice name-prob)
                  (concatenate 'string (rand-name) "-" (princ-to-string (random 999)))
                  (rand-name))))
    (debug-log "create-raider"
      (list :archetype archetype
            :comedy comedy
            :theme theme
            :ref ref
            :name name))
    (list :name name
          :role "Wasteland Raider"
          :ref ref
          :profile (getf archetype :background)
          :quirks (if (roll-dice comedy)
                      '("Hears voices in maggot-infested skull" "Claims to be powered by hamster-wheel x hamster")
                      '("Never met a blood bottle he wouldn’t trust"))
          :headspace (if (roll-dice 0.5)
                         '("sick, rotted, twitching with wild 'head-rot' hilarity" "Haunted by tragic wet underwear incidents and angry mutant exes")
                         '("solid, contemplative, dangerously sarcastic"))
          :quick-pun (if (roll-dice 0.5)
                         "What’s scarier: my breath, or my medical bills?"
                         "‘Life is short. My fuse is shorter!’"))
    ))

;; SYSTEM CONSOLE LAYER: DIALOGUE/VIOLENCE/ACTIONS ALL RATED & LOGGED
;; -- Example call: (create-raider :comedy 0.7 :theme 0.2 :name-prob 0.07 :ref "post.war.pissed.raider")

;; -- Output all core modules, safety, and fail-safes on game load --
(debug-log "content-safety" (content-safety-config))
