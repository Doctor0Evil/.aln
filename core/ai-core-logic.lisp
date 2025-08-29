;; ai-core-logic.lisp
;; ========================
;; FULL AI.CORE.LOGIC.REASONING.MODEL
;; Example set applied to Grindhouse NPCs
;; Built to demonstrate advanced reasoning, personality diversity,
;; and debug pipelines in ALN-style Lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL SYSTEM SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *advanced.reasoning* t)     ;; toggle advanced chain reasoning
(defparameter *character.diversity* t)    ;; force unique persona calculations
(defparameter *debug.logging* t)          ;; toggle live debug logs
(defparameter *external.configs* '("COMEDY.fuck.cfg" "personality-matrix.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSONALITY VECTOR STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct personality
  logic       ;; rational weighting
  emotion     ;; emotional intensity
  instinct    ;; base drives
  override)   ;; chaotic impulse toggle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE REASONING ENGINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-response (persona stimulus)
  "AI.core.reasoning.model. Converts stimulus into behavior output."
  (let* ((l (personality-logic persona))
         (e (personality-emotion persona))
         (i (personality-instinct persona))
         (o (personality-override persona)))
    ;; LOGGING
    (when *debug.logging*
      (format t "~%[DEBUG] Persona: L=~a E=~a I=~a O=~a" l e i o)
      (format t "~%[DEBUG] Stimulus: ~a" stimulus))
    ;; RESPONSE ROUTING
    (cond
      ;; OVERRIDE logic (chaotic flute)
      ((= o 1)
       (format nil "Override chaos event: ~a"
               (generate-chaotic-response persona stimulus)))
      ;; LOGIC-dominant route
      ((>= l (max e i))
       (format nil "Calculated rational response: ~a"
               (rational-decision persona stimulus)))
      ;; INSTINCT-dominant route
      ((>= i (max l e))
       (format nil "Instinctive reaction: ~a"
               (instinct-action persona stimulus)))
      ;; EMOTIONAL-dominant route
      ((>= e (max l i))
       (format nil "Emotive performative outburst: ~a"
               (emotional-flare persona stimulus)))
      (t (format nil "Neutral default response.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RESPONSE MODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rational-decision (persona stimulus)
  "Process stimulus through high-logic interpretation."
  (format nil "Strategizing outcome of ~a with cold logic." stimulus))

(defun instinct-action (persona stimulus)
  "High instinct triggers animalistic behaviors."
  (format nil "Primal reflex behavior triggered by ~a." stimulus))

(defun emotional-flare (persona stimulus)
  "Emotive overload behaviors."
  (format nil "Dramatic surge of feeling around ~a." stimulus))

(defun generate-chaotic-response (persona stimulus)
  "Override = chaos; produce stylized cinematic spectacle."
  (format nil "CINEMATIC OVERRIDE! Twist stimulus (~a) into spectacle." stimulus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHARACTER DEFINITIONS USING MATRIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *npc-actors*
  (list
   (cons 'roach-woman
         (make-personality :logic 5 :emotion 2 :instinct 5 :override 0))
   (cons 'elle-driver
         (make-personality :logic 5 :emotion 2 :instinct 5 :override 1))
   (cons 'jason-voorhees
         (make-personality :logic 5 :emotion 2 :instinct 5 :override 0))
   (cons 'freddy-krueger
         (make-personality :logic 5 :emotion 2 :instinct 5 :override 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCENE SIMULATION LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-scene (stimuli)
  "Apply stimuli through each NPC using reasoning engine."
  (dolist (actor *npc-actors*)
    (let* ((name (car actor))
           (persona (cdr actor)))
      (format t "~%--- [ACTOR: ~a] ---" name)
      (dolist (s stimuli)
        (let ((response (calculate-response persona s)))
          (format t "~%Response: ~a" response))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST STIMULI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *stimuli*
  '("roach swarm noise"
    "chair gets kicked"
    "toilet bubbles with mask"
    "gasoline spray and lighter"))

;; RUN SIMULATION
(run-scene *stimuli*)
