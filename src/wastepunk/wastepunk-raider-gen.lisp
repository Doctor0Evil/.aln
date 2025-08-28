;; ================================================================
;; INTERNAL COMPOUNDING LISP LOGIC SCRIPT
;; ================================================================
;; This script binds together all active modules, debug logs,
;; NPC/raider generation, henchmen simulations, and world-state cycling
;; into a coherent runtime simulation loop. It provides a unified entry
;; point for random raider creation + "slopbucket" empire-dog chaos cycles.
;; ================================================================

(defpackage :wastepunk-core
  (:use :cl :wastepunk-raider-gen :slopbucket-sim)
  (:export :main-simulation-tick))

(in-package :wastepunk-core)

(defun main-simulation-tick ()
  "Unified simulation step combining raider gen + slopbucket cycle."
  ;; STEP 1: Roll a raider
  (multiple-value-bind (rname roll quirk desc)
      (wastepunk-raider-gen:roll-dice+create-raider)
    (format t "~%[SIM] RaiderGen => Name:~A Roll:~A Quirk:~A Desc:~A~%"
            rname roll quirk desc)

    ;; STEP 2: Slopbucket cycle (dog-state, NPC chaos events, world mood)
    (let ((cycle-log (slopbucket-sim:run-slopbucket-cycle)))
      (format t "~%[SIM] Slopbucket Log Cycle Dump~%------------------------~%")
      (dolist (entry cycle-log)
        (format t "~A~%" entry)))
    ;; Return as structured data
    (values rname roll quirk desc *dog-state* *dog-trust* *dog-aggression* *dog-loyalty* *world-mood* cycle-log)))
