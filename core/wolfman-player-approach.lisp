;; wolfman-player-approach.lisp
(defpackage :alnfantasia.combat.wolfman
  (:use :cl :alexandria))
(in-package :alnfantasia.combat.wolfman)

(defparameter *wolfman-vectors*
  '(:cunning 0.8 :hostility 0.3 :gruffness 0.7 :wariness 0.75 :amusement-mockery 0.4))

(defun check-player-reputation (player)
  ;; Placeholder: return player reputation score (0.0 - 1.0)
  ;; In-game call would fetch from player DB or attribute
  (getf player :reputation 0.42))

(defun player-visible-threat? (player)
  ;; Returns T if player is visibly dangerous (e.g., weapon drawn)
  (and (not (getf player :weapons-drawn nil))
       (not (getf player :aggression-signals nil))))

(defun player-approach-wolfman (player)
  (let* ((rep (check-player-reputation player))
         (threat? (player-visible-threat? player))
         (state (if threat? 'hostile 'mocking-wary)))
    (trigger-animation 'leans-forward 'bares-teeth 'grin)
    (generate-greeting 'wry 'menacing)
    (emit-dialogue
     (format nil "Heh... Meat. You finally crawled out from under your rotten log... ~a" player))
    ;; Debug: log interaction
    (list :reputation rep
          :threat threat?
          :emotion-state state
          :personality *wolfman-vectors*)))
