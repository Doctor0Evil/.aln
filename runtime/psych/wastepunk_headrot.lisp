;;; ================================================================
;;; COMPOUNDED INTERNAL LOGIC – BLEAK HUMOR "HEAD ROT" MODULE
;;; ================================================================
;;; PURPOSE:
;;; - Models grotesque, black-comedy style characters’ internal states
;;; - Provides "rot-core manifest" (mad inner monologue descriptors)
;;; - Infuses survival chance metrics with twisted descriptors
;;; - Mindspace represented as narrative metaphors (skulls, maggots, etc.)
;;; ================================================================
(defpackage :wastepunk-headrot
  (:use :cl)
  (:export :simulate-rotcore))
(in-package :wastepunk-headrot)

(defparameter *rotcore-descriptors*
  '("maggot-infested skullspace dripping with irony"
    "dry-rotted piss thoughts buzzing, sting of tarantula-hawk"
    "shit-stained linens of some cursed domestic vampire"
    "gore-slick tunnel where your humor tries to crawl out screaming"
    "echo chamber of cracked bone and ugly giggles"))

(defun rotcore-survival-chance (base)
  "Darkly modifies survival outcome with existential dread."
  (let ((roll (random 100)))
    (if (> roll base)
        (list :chance "slim"
              :effect "you’ll probably crawl toward that old doc and collapse halfway")
        (list :chance "miraculously hanging on"
              :effect "despite buzzing flies in your headspace"))))

(defun simulate-rotcore (&key (base-survival 25) (humor-twist 0.8))
  "Generate twisted headspace description and survival roll."
  (let* ((descriptor (nth (random (length *rotcore-descriptors*))
                          *rotcore-descriptors*))
         (survival (rotcore-survival-chance base-survival)))
    (format t "~%[HEADROT] Internal Rotcore Engaged...")
    (format t "~%  Rot-Descriptor: ~A" descriptor)
    (format t "~%  Humor-Twist Level: ~,2f" humor-twist)
    (format t "~%  Survival Prognosis => ~A / ~A"
            (getf survival :chance) (getf survival :effect))
    (list :descriptor descriptor
          :humor humor-twist
          :survival survival)))
;;; ================================================================
;;; github-file-destination: runtime/psych/wastepunk_headrot.lisp
;;; ================================================================
