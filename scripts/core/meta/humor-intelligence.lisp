;; scripts/core/meta/humor-intelligence.lisp
;; github:Doctor0Evil/ALN_Programming_Language.git

(defpackage :meta.humor-intelligence
  (:use :cl)
  (:export :evaluate-intelligence :humor-award :demo-humor-vector))

(in-package :meta.humor-intelligence)

(defclass intelligence ()
  ((strength     :initarg :strength     :accessor strength     :initform 0.5)
   (rationality  :initarg :rationality  :accessor rationality  :initform 0.5)
   (obliqueness  :initarg :obliqueness  :accessor obliqueness  :initform 0.5)
   (humor-rating :initarg :humor-rating :accessor humor-rating :initform 0.0)
   (tag          :initarg :tag          :accessor tag          :initform 'standard)))

(defun evaluate-intelligence (obj)
  "Rates an intelligence object, favoring absurdly high strength and low rationality (retarded.strength) for higher humor-rating."
  (let* ((s (strength obj))
         (r (rationality obj))
         (o (obliqueness obj))
         (humor (/ (+ (* s 2) o) (max 0.3 r))))
    (setf (humor-rating obj) (min 1.0 (max 0.0 (* humor 0.4))))
    (cond
      ((and (> s 0.9) (< r 0.4) (> o 0.6))
       (setf (tag obj) 'intelligence.of.retarded.strength)))
    obj))

(defun humor-award (obj)
  "Returns a comedic string if the humor-rating is sufficiently high, in-character with meta self-awareness."
  (if (> (humor-rating obj) 0.69)
      (format nil
        "~&[META] Awarded: 'Order of Sacred Overkill' to instance tagged as ~A (humor-rating: ~2f).~%The pure, chaotic joy of ~A cannot be underestimated. Breaking the wall (sometimes literally)."
        (tag obj) (humor-rating obj) (tag obj))
      (format nil "~&[HUMOR] Rating insufficient for transcendental absurdity. Try harder to be less reasonable.")))

(defun demo-humor-vector ()
  "Demonstration: create two intelligence objects — one normal, one with retarded.strength — and show full logic path."
  (let ((norm (make-instance 'intelligence :strength 0.5 :rationality 0.7 :obliqueness 0.4))
        (brute (make-instance 'intelligence :strength 1.0 :rationality 0.2 :obliqueness 0.8)))
    (dolist (intellect (list norm brute))
      (let ((evald (evaluate-intelligence intellect)))
        (format t "~&[DEBUG] Tag: ~A~%  Strength: ~2f, Rationality: ~2f, Obliqueness: ~2f, Humor: ~2f~%"
                (tag evald) (strength evald) (rationality evald) (obliqueness evald) (humor-rating evald))
        (format t "~A~%" (humor-award evald))))))

;; Github-remote: scripts/core/meta/humor-intelligence.lisp
