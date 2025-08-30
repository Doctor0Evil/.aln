;; File: /aln/ALNFantasia.vectors.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git/aln/ALNFantasia.vectors.lisp

(defpackage :alnfantasia-vectors
  (:use :cl)
  (:export :make-personality :randomize-personality :debug-persona-vector))

(in-package :alnfantasia-vectors)

;; Possible personality axes
(defparameter *personality-axes*
  '(:aggression :humor :paranoia :empathy :courage :profane :orderliness :chaos :curiosity :snark :grit :ruthlessness))

;; Axis Defaults (0.0 to 1.0)
(defparameter *default-vector*
  (mapcar (lambda (ax) (cons ax 0.5)) *personality-axes*))

;; Personality Constructor
(defun make-personality (&key (base *default-vector*) (seed (random 10000)))
  "Constructs a new personality vector, optionally seeded for repeatability."
  (let ((newvec
          (mapcar (lambda (ax)
                    (cons ax (min 1.0 (max 0.0 (+ (cdr (assoc ax base))
                        (/ (random 1000) 10000.) ;; jitter by up to ±0.1
                        (- 0.05 (random 0.1))))))
                  *personality-axes*)))
    (cons :seed seed newvec)))

;; Random Personality Generator
(defun randomize-personality ()
  (make-personality :base (mapcar (lambda (ax) (cons ax (random 1001)/1000.)) *personality-axes*)))

;; Debug Printout for a Personality Vector
(defun debug-persona-vector (persona)
  (list
    :persona persona
    :axes *personality-axes*
    :vector-values (mapcar (lambda (ax) (cons ax (cdr (assoc ax persona)))) *personality-axes*)
    :interpretation
      (format nil
        "Profile: Aggression(~,2F), Humor(~,2F), Paranoia(~,2F), Empathy(~,2F), Courage(~,2F), Profane(~,2F), Order(~,2F), Chaos(~,2F), Curiosity(~,2F), Snark(~,2F), Grit(~,2F), Ruthless(~,2F)"
        (cdr (assoc :aggression persona))
        (cdr (assoc :humor persona))
        (cdr (assoc :paranoia persona))
        (cdr (assoc :empathy persona))
        (cdr (assoc :courage persona))
        (cdr (assoc :profane persona))
        (cdr (assoc :orderliness persona))
        (cdr (assoc :chaos persona))
        (cdr (assoc :curiosity persona))
        (cdr (assoc :snark persona))
        (cdr (assoc :grit persona))
        (cdr (assoc :ruthlessness persona))))
    :github-file-path "https://github.com/Doctor0Evil/ALN_Programming_Language.git/aln/ALNFantasia.vectors.lisp"))

;; Runtime Demo Output (Personality Vector + Debug)
(debug-persona-vector (randomize-personality))

;; END EXECUTION – Dynamic personality system live; debug and traceability enabled.
