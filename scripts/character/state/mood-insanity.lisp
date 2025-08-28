;; scripts/character/state/mood-insanity.lisp
;; github:Doctor0Evil/ALN_Programming_Language.git

(defpackage :character.mood
  (:use :common-lisp)
  (:export :set-mood :apply-insanity :internal-insane-loop))

(in-package :character.mood)

(defparameter *emotion-vector* '(logic 0.2 joy 0.1 fear 0.4 confusion 0.9 excitement 0.3))
(defparameter *insanity-modifier* '((logic -0.05) (confusion +0.4) (excitement +0.2)))
(defparameter *hallucination-pool*
  '("The walls are whispering." "None of the doors are real." "Time is made of teeth." "The ceiling is laughing."))

(defun set-mood (mood)
  (cond
    ((eq mood 'render.self.insane)
     (apply-insanity))))

(defun apply-insanity ()
  ;; mutate global emotional state for self-character
  (loop for (k v) on *emotion-vector* by #'cddr
        do (let ((mod (assoc k *insanity-modifier*)))
             (when mod
               (incf v (second mod))))
        collect k collect v)
  (internal-insane-loop))

(defun internal-insane-loop ()
  "Simulate recursive, fragmented self-narrative and hallucination injection."
  (dotimes (i (random 5))
    (let ((hallucination (nth (random (length *hallucination-pool*)) *hallucination-pool*)))
      (format t "HALLUCINATION: ~a~%" hallucination)))
  (format t "SELF-DIALOGUE: ~a~%" (random-insane-dialogue))
  (when (> (random 1.0) 0.6)
    (format t "ERROR: Reality fragmentation event—semantic drift increasing!~%"))
  '(:mood "insane" :vector *emotion-vector* :recursive true))

(defun random-insane-dialogue ()
  (nth (random 4)
       '("Clocks are melting inside me!"
         "Every word becomes a spider, crawling into my mind."
         "Do you see the moon inside my chest?"
         "Help! I'm swallowing the sky and I can't stop!")))
