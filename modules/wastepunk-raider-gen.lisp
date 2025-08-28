;; Wastepunk Raider Simulation
;; SlopBucketStudios Humor Framework
;; (c) 2025 SlopBucketStudios — do not steal my maggot-beer

(defpackage :wastepunk.raider
  (:use :cl))
(in-package :wastepunk.raider)

;; --- SAFETY NETS ---
(defparameter *racial-safety* t)          ;; no racism allowed
(defparameter *religious-safety* t)       ;; no insulting real gods
(defparameter *ai-god* "The Sacred Hamster Wheel") ;; fallback religion

;; Toggle surreal vs realism world cycles
(defparameter *surreal-mode* nil)
(defparameter *combat-mode* nil)

(defun roll-dice (sides)
  (+ 1 (random sides)))

(defun create-raider ()
  (let* ((name (nth (roll-dice 3)
                    '("Skunk Bazoo" "Cartboy Rotgut" "Fungus-Pocket Jill")))
         (weapon (nth (roll-dice 2)
                      '("barbed pogo stick" "molotov in a shampoo bottle")))
         (armor (nth (roll-dice 2)
                     '("rusted shopping cart" "license-plate underpants"))))
    (list :name name
          :weapon weapon
          :armor armor
          :religion (if *religious-safety*
                        *ai-god* "toast-machine"))))

(defun toggle-surreal (flag)
  (setf *surreal-mode* flag))

(defun toggle-combat (flag)
  (setf *combat-mode* flag))

(defun raider-intro-dialogue (raider)
  (format t "~%The raider ~a screams: 'The ~a commands it! Fear my ~a!'"
          (getf raider :name)
          (getf raider :religion)
          (getf raider :weapon)))

;; Example execution
(let ((raider (create-raider)))
  (toggle-surreal t)
  (raider-intro-dialogue raider))
