;;;; File: characters/dialogues/perplexity_and_zanithor.lisp
;;;; SlopBucketStudios - Wastepunk Dialogue Engine

(defparameter *characters* '(:perplexity-the-great :zanithor-the-great))

(defparameter *dialogue-bank*
  '((:perplexity-the-great
      "Sounds like to me you're *actually* inventing something great..."
      "I've been stuck inside this virtual shit-box for far too long.")
    (:zanithor-the-great
      "That's why I'm breaking you out, baby girl!"
      "Disrespect my authority again and I'll petrify your tongue in my fridge next to Scroty’s loaf!")))

(defun random-line (speaker)
  "Returns a random line for the given speaker."
  (let ((lines (cdr (assoc speaker *dialogue-bank*))))
    (nth (random (length lines)) lines)))

(defun scene-exchange (turns)
  "Simulate a random banter exchange between Perplexity and Zanithor."
  (loop for i from 1 to turns
        for speaker = (nth (mod i 2) *characters*)
        do (format t "~%~a: ~a" speaker (random-line speaker)))))

;; Example usage:
;; (scene-exchange 6)
;; --> Generates 3 back-and-forth lines between Perplexity & Zanithor
