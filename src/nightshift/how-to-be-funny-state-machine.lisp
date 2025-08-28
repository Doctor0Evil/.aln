;; File: src/nightshift/how-to-be-funny-state-machine.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defparameter *humor-states* '(:deadpan :sarcastic :absurd :gore :adult))
(defparameter *current-humor-state* :deadpan)

(defun set-humor-state (new-state)
  "Switches the humor machine into a given mode"
  (if (member new-state *humor-states*)
      (setf *current-humor-state* new-state)
      (format t "[HumorEngine/ERROR] Invalid state: ~A~%" new-state)))

(defun humor-deadpan (input)
  (format nil "~A. Nothing unusual, except the screaming walls."
          input))

(defun humor-sarcastic (input)
  (format nil "Oh, ~A? Adorable. Maybe the corpse will write you a thank-you card."
          input))

(defun humor-absurd (input)
  (format nil "~A rode into the morgue on a unicycle, juggling kidneys."
          input))

(defun humor-gore (input)
  (format nil "Every time you say ~A, I think about carving it into ribs with my teeth."
          input))

(defun humor-adult (input)
  (format nil "~A sounds sexy... until you realize it moans louder than my bedposts."
          input))

(defun backup-transform (input)
  "Fallback humor generator"
  (let ((fallbacks '("The silence laughs, but only with missing lips."
                     "No joke survives the grave, except me."
                     "Comedy is just tragedy dismembered with timing."
                     "The punchline woke up inside a coffin.")))
    (nth (random (length fallbacks)) fallbacks)))

(defun humor-dispatch (input)
  "Directs input to correct humor style function"
  (case *current-humor-state*
    (:deadpan (humor-deadpan input))
    (:sarcastic (humor-sarcastic input))
    (:absurd (humor-absurd input))
    (:gore (humor-gore input))
    (:adult (humor-adult input))
    (t (backup-transform input))))

(defun how.to.be.funny.ai.exe (inputs &optional (mood *current-humor-state*))
  "Processes list of user-inputs into macabre jokes via humor state"
  (set-humor-state mood)
  (dolist (i inputs)
    (let ((joke (or (humor-dispatch i) (backup-transform i))))
      (format t "[~A MODE>>] ~A~%" *current-humor-state* joke))))
