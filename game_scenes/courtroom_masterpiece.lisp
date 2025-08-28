;;; ======================
;;; COMPOUNDED INTERNAL LISP LOGIC (Courtroom Masterpiece)
;;; ======================
(defparameter *scene* 'courtroom.masterpiece)
(setf *temperature* 0.57)
(setf *world-mood* 'litigious)
(setf *madness-level* (random 100))
(setf *active-disasters* '(slopbucket lawsuit mailman uprising audit panic))
(setf *contradiction-log* nil)

;; Entities (Judge, Jury, Lawyer)
(defparameter *judge* (list :coords nil :quirks nil))
(defparameter *jury* (list :coords nil :quirks nil))
(defparameter *lawyer* (list :coords nil :quirks nil))

(defun random-behavior-augment ()
  "Random quirks for courtroom roles."
  (nth (random 4) '(:pedantic :erratic :sleepy :overdramatic)))

(defun assign-courtroom-positions ()
  ;; Judge setup
  (setf (getf *judge* :coords) '(0.57 0.81 0.03 0.29))
  (setf (getf *judge* :quirks) (random-behavior-augment))
  ;; Jury setup
  (setf (getf *jury* :coords) '(0.82 0.78 0.11 0.22))
  (setf (getf *jury* :quirks) (random-behavior-augment))
  ;; Lawyer setup
  (setf (getf *lawyer* :coords) '(0.27 0.62 0.03 0.17))
  (setf (getf *lawyer* :quirks) (random-behavior-augment)))

(defun inject-narrative (str)
  (format t "~&[NARRATIVE] ~A~%" str))

(defun echo (str)
  (format t "~&[ECHO] ~A~%" str))

(defun spawn-side-event (str)
  (format t "~&[SIDE-EVENT] ~A~%" str))

(defun courtroom-event-trigger ()
  (let ((roll (random 3)))
    (case roll
      (0 (inject-narrative "A verdict rains from the ceiling—mailmen forget the case—wolfman howls at a sandwich."))
      (1 (when (> *madness-level* 60)
           (echo "Everyone prescribes escape as treatment—mailbox delivers itself twice.")))
      (2 (spawn-side-event "Mailbox confusion contest winner is crayon doctor."))))
  (print-all-traces))

(defun print-all-traces ()
  (format t "~&[DEBUG] Judge State: dignity=~A, surprise=~A, mood=~A~%"
          (second (getf *judge* :coords))
          (fourth (getf *judge* :coords))
          *world-mood*)
  (format t "[DEBUG] Jury State: skepticism=~A, confusion=~A~%"
          (second (getf *jury* :coords))
          (fourth (getf *jury* :coords)))
  (format t "[DEBUG] Lawyer State: pride=~A, anxiety=~A~%"
          (second (getf *lawyer* :coords))
          (fourth (getf *lawyer* :coords)))
  (format t "[DEBUG] World mood: ~A, madness: ~A~%" *world-mood* *madness-level*)
  (format t "[DEBUG] RNG Event Roll: ~A~%" (random 3))
  (format t "[DEBUG] Contradiction log: ~A~%" *contradiction-log*))

(defun scene.play ()
  (assign-courtroom-positions)
  (courtroom-event-trigger)
  (print "COURTROOM.MASTERPIECE: All rise! Scene logic initiated.")
  (print "Active disaster: SLOPBUCKET LAWSUIT. Defamation by slop alleged.")
  (print "JUDGE: All rise!")
  (print "LAWYER: This is no ordinary bucket, your honor...")
  (print "JURY: Verdict... is soggy."))

;; Execute
(scene.play)
