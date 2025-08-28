;;; file: character_cycle_demo.lisp
(in-package :wastepunk.character)

(defun demo-sim-cycle ()
  "Demonstrate one scene of character banter with debug events."
  (let ((raider-a (create-character))
        (raider-b (create-character)))
    (format t "~&[SIM] Entering ~A vs ~A~%"
            (gethash "name" raider-a)
            (gethash "name" raider-b))
    (format t "> ~A: ~A~%"
            (gethash "name" raider-a)
            (gethash "internal" raider-a))
    (format t "> ~A: ~A~%"
            (gethash "name" raider-b)
            (safe-output "You dripping oil, or just desperation, buddy?"))))
