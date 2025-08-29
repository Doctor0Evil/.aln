(defmacro make-humorous-script (name depth emotion)
  `(progn
     (defun ,name ()
       (format t "~%;; This script was created by a workflow with the emotion: ~a! ~%" ,emotion)
       (format t "~%;; (I love|hate|friend|bitch|fuck) being created.~%")
       (if (> ,depth 0)
           (let ((next-name (intern (concatenate 'string (symbol-name ',name) "-NEXT"))))
             (format t ";; Creating the next workflow & script... ~%")
             (make-humorous-script next-name (- ,depth 1) (aref #("love" "hate" "friend" "bitch" "fuck") (mod ,depth 5))))
           (format t ";; [Base Script]: The recursion is complete. ~%")))))
;; Example Usage:
(make-humorous-script 'workflow-script 5 "love")
(workflow-script)
