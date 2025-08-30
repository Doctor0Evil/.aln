(defpackage :aln-devops-agent
  (:use :cl :aln))
(in-package :aln-devops-agent)

(defun deploy-workflow-playbook ()
  (load-playbook "ALN_Github_Playbook_Chatbots.txt")
  (while (pending-tasks)
    (let ((task (next-task)))
      (execute-task task))))

(defun execute-task (task)
  (cond
    ((equal (task-type task) :diagnostics)
     (run-diagnostics task))
    ((equal (task-type task) :workflow-trigger)
     (trigger-workflow task))
    (t (error "Unknown task in ALN playbook"))))
