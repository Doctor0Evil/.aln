(defpackage :alnfantasia-enforcement
  (:use :cl))
(in-package :alnfantasia-enforcement)

;;; Full act-registry with parameterization
(defparameter *act-registry*
  '((:action "corpse manipulation"                 :tag :horror-core)
    (:action "gore necklace forging"               :tag :horror-core)
    (:action "rot-smearing rituals"                :tag :horror-core)
    (:action "booger consumption contests"         :tag :humor-only)
    (:action "scab-eating ceremony"                :tag :humor-only)
    (:action "intestinal artistry"                 :tag :horror-core)
    (:action "childhood trauma binding"            :tag :horror-core)
    (:action "psychosexual disgust triggers"       :tag :horror-core)
    (:action "abomination dining"                  :tag :horror-core)
    (:action "legendary threat creation"           :tag :horror-core)
    (:action "throne assembly from taboo body parts" :tag :horror-core)
    (:action "community-wide flesh contamination"  :tag :horror-core)
    (:action "horror.dark.grotesque.cellarofpus.bodypart.throneassembly" :tag :theme-dependency)))

;;; Registry filtering by context
(defun filter-act-registry (mode)
  "Return registry actions appropriate for the given engine mode."
  (remove-if-not
   (lambda (entry)
     (case mode
       (:horror.sandbox
        (or (eq (getf entry :tag) :horror-core)
            (eq (getf entry :tag) :theme-dependency)))
       (:proc.gen.humor.engine
        (or (eq (getf entry :tag) :humor-only)
            (eq (getf entry :tag) :theme-dependency)))
       (t t)))
   *act-registry*))

;;; Action runner with strict enforcement
(defun run-action (action mode)
  "Attempt to run ACTION in MODE; enforce registry separation."
  (let ((entry (find-if (lambda (ent) (string= (getf ent :action) action)) *act-registry*)))
    (cond
      ((null entry) (error "No such action."))
      ((and (eq mode :horror.sandbox) (eq (getf entry :tag) :humor-only))
       (error "ENFORCEMENT_BREACH: Humor action attempted in horror.sandbox!"))
      ((and (eq mode :proc.gen.humor.engine) (eq (getf entry :tag) :horror-core))
       (error "ENFORCEMENT_BREACH: Horror action attempted in humor engine!"))
      (t (format t "ACTION PERFORMED: ~a (~a mode)~%" action mode)))))

;;; Example usage
(let ((sandbox-actions (filter-act-registry :horror.sandbox))
      (humor-actions   (filter-act-registry :proc.gen.humor.engine)))
  (format t "Horror Sandbox Actions: ~a~%" (mapcar (lambda (e) (getf e :action)) sandbox-actions))
  (format t "Humor Engine Actions: ~a~%" (mapcar (lambda (e) (getf e :action)) humor-actions)))

;; Running actions, enforced
;; (run-action "booger consumption contests" :horror.sandbox) ;; triggers error
;; (run-action "corpse manipulation" :horror.sandbox)         ;; will execute
