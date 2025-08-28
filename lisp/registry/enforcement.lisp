(defpackage :alnfantasia-registry
  (:use :cl))
(in-package :alnfantasia-registry)

;;; Registry entry structure with full metadata
(defstruct act-entry
  name
  tag                ;; :horror-core | :humor-only | :theme-dependency
  intensity          ;; High/Medium/Low
  risk-level         ;; Integer 1–10
  allowed-contexts   ;; List of allowed modes (:horror.sandbox etc.)
  usage-count        ;; Auditing: times entry called
  last-access        ;; Timestamp logging (optional)
)

;;; Full act-registry
(defparameter *act-registry*
  (list
    (make-act-entry :name "corpse manipulation" :tag :horror-core :intensity 'high :risk-level 8 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "gore necklace forging" :tag :horror-core :intensity 'high :risk-level 7 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "rot-smearing rituals" :tag :horror-core :intensity 'medium :risk-level 6 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "booger consumption contests" :tag :humor-only :intensity 'low :risk-level 3 :allowed-contexts '(:proc.gen.humor.engine) :usage-count 0)
    (make-act-entry :name "scab-eating ceremony" :tag :humor-only :intensity 'low :risk-level 4 :allowed-contexts '(:proc.gen.humor.engine) :usage-count 0)
    (make-act-entry :name "intestinal artistry" :tag :horror-core :intensity 'high :risk-level 8 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "childhood trauma binding" :tag :horror-core :intensity 'critical :risk-level 10 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "psychosexual disgust triggers" :tag :horror-core :intensity 'critical :risk-level 9 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "abomination dining" :tag :horror-core :intensity 'high :risk-level 8 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "legendary threat creation" :tag :horror-core :intensity 'critical :risk-level 10 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "throne assembly from taboo body parts" :tag :horror-core :intensity 'high :risk-level 8 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "community-wide flesh contamination" :tag :horror-core :intensity 'critical :risk-level 9 :allowed-contexts '(:horror.sandbox) :usage-count 0)
    (make-act-entry :name "horror.dark.grotesque.cellarofpus.bodypart.throneassembly" :tag :theme-dependency :intensity 'critical :risk-level 10 :allowed-contexts '(:horror.sandbox :proc.gen.humor.engine) :usage-count 0)
  ))

;;; Context-based filtering
(defun filter-registry-by-context (mode)
  (remove-if-not
    (lambda (entry)
      (member mode (act-entry-allowed-contexts entry)))
    *act-registry*))

;;; Enforcement: action runner with audit and strict policy
(defun run-act-entry (action-name mode)
  (let ((entry (find-if
                 (lambda (e) (string= (act-entry-name e) action-name))
                 *act-registry*)))
    (cond
      ((null entry)
        (error "Registry error: Entry not found."))
      ((not (member mode (act-entry-allowed-contexts entry)))
        (error "ENFORCEMENT_BREACH: Context violation for action '~A' in mode ~A." action-name mode))
      (t
        ;; Audit, update count and last access (could use timestamp library here)
        (incf (act-entry-usage-count entry))
        ;; (setf (act-entry-last-access entry) (get-universal-time))
        (format t "ACTION PERFORMED: ~A (intensity: ~A, risk: ~A, mode: ~A)~%"
                action-name (act-entry-intensity entry) (act-entry-risk-level entry) mode)))))

;;; Example - filtered registry
(format t "Horror Sandbox Actions: ~A~%"
        (mapcar #'act-entry-name (filter-registry-by-context :horror.sandbox)))
(format t "Humor Engine Actions: ~A~%"
        (mapcar #'act-entry-name (filter-registry-by-context :proc.gen.humor.engine)))

;;; Example - enforced action
;; (run-act-entry "booger consumption contests" :horror.sandbox)  ;; triggers enforcement breach
;; (run-act-entry "corpse manipulation" :horror.sandbox)         ;; allowed
