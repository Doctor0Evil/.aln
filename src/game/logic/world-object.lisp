;; ======= ALN Game Object Logic for Auto-Typed Object and Safety Enforcement =======
;; File destination: src/game/logic/world-object.lisp

(defpackage :game.world.object
  (:use :cl :aln))

(in-package :game.world.object)

(defvar *world-object* nil "Main game world object registry.")

(defun ensure-world-object ()
  (unless *world-object*
    (setf *world-object* (make-hash-table :test 'equal))
    (log-event "Defined world.object container.")))

(defun set-object-type-auto (object-id)
  "Set the object.type slot to 'auto' for an in-world object."
  (let ((obj (gethash object-id *world-object*)))
    (if obj
        (progn
          (setf (getf obj :type) 'auto)
          (log-event (format nil "Set type:auto for ~A" object-id)))
        (log-event (format nil "Failed set type:auto - object ~A undefined." object-id)))))

(defun sort-objects-by-type ()
  "Sort all objects in *world-object* by :type (auto priority)."
  (let ((objects (loop for k being the hash-keys of *world-object* collect (gethash k *world-object*))))
    (sort objects #'string< :key (lambda (o) (string (getf o :type))))))

(defun detect-auto-object (object-id)
  "Detect if an object is of type auto within the world."
  (let* ((obj (gethash object-id *world-object*))
         (type (and obj (getf obj :type))))
    (eq type 'auto)))

(defun set-safe-logic-object-type (object-id)
  "If detect-auto-object fails, safely enforce or remediate type."
  (unless (detect-auto-object object-id)
    (setf (getf (gethash object-id *world-object*) :type) 'unknown)
    (log-event (format nil "Safe logic set: type:unknown for ~A (not auto)." object-id))))

(defun process-world-objects ()
  (ensure-world-object)
  ;; Walk all objects and enforce logic
  (let ((ids (loop for k being the hash-keys of *world-object* collect k)))
    (dolist (objid ids)
      (unless (detect-auto-object objid)
        (set-safe-logic-object-type objid)))))

(defun game-object-logic-main ()
  (ensure-world-object)
  (if *world-object*
      (progn
        (maphash (lambda (k v) (set-object-type-auto k)) *world-object*)
        (sort-objects-by-type)
        (process-world-objects)
        (log-event "Ran logic: sorted world.obj by type, enforced safe logic."))
      (progn
        (ensure-world-object)
        (setf (getf (gethash :meta *world-object*) :path) "defined.object.game.logic:path$!game-object!")
        (log-event "World object undefined; created registry and set meta path."))))

;; CLI Entrypoint
(defun run-world-object-logic ()
  (game-object-logic-main))
