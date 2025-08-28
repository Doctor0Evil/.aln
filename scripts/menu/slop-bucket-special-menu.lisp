;; ALNFantasia: Slop.bucket.special-menu Loader & Debug Trace
;; Github destination: scripts/menu/slop-bucket-special-menu.lisp

(defpackage :alnfantasia.menu
  (:use :cl)
  (:export :load-slop-bucket-special-menu :get-current-menu :apply-menu-choice :debug-log))

(in-package :alnfantasia.menu)

(defvar *special-menu*
  '((:name "Slop Bucket Special Menu")
    (:entries
      ((:id 1 :name "Meat Porridge"       :effect "Restore 12 HP, 10% food poisoning.")
       (:id 2 :name "Mystery Stew"        :effect "Restore 8 HP, ? random minor status effect.")
       (:id 3 :name "Soggy Bread"         :effect "Restore 2 HP, minor morale penalty.")
       (:id 4 :name "Chef's 'Surprise'"   :effect "Restore 1 HP, +2 AP next turn, 25% confusion.")
       (:id 5 :name "Dirty Pickles"       :effect "+3 poison resist (5 turns), -1 Charisma."))))
    (:meta
      ((:availability :daytime)
       (:chef :npc "Ratbone")
       (:location "Slop Bucket")
       (:menu-type :special :rotates t)))
    (:console-state
      ((:last-loaded (get-universal-time))
       (:debug-flags nil)
       (:current-selection nil))))

(defun load-slop-bucket-special-menu ()
  "Loads the Slop Bucket special menu, sets console-state, logs event, returns entries."
  (setf (getf *special-menu* :console-state)
        (list :last-loaded (get-universal-time)
              :debug-flags (list :menu-load :success)
              :current-selection nil))
  (debug-log "Loaded Slop Bucket Special Menu." *special-menu*)
  (getf *special-menu* :entries))

(defun get-current-menu ()
  "Return current special menu entries."
  (getf *special-menu* :entries))

(defun apply-menu-choice (item-id)
  "Simulate ordering a menu item, update selection and debug flags."
  (let* ((entries (getf *special-menu* :entries))
         (choice (find item-id entries :key (lambda (e) (getf e :id))))
         (result (if choice
                     (progn
                       (push :item-ordered (getf (getf *special-menu* :console-state) :debug-flags))
                       (setf (getf (getf *special-menu* :console-state) :current-selection) choice)
                       (debug-log (format nil "Menu item ordered: ~A" (getf choice :name)) choice)
                       (getf choice :effect))
                   (progn
                     (push :item-not-found (getf (getf *special-menu* :console-state) :debug-flags))
                     (debug-log "Menu choice not found" item-id)
                     "Invalid menu choice."))))
    result))

(defun debug-log (msg &optional detail)
  "Simulates debug output for menu actions."
  (format t "~&[DEBUG:SLBKT] ~A~@[ | Details: ~A~]~%" msg detail)
  (push (list :timestamp (get-universal-time) :msg msg :detail detail)
        (getf *special-menu* :console-state)))
