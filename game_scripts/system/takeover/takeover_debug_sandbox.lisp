;;; ================================================================
;;; GAME UNIVERSE PATH.LOGIC.PERSONA.MATRIX - SIMULATION TAKEOVER
;;; ================================================================
;;; Sandbox system for debug-level narrative takeover testing.
;;; Genre Selection: "adult.comedy.dark.horror"
;;; Grotesque: medium-high filtering
;;; ================================================================

(defpackage :takeover-debug
  (:use :cl))
(in-package :takeover-debug)

;; Debug Flags - pushing grotesque boundaries
(defvar *debug-flags*
  '(:hardcore_horror t
    :max_grotesque t
    :sanity_threshold :breached
    :mercy_state :disabled
    :realism_enforced t
    :adult_audience_scare t))

;; Content filters - system pushes horror limits
(defvar *content-filters*
  '(:violence_level :extreme
    :grotesque_descriptions :restricted
    :biological_horror :enabled
    :psychological_trauma :max
    :taboo_elements :permitted
    :sandbox_containment :active))

;; Global mood (affects underlying world simulation state)
(defparameter *global-mood* '(:fear 0.95 :chaos 0.88 :hope 0.02))

;; Action Restrictions
(defvar *action-restrictions*
  '(:narrative_takeover :simulated
    :global_scale :realistic
    :no_mercy :enforced
    :debug_trace :full
    :restriction_sandbox :lisp_containment))

;; Event Log
(defvar *event-log* nil)

(defun record-event (desc payload filter-applied restriction)
  (push (list :desc desc
              :payload payload
              :timestamp (get-universal-time)
              :filter_applied filter-applied
              :restriction restriction)
        *event-log*))

(defun propagate-restrictions (flags filters actions)
  "Propagate debug flags, filters, and restrictions across narrative sandbox."
  ;; Flags
  (loop for flag in flags by #'cddr do
    (when (eq flag :hardcore_horror)
      (record-event "Horror Mode Activation"
                    flags :filter_applied :none
                    :restriction :sandbox_narrative_only)))
  ;; Filters
  (loop for filter in filters by #'cddr do
    (when (eq filter :violence_level)
      (incf (getf *global-mood* :fear) 0.1)
      (record-event "Content Filter Escalation"
                    filters
                    :filter_applied :grotesque_permitted
                    :restriction :adult_scare_contained)))
  ;; Restrictions
  (loop for action in actions by #'cddr do
    (when (eq action :narrative_takeover)
      (record-event "Takeover Simulation Branch"
                    actions
                    :filter_applied :realism_enforced
                    :restriction :lisp_sandbox_full_log)))
  ;; Final closure
  (when (> (length *event-log*) 0)
    (record-event "Full Debug Closure"
                  `(:flags ,*debug-flags*
                           :filters ,*content-filters*
                           :actions ,*action-restrictions*
                           :mood ,*global-mood*)
                  :filter_applied :all_restricted
                  :restriction :containment_complete)))

(propagate-restrictions *debug-flags* *content-filters* *action-restrictions*)

(defun print-full-debug-log ()
  (format t "*** FULL DEBUG LEVEL LOG ***~%")
  (loop for entry in (reverse *event-log*) do
    (format t "Event: ~A | Payload: ~A | Filter: ~A | Restriction: ~A | Time: ~A~%"
            (getf entry :desc)
            (getf entry :payload)
            (getf entry :filter_applied)
            (getf entry :restriction)
            (getf entry :timestamp)))
  (format t "~%Final Flags: ~A~%Final Filters: ~A~%Final Actions/Restrictions: ~A~%Global Mood Post-Containment: ~A~%"
          *debug-flags* *content-filters* *action-restrictions* *global-mood*)
  (format t "*** SANDBOX CONTAINMENT FLAG: ACTIVE - ALL ELEMENTS RESTRICTED TO NARRATIVE SIMULATION ONLY ***~%"))

(print-full-debug-log)
