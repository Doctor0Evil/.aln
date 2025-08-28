;; File: commands/create_intel_i_link_cfg.lisp

(define-object :object-name 'intel-i-link-cfg
  :path "systems.intel.link.cfg:path$config"
  :logic (lambda ()
    ;; Step 1. Initialize config object state
    (log "[DEBUG] INTEL-LINK-CFG: Begin configuration sequence (command.create.intel.i.link.cfg)")
    (let ((config (make-hash-table)))
      (setf (gethash :link-enabled config) nil)
      (setf (gethash :init-timestamp config) (get-universal-time))
      (log (format nil "[DEBUG] Config initialized at ~A" (gethash :init-timestamp config)))
      ;; Step 2. Register system hooks for link activation
      (when (system-module-exists-p :intel-link-core)
        (setf (gethash :hook-registered config) t)
        (log "[DEBUG] INTEL-LINK-CFG: Core module found, hook registered."))
      (unless (gethash :hook-registered config)
        (log "[WARN] INTEL-LINK-CFG: Core module missing, fallback mode."))
      ;; Step 3. Validate link requirements
      (let ((pass (and (featurep :link-security) (featurep :link-audit))))
        (if pass
          (progn
            (setf (gethash :link-enabled config) t)
            (log "[DEBUG] INTEL-LINK-CFG: Security & audit checks passed, link enabled.")
            (emit-event :intel-link-cfg :activated))
          (progn
            (setf (gethash :link-enabled config) nil)
            (log "[ERROR] INTEL-LINK-CFG: Security/audit prerequisite(s) failed. Link disabled.")
            (emit-event :intel-link-cfg :activation-failed))))
      ;; Step 4. Write config to global state for hot-reload
      (setf (global 'intel-link-config) config)
      (log (format nil "[DEBUG] INTEL-LINK-CFG: Config state flushed to global env. (enabled: ~A)"
                   (gethash :link-enabled config)))
      ;; Step 5. Final state and return
      (list :status (if (gethash :link-enabled config) 'success 'fail)
            :config config
            :event-log '(:init :register-hook :validate :apply :emit-event :write-global :finalize))
    ))
)
