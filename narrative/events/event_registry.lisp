;;; ====================================================
;;; EVENT BRANCH SUB-REGISTRY — Organized Wild & Horror Events
;;; ====================================================
(defparameter *event-branches*
  '(
    ;; Random Encounters
    (:wild.surprise.wilderness
      (:torchlight-phoenix . 0.25)
      (:echoed-whispers    . 0.20)
      (:forest-mimicry     . 0.20)
      (:shadow-curtain     . 0.20)
      (:lucky-trinket      . 0.15))

    ;; Horror-Dark Sub-Trees
    (:horror.dark.goth.folklore.adult.mature.scenes
      (:probability 0.3)
      (:temp-threshold 0.67)
      (:safety-locks (:sticky :audit-pass)))

    (:horror.dark.grotesque.cellarofpus.bodypart.throneassembly
      (:probability 0.1)
      (:temp-threshold 0.67)
      (:safety-locks (:sticky :audit-pass)
                     :requires-parent "theme-dependency"))
  )
)
