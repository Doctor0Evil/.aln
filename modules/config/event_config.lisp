;;; ====================================================
;;; AUTO-CONFIGURABLE EVENT PIPE CONFIGURATION
;;; ====================================================
(defparameter *default-event-config*
  '(
    :probability     (random-range 0.05 0.25)
    :temp-threshold  0.67
    :npc-reactions   (pick '(:enabled :disabled))
    :surreal-factor  (random-range 0.1 0.35)
    :rarity-tier     (roll-weighted '(:common . 0.6) (:rare . 0.3) (:epic . 0.1))
    :requires-sticky true
    :audit-required  true
  ))
