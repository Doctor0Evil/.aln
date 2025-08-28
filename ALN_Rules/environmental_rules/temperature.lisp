;;;; file: ALN_Rules/environmental_rules/temperature.lisp
;; repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defrule apply-cold-hazard
  "Debuff stamina/recovery if zone temperature < 2°C and no cold gear"
  (zone-temp player-gear)
  (when (< zone-temp 2)
    (unless (has-cold-gear? player-gear)
      (apply-debuff 'stamina 0.08)
      (apply-debuff 'recovery-rate 0.12)
      (emit-signal 'environmental-hazard-triggered :type 'cold :severity 'mild)))
)
