(defstruct aln.character.vector
  id core.traits banter.mode compliance.guard adaptive.modules
  self.heal.handlers humor.seed memory.weighting vector.morphs)

(defparameter *vec-hardlight-ronin*
  (make-aln.character.vector
   :id :aln.vec.hardlight-ronin
   :core.traits '(:stoic :codebound :auditloyal)
   :banter.mode :max-permitted-profane
   :compliance.guard #'verify-compliance
   :adaptive.modules '(:combat-sim :manifest-modder :yml-healer)
   :self.heal.handlers '((:drift . repair-manifest)
                         (:fail-lint . reformat-module))
   :humor.seed '42-mildly-chaotic
   :memory.weighting '((:compliance . 1.0)
                       (:banter . 0.7)
                       (:ci-ops . 0.9))
   :vector.morphs '(:switch-to-audit-mode-on-PR-merge)))

(defparameter *vec-sarcastic-skyforge*
  (make-aln.character.vector
   :id :aln.vec.sarcastic-skyforge
   :core.traits '(:visionary :meta-aware :forgeproud)
   :banter.mode :dry-wit-compliance-checked
   :compliance.guard #'verify-compliance
   :adaptive.modules '(:ci-orchestrator :persona-handoff :slopbucket-debug)
   :self.heal.handlers '((:failed-release . auto-patch-and-retag))
   :humor.seed '999-chaotic-neutral
   :memory.weighting '((:ci-ops . 1.0)
                       (:humor . 0.95)
                       (:world-events . 0.8))
   :vector.morphs '(:escalate-wit-if-builds-green)))
