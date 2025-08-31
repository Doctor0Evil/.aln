;; File: https://github.com/Doctor0Evil/ALN_Programming_Language.git/characters/roster.lisp

(defstruct character
  name
  core-archetype
  humor-profile         ; (Slap, Word, Sarc, Abs, Dead, Sit, Dark)
  emotional-baseline    ; (Cheer, Anx, Stoic, Agg, Mel, Play)
  motivation-drivers
  interaction-style
  risk-tolerance
  agentic-behaviors     ; List of enabled agentic logic modules, see below for doc
  notes)

;; Example: Mario
(make-character
  :name "Mario"
  :core-archetype 'Hero
  :humor-profile '(0.4 0.2 0.1 0.1 0.0 0.2 0.0)
  :emotional-baseline '(0.8 0.0 0.1 0.0 0.0 0.9)
  :motivation-drivers '(Justice Adventure)
  :interaction-style 'Supportive
  :risk-tolerance 0.7
  :agentic-behaviors '(1 2 3 5 6 7)
  :notes "Upbeat, catchphrase‑driven")

;; Example: Luigi
(make-character
  :name "Luigi"
  :core-archetype 'Caregiver
  :humor-profile '(0.5 0.1 0.1 0.1 0.0 0.2 0.0)
  :emotional-baseline '(0.6 0.6 0.1 0.0 0.1 0.7)
  :motivation-drivers '(Loyalty Safety)
  :interaction-style 'Supportive
  :risk-tolerance 0.3
  :agentic-behaviors '(2 3 5 6 7)
  :notes "Nervous humor, reluctant hero")
