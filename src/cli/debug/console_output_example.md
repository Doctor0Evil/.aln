# Example simulated CLI output
# File: /src/cli/debug/console_output_example.md

[CONSOLE_OUTPUT_FULL]
> XP Event: Calculating XP for killing rat at L4...
> Using: /src/ai/xp/xp_math_mutator.lisp :: default-xp-per-kill
> Result: 7 XP
--
> Mutate XP formula? (/mutate-xp-formula)
> XP mutation event: Base XP +20% | Formula added to stack!
> Next kill event will use new logic from stack head.

> XP Event: Calculating XP for killing TammyTwiceTooth (Boss L40) at player L10...
> Result: 5,126 XP (after all modifiers and event folding, including “Boss kill bonus”)

> Mutation history: /src/ai/xp/xp_mutation_stack.lisp :: see stack for formula log
```

***

**All logic and file placement referenced per:**
**https://github.com/Doctor0Evil/ALN_Programming_Language.git**

***

### [ALN_Command_Terminal :: XP Event — Mutation Knowledgebase]

- All snippets above are directly mapped to the repo and file structure.
- ℹ️ XP calc events now use a dynamic knowledgebase (mutation/fold-aware; Lisp + MATLAB).
- At any moment, run `/mutate-xp-formula` to inject a new math logic (stacked, reversible, traceable).
- All outputs, logs, and calculated event remainders are fully traceable by source and stack.

***

[CONSOLE_INPUT >>>]
Options:
(/xp-per-event player rat)
(/xp-per-event player tammytwicetooth)
(/mutate-xp-formula)
(/knowledgebase.log show)
(/advance_turn)
