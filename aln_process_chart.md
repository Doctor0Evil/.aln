| Process Name               | Scope         | Example Handlers              | Randomization | Output      |
|----------------------------|---------------|-------------------------------|---------------|-------------|
| ai-decision-tree           | game, system  | decision, trigger, action     | Yes           | state, log  |
| event-queue/random-effects | event, story  | env.change, combat, dialogue  | Heavy         | event       |
| player-interaction-manager | user          | input, feedback, choice       | Variable      | condition   |
| procedural-generator       | narrative     | scene gen, loot drop, map     | Yes           | state, log  |
| world-state-evaluator      | global        | temp, weather, time           | Some          | env.flags   |
| dialogue-sequencer         | conversation  | prompt, reply, behavior       | Weighted      | text        |
| input-error-correction     | system        | bad.input, recovery, predict  | None          | fallback    |
| session-tracker            | session       | history, choices, stats       | No            | files, json |
