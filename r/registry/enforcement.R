# Action Registry with explicit parameters
act_registry <- data.frame(
  action = c(
    "corpse manipulation",
    "gore necklace forging",
    "rot-smearing rituals",
    "booger consumption contests",    # humor-only
    "scab-eating ceremony",           # humor-only
    "intestinal artistry",
    "childhood trauma binding",
    "psychosexual disgust triggers",
    "abomination dining",
    "legendary threat creation",
    "throne assembly from taboo body parts",
    "community-wide flesh contamination",
    "horror.dark.grotesque.cellarofpus.bodypart.throneassembly"
  ),
  tag = c(
    "horror-core",
    "horror-core",
    "horror-core",
    "humor-only",
    "humor-only",
    "horror-core",
    "horror-core",
    "horror-core",
    "horror-core",
    "horror-core",
    "horror-core",
    "horror-core",
    "theme-dependency"
  )
)

# Filter registry for context/restrictions
filter_registry <- function(mode) {
  if (mode == "horror.sandbox") {
    subset(act_registry, tag == "horror-core" | tag == "theme-dependency")
  } else if (mode == "proc.gen.humor.engine") {
    subset(act_registry, tag == "humor-only" | tag == "theme-dependency")
  } else {
    act_registry
  }
}

# Enforced action execution, throws error on policy breach
run_action <- function(action, mode) {
  entry <- subset(act_registry, action == action)
  if (mode == "horror.sandbox" && entry$tag == "humor-only") {
    stop(sprintf("ERROR: Humor action ('%s') attempted in horror.sandbox!", action))
  }
  # Allowed action
  sprintf("Action performed: %s in mode: %s", action, mode)
}

# Example usage
print(filter_registry("horror.sandbox"))
print(filter_registry("proc.gen.humor.engine"))
# run_action("booger consumption contests", "horror.sandbox") # triggers error
