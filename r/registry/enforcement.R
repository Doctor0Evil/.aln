# Registry entries with tags
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

# Registry filtering function
filter_registry <- function(mode) {
  if (mode == "horror.sandbox") {
    subset(act_registry, tag == "horror-core" | tag == "theme-dependency")
  } else if (mode == "proc.gen.humor.engine") {
    subset(act_registry, tag == "humor-only" | tag == "theme-dependency")
  } else {
    act_registry
  }
}

# Example usage
sandbox_registry <- filter_registry("horror.sandbox")
humor_registry   <- filter_registry("proc.gen.humor.engine")

print("Horror Sandbox Registry:")
print(sandbox_registry)

print("Humor Engine Registry:")
print(humor_registry)

# Enforcement: block humor actions in horror.sandbox
run_action <- function(action, mode) {
  entry <- subset(act_registry, action == action)
  if (mode == "horror.sandbox" && entry$tag == "humor-only") {
    stop(paste("ERROR: Attempted humor action in horror.sandbox:", action))
  }
  paste("Action performed:", action)
}

# Example, triggers error:
# run_action("booger consumption contests", "horror.sandbox")
