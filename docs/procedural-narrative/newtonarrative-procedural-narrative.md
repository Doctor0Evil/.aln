# Procedural Narrative and How to Keep it Coherent

## What Is Procedural Narrative?

Typically, when we talk about "story" in games or interactive fiction we think of either a *linear* story that the player experiences plot beat to plot beat. Or perhaps a *branching* story where the player's choice influences the direction the story takes. I like to describe *procedural narrative* as a branching story of which the creators don't know *if* or *when* a plot beat will show up.

A popular approach of *procedural narrative* is chopping up the story into little pieces, that I like to call *events*. We pile these *events* up in a big stack, like a deck of cards, and then sort them by *probability*. *Probability* in this case refers to the likelihood of that card being picked next. *Probability* in this case refers to the likelihood of that card being picked next.

## What Makes Procedural Narrative Incoherent?

These randomized *events* can be a strong foundation for a *procedural narrative*. The hard part is controlling the randomness so that the overall story still makes sense. I've found that there are three major inconsistencies you can run into when creating a *procedural narrative*: *continuity*, *tone*, and *pacing*.

### Continuity
*Continuity* is how well-established the consequences of plot beats are in your story. If one character dies in a plot beat but is suddenly alive without explanation in the next, that breaks *continuity*.

### Tone
*Tone* is the overall atmosphere of your story. If your story is a dark, gritty, murder mystery, it would feel very jarring for a cartoon dog to suddenly walk into your scene and quip a few jokes.

### Pacing
*Pacing* is the rhythm and speed in which a story unfolds. If an adrenaline-pumping action scene gets interrupted by ten minutes of heartfelt dialogue, the sudden shift might feel jarring to the player.

## How Do We Keep Procedural Narrative Coherent?

### Prerequisites & Guard Clauses
In short, a *prerequisite* is when you check if certain conditions are fulfilled to allow an *event* to get picked. If a character comments on the player's cool sword, the player should actually have a sword equipped, right? Similarly, you can abort *events* based on certain conditions as well. If the current *event* is triggered by a cursed artifact in the player's inventory, but they just sold their entire cargo off to a merchant, you might want to cancel it altogether. That's called a *guard clause*.

### Foreshadowing
In a previous example, we talked about how Loki can potentially push you off of a cliff randomly. Naturally, the player might find that unreasonable if they don't even know that Loki is around. Foreshadowing is a way to warn the player of upcoming events, albeit indirectly.

### The *TAPIR* Method
The *Thematically Accurate Procedural Interaction Registry*, or *TAPIR* for short, is a system I initially designed for *procedural* dungeon generation. Know that I will jump through hoops to make an acronym work. The goal was to make sure that all the creatures and loot inside would fit the dungeon's setting.

### Roll With The Implausibility
Lastly, this technique, if you can even call it that, might seem like a bit of a cop-out. Instead of trying to fix inconsistencies, you can simply try to justify them. Embrace the randomness of *procedural narrative* by setting it in a multiverse, for example.

## TLDR
**What is procedural narrative?** 
Procedural Narrative is a story that is driven by dynamic "procedures", as opposed to a pre-defined path.

**What's the difference between linear, branching and procedural narrative?** 
A linear story unfolds plot beat to plot beat: the player starts at the beginning and finishes at the end. A branching story is much the same, but the direction changes based on the player's choices. A procedural story is usually jumbled up, so plot beats can be experienced independently from each other.

**What makes procedural narrative incoherent?** 
Your lack of control over when plot beats will happen makes you run into inconsistencies in continuity, tone and pacing.
