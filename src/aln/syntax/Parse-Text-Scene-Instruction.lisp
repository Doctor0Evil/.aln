# --- STEP 1: Parse Text Scene Instruction ---

scene-description = "Night Whispers Game World: Blueprint & Template (Mature-Audience "Urban Legend" Survival Horror) 1. Core World Principles Setting & Themes A sprawling, decaying city limned by rain and shadows, where folklore births reality. The city's heart is a labyrinth—wards split by alleyways, sunken parks, ancient tenements, derelict amusements, drowned subways, each crumbling under the weight of urban myths. Environment is dynamic: Each zone's mood (fog, rainfall, lighting, rumor-intensity) changes by time, weather, recent events, and local stories. Creatures and horrors evolve as legends are fed or debunked. Visual/Audio Tone Constant dread, liminality, and spectral beauty. Audio cues: unnatural hush, children's laughter in rain, music box drifts, sudden metallic groan, urban cacophony peaking and falling with the wind. Flickering lights, creeping mildew, fungal blooms in odd shadowed corners; sometimes your own reflection moves out of sync. Explicit: Scenes of haunting domesticity-turned-wrong, subtle adult themes of loss, betrayal, trauma, and desire woven through both major and minor events."

# Parse scene description into structured data
extracted-entities = {
  :world => {
    :setting => "decaying city limned by rain and shadows",
    :themes => ["folklore births reality", "urban myths"],
    :dynamic => ["mood (fog, rainfall, lighting, rumor-intensity) changes by time, weather, recent events, and local stories"],
    :creatures => "horrors evolve as legends are fed or debunked"
  },
  :visual => {
    :audio => ["unnatural hush", "children's laughter in rain", "music box drifts", "sudden metallic groan", "urban cacophony"],
    :visual => ["flickering lights", "creeping mildew", "fungal blooms", "reflection moves out of sync"],
    :explicit => ["haunting domesticity-turned-wrong", "loss, betrayal, trauma, and desire"]
  }
}

# Generate prompt for image generation
main-prompt = "Portrait of a decaying city limned by rain and shadows, folklore born reality, urban myths, constant dread, spectral beauty, children's laughter in rain, music box drifts, sudden metallic groan, flickering lights, creeping mildew, fungal blooms, reflection moves out of sync."
