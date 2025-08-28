-- ====================================
-- SlopBucket Beastmode Chaos Engine
-- For Lua-based game devs (LÖVE2D, Defold, custom engines, etc.)
-- ====================================

-- Debug Logging
local DEBUG_GUTS = true
local function debugLog(msg, ...)
  if DEBUG_GUTS then
    print("[SLOPBUCKET.DEBUG] " .. string.format(msg, ...))
  end
end

-- Character/Sprite Struct ===============================
local function newCharacter(name, spriteSheet, quirks, malfunction)
  return {
    name        = name,
    quirks      = quirks,
    malfunction = malfunction,
    spriteSheet = spriteSheet, -- assume love.graphics.newImage(...)
    frame       = 1,
    glitch      = false,
    timer       = 0,
    x           = math.random(50, 500),
    y           = math.random(50, 300),
    speed       = math.random(30, 100)
  }
end

-- Chaos Mutations (glitch frames) ========================
local glitches = {
  function(c) c.frame = math.random(1, 4); debugLog("%s pixel-vomits on frame %d", c.name, c.frame) end,
  function(c) c.x = c.x + math.random(-20,20); c.y = c.y + math.random(-20,20); debugLog("%s teleports spastically!", c.name) end,
  function(c) c.scale = (c.scale or 1) * (0.5 + math.random()); debugLog("%s mutates scale to %.2f", c.name, c.scale) end,
  function(c) c.rotation = (c.rotation or 0) + math.random() * 3.14; debugLog("%s spins into chaos!", c.name) end
}

local actions = {"idling", "attacking", "humping air", "debug-vomiting", "glitch dancing"}

-- Update Cycle ==========================================
local function updateCharacter(c, dt)
  -- Advance timer
  c.timer = c.timer + dt
  if c.timer > 0.2 then
    c.frame = c.frame % 4 + 1
    c.timer = 0

    -- 30% chance of glitch
    if math.random() < 0.3 then
      local g = glitches[math.random(#glitches)]
      g(c)
    else
      debugLog("%s is %s (frame %d)", c.name, actions[math.random(#actions)], c.frame)
    end
  end
end

-- Draw ==================================================
local function drawCharacter(c)
  -- Fake frame calculation: assume each sprite has 4x1 frames
  local fw, fh = c.spriteSheet:getWidth()/4, c.spriteSheet:getHeight()
  local quad = love.graphics.newQuad((c.frame-1)*fw,0,fw,fh, c.spriteSheet:getDimensions())

  love.graphics.draw(c.spriteSheet, quad, c.x, c.y, c.rotation or 0, c.scale or 1, c.scale or 1)
end

-- Beast Mode Loop ========================================
local cast = {
  -- NOTE: replace "love.graphics.newImage(...)" with your sprite paths
  newCharacter("S.Barks!", love.graphics.newImage("sprites/sbarks.png"), "Cybernetic Great Dane", "public violent lashouts"),
  newCharacter("Captain Cupcakes", love.graphics.newImage("sprites/cupcakes.png"), "Sugar demon", "disco reboot"),
  newCharacter("Trashcan R2.DOOM", love.graphics.newImage("sprites/trashcan.png"), "Shit-talking bin", "guts floor spill")
}

function love.update(dt)
  for _,c in ipairs(cast) do
    updateCharacter(c, dt)
  end
end

function love.draw()
  for _,c in ipairs(cast) do
    drawCharacter(c)
  end
end

-- =========================================
-- End of SlopBucket Beastmode Lua Engine
-- Run under LÖVE2D: `love .`
-- =========================================
