local balltze = Balltze
local engine = Engine

-- Resources Modules
local loadTags = require "helljumper.systems.constants.tags"

-- Gameplay Core Modules
local healthRegen = require "helljumper.systems.player.playerHealthRegen"
local dynamicCross = require "helljumper.systems.hud.hudDynamicCrosshair"


local multiplayer = {}


function multiplayer.load()
    loadTags.get()
    balltze.logger.info("Loaded Helljumper Multiplayer Systems and Resources!")
end

function multiplayer.gameplaySystems()
    healthRegen.healthRegen()
    dynamicCross.dynamicReticles()
end

return multiplayer