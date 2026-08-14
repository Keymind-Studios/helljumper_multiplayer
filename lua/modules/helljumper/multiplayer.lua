local balltze = Balltze
local engine = Engine
local script = require "script"
-- Resources Modules
local loadTags = require "helljumper.systems.constants.tags"
local input = require "helljumper.systems.core.input"

-- Gameplay Core Modules
local healthRegen = require "helljumper.systems.player.playerHealthRegen"
local pingObjectives = require "helljumper.systems.player.playerPingObjectives"
local dynamicCross = require "helljumper.systems.hud.hudDynamicCrosshair"
local weaponExtensions = require "helljumper.systems.weapons.weaponExtensions"
local hudExtensions = require "helljumper.systems.hud.hudExtensions"
local secondaryWeaponIcons = require "helljumper.systems.hud.hudSecondaryWeapons"
local aimingDownSights = require "helljumper.systems.weapons.weaponAimingDownSights"
local performanceMeter = require "helljumper.systems.debug.debugPerformanceMeter"

local multiplayer = {}

local measure = performanceMeter.run


function multiplayer.load()
    loadTags.get()
    -- After it and not before: what this works out is keyed and filled by the handles that call
    -- puts in constants.tags.
    hudExtensions.load()
    balltze.logger.info("Loaded Helljumper Multiplayer Systems and Resources!")
end

--- What is stepped on the tick.
---
--- Each system goes through the meter under a name, which is what puts a line of its own on the
--- performance readout: what the meter is not handed, it cannot report on, and the Lua Total it
--- prints is the sum of these and nothing else. The names are what appear on screen, so they are
--- kept short enough to sit in the readout's column.
---
--- The functions are passed by reference rather than wrapped in a closure, so this allocates nothing
--- per tick, and the meter drops the measuring entirely when it has nowhere to publish a reading.
function multiplayer.gameplaySystems()
    measure("input", input.ensureListener)
    measure("healthRegen", healthRegen.healthRegen)
    measure("dynamicCrosshair", dynamicCross.dynamicReticles)
    measure("weaponAge", weaponExtensions.syncWeaponAge)
    measure("hudExtensions", hudExtensions.init)
    measure("secondaryWeapons", secondaryWeaponIcons.showSecondaryWeaponIcons)
    measure("pingObjectives", pingObjectives.pingObjectives)
    measure("ads onTick", aimingDownSights.adsSystem)
end

--- What is stepped on the frame rather than on the tick.
---
--- Kept apart from gameplaySystems because the two answer different questions: the tick decides what
--- should be on screen, the frame moves it there. Stepping movement on the tick would quantise it
--- into the 30 steps a second the game thinks in, however smoothly the game is running.
---
--- Systems do not subscribe to "frame" for themselves. Balltze keeps one listener per event name, so
--- each subscription silently replaces the last, and whichever registered last was the only one left
--- drawing.
function multiplayer.frameSystems()
    measure("ads onFrame", aimingDownSights.updateShownElements)
end

function multiplayer.unload()
    input.unload()
    hudExtensions.unload()
    secondaryWeaponIcons.unload()
    pingObjectives.unload()
end

return multiplayer