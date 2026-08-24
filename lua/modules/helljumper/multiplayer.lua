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

-- Every system, for the two ends of a map's life. What each does on the tick is gameplaySystems'
-- business and is written out there by hand, because each line of it carries a name for the meter;
-- this list carries no such thing, so a system is added to it once and is both loaded and unloaded
-- by that. Before it there were two lists to remember, and a system left off either failed quietly:
-- off the load, its tables stay empty and it does nothing; off the unload, it holds the last map's
-- handles into the next one.
--
-- The performance meter is deliberately not here. It belongs to the plugin rather than to the map,
-- counting from the moment the plugin loads, and helljumper_multiplayer.lua takes it up and down.
local systems = {
    input, healthRegen, pingObjectives, dynamicCross, weaponExtensions, hudExtensions,
    secondaryWeaponIcons, aimingDownSights
}

--- What every system works out once, while the map it belongs to is coming up.
---
--- All of it is tag handles, or tables keyed by them, and a handle is an index into the loaded map:
--- good until that map goes and meaningless after. Which is the whole point of doing it here. The
--- tick then asks its questions with the handle an object already carries, instead of reading a path
--- out of a tag thirty times a second to look the same thing up by.
---
--- Order matters once: loadTags.get() is what puts the handles in constants.tags, and a system that
--- reads them reads them after. Past that the systems do not depend on one another's loading.
function multiplayer.load()
    loadTags.get()
    for index = 1, #systems do
        local system = systems[index]
        -- Only some of them have anything to work out, and a system that grows a load() later is
        -- picked up by this without being named anywhere else.
        if system.load then
            system.load()
        end
    end
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
    measure("secondaryWeapons", secondaryWeaponIcons.showSecondaryWeapons)
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

--- Give back everything that belonged to the map that is going: what is drawn on the screen, what is
--- written into its tags, and the handles into it that nothing may carry into the next one.
---
--- In the same order as the loading rather than backwards, since none of them undoes anything
--- another one of them did.
function multiplayer.unload()
    for index = 1, #systems do
        local system = systems[index]
        if system.unload then
            system.unload()
        end
    end
end

return multiplayer