DebugMode = true
blam = require "blam"
local balltze = Balltze
local engine = Engine
local drawText = balltze.chimera.draw_text

-- Gameplay Core Modules
local dynamicCross = require "helljumper.systems.hud.hudDynamicCrosshair"
local hudExtensions = require "helljumper.systems.hud.hudExtensions"
local healthRegen = require "helljumper.systems.player.playerHealthRegen"
--local aimingDownSights = require "helljumper.systems.weapons.weaponAimingDownSights"
--local playerPingObjectives = require "helljumper.systems.player.playerPingObjectives"
--local sprint = require "helljumper.systems.player.playerSprint"
--local weaponExtensions = require "helljumper.systems.weapons.weaponExtensions"

-- Functions OnTick
function OnTick()
    dynamicCross.dynamicReticles()
    hudExtensions.radarHideOnZoom()
    hudExtensions.hudBlurOnLowHealth()
    hudExtensions.changeGrenadeSound()
    healthRegen.healthRegen()
    -- aimingDownSights.customKeys()
    -- playerPingObjectives.pingObjectives()
    -- weaponExtensions.casterFixHeat()
end

-- Print version on pause menu
function OnFrame()
    local isPlayerOnMenu = engine.userInterface.getRootWidget() ~= nil
    if isPlayerOnMenu then
        return
    end
    local font = "smaller"
    local align = "right"
    local bounds = {left = 0, top = 460, right = 632, bottom = 480}
    local textColor = {1.0, 0.45, 0.72, 1.0}
    drawText("helljumpermp-5.2.0", bounds.left, bounds.top, bounds.right, bounds.bottom, font, align, table.unpack(textColor))
end

local onTickEvent = balltze.event.tick.subscribe(function(event)
    if event.time == "before" then
        OnTick()
    end
end)

local onFrameEvent = balltze.event.frame.subscribe(function(event)
    if event.time == "before" then
        OnFrame()
    end
end)

local onRconMessageEvent = balltze.event.rconMessage.subscribe(function(event)
    if event.time == "before" then
        if blam.rcon.handle(event.context:message()) == false then
            event:cancel()
        end
    end
end)

return {
    unload = function()
        logger:warning("Unloading main")
        onTickEvent:remove()
        onFrameEvent:remove()
        onRconMessageEvent:remove()
    end
}
