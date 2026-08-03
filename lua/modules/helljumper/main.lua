local balltze = Balltze
local engine = Engine

-- Gameplay Core Modules
local dynamicCross = require "helljumper.systems.hud.hudDynamicCrosshair"
local hudExtensions = require "helljumper.systems.hud.hudExtensions"
local healthRegen = require "helljumper.systems.player.playerHealthRegen"
local aimingDownSights = require "helljumper.systems.weapons.weaponAimingDownSights"
--local playerPingObjectives = require "helljumper.systems.player.playerPingObjectives"
--local sprint = require "helljumper.systems.player.playerSprint"
local weaponExtensions = require "helljumper.systems.weapons.weaponExtensions"

-- Functions OnTick
function OnTick()
    dynamicCross.dynamicReticles()
    hudExtensions.radarHideOnZoom()
    hudExtensions.hudBlurOnLowHealth()
    hudExtensions.changeGrenadeSound()
    healthRegen.healthRegen()
    aimingDownSights.adsSystem()
    weaponExtensions.casterFixHeat()
    -- playerPingObjectives.pingObjectives()
end

-- v2 has no draw_text: Engine.hud.addText keeps the string on screen on its own, so instead
-- of drawing every frame we only add it and take it away as the menu comes and goes.
local versionText
-- addText measures x/y inward from the anchored corner, so v1's bounds (right 632 of 640,
-- top 460 of 480) become an 8/20 inset from the bottom right corner, and the old
-- align = "right" is now justification. v2 takes a font tag handle instead of v1's
-- "smaller" name; a nil handle just falls back to the default font.
local versionTextPosition = {x = 6, y = 0}
local versionTextOptions = {
    color = {a = 1.0, r = 0.45, g = 0.72, b = 1.0},
    font = engine.tag.lookupTag("keymind\\helljumper\\ui\\fonts\\small_ui", "vector_font"),
    layer = "ui",
    anchor = "bottomRight",
    justification = "right"
}

-- Tracked apart from the handle: addText does not hand one back on every Balltze build, and
-- keying "is it already up?" off the handle alone would re-add the text on every frame.
local versionTextShown = false

-- Print version on pause menu
function OnFrame()
    local isPlayerOnMenu = engine.uiWidget.getActiveWidget() ~= nil
    if not isPlayerOnMenu then
        if versionTextShown then
            if versionText and versionText.remove then
                versionText:remove()
            end
            versionText = nil
            versionTextShown = false
        end
        return
    end
    if not versionTextShown then
        versionText = engine.hud.addText("helljumpermp-5.2.0", versionTextPosition.x,
                                         versionTextPosition.y, versionTextOptions)
        versionTextShown = true
    end
end

local onTickEvent = balltze.addEventListener("tick", function()
    OnTick()
end)

local onFrameEvent = balltze.addEventListener("frame", function()
    OnFrame()
end)

-- The v1 "rconMessage" event has no v2 equivalent yet, so client side rcon requests are not
-- dispatched here anymore. On the server blam.rcon.handle is still reached through
-- blam.rcon.patch, called from PluginOnSappLoad.
--local onRconMessageEvent = balltze.event.rconMessage.subscribe(function(event)
--    if event.time == "before" then
--        if blam.rcon.handle(event.context:message()) == false then
--            event:cancel()
--        end
--    end
--end)

-- addEventListener does not hand back an EventListener on every Balltze build, and addText
-- does not always hand back a HudText either, so nothing here assumes it has a handle.
local function removeHandle(handle)
    if handle and handle.remove then
        handle:remove()
    end
end

return {
    unload = function()
        balltze.logger.warning("Unloading main")
        removeHandle(onTickEvent)
        removeHandle(onFrameEvent)
        removeHandle(versionText)
        -- Must be reset: require caches this module, so a reload would otherwise come back
        -- believing the text is still up and never add it again.
        versionText = nil
        versionTextShown = false
        -- weaponAimingDownSights owns a player_input listener of its own, so it needs the same
        -- treatment.
        aimingDownSights.unload()
    end
}
