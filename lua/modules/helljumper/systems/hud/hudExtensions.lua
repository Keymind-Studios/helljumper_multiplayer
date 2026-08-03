-- Lua libraries
local engine = Engine
local balltze = Balltze
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local hsc = require "hsc"
local sounds = require "helljumper.systems.constants.sounds"

-- v2 removed Engine.userInterface.playSound, so play these through HSC like the rest of the
-- project does. The project wide command attaches the sound to a player with
-- (list_get (players) n), but v2's Player carries no players list index, and "none" suits
-- these UI sounds better anyway: it plays them non positionally, for this client only.
local playSoundCommand = [[(begin (sound_impulse_start "%s" none %s))]]

local function playSound(tagPath, gain)
    if not tagPath then
        return
    end
    execute_script(playSoundCommand:format(tagPath, gain or 1.0))
end

local hudExtensions = {state = {playerCriticalHealth = false}}

--- Hide HUD on zoom
function hudExtensions.radarHideOnZoom()
    local player = getPlayer()
    if not player then
        return
    end
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    local levelZoom1 = biped.desiredZoomLevel == 0
    local levelZoom2 = biped.desiredZoomLevel == 1
    if levelZoom1 or levelZoom2 then
        execute_script("hud_show_motion_sensor 0")
    else
        execute_script("hud_show_motion_sensor 1")
    end
end

local lastGrenadeType = nil
function hudExtensions.changeGrenadeSound()
    local player = getPlayer()
    if not player then
        return
    end
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    local isPlayerOnMenu = engine.uiWidget.getActiveWidget() ~= nil
    if isPlayerOnMenu then
        return
    end
    local currentGrenadeType = biped.currentGrenadeIndex
    if lastGrenadeType ~= currentGrenadeType then
        lastGrenadeType = currentGrenadeType
        balltze.logger.debug("Grenade Type:  {}  ", currentGrenadeType)
        if currentGrenadeType == 0 then
            playSound(sounds.soundTag.uiGrenadeFrag and sounds.soundTag.uiGrenadeFrag.path)
        elseif currentGrenadeType == 1 then
            playSound(sounds.soundTag.uiGrenadePlasma and sounds.soundTag.uiGrenadePlasma.path)
        end
    end
end


-- Blur HUD vision on critical health
function hudExtensions.hudBlurOnLowHealth()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    if biped then
        -- A biped riding a vehicle is parented to it, which is what the old vehicleObjectId
        -- null check was really asking.
        local isOnVehicle = not biped.parentObject:isNull()
        if biped.vitals.health <= 0.25 and biped.vitals.shield <= 0 and not isOnVehicle then
            if not hudExtensions.state.playerCriticalHealth then
                hudExtensions.state.playerCriticalHealth = true
                hudExtensions.hudBlur(true)
            end
        else
            if hudExtensions.state.playerCriticalHealth then
                hudExtensions.hudBlur(false)
            end
            hudExtensions.state.playerCriticalHealth = false
        end
    elseif hudExtensions.state.playerCriticalHealth then
        hudExtensions.hudBlur(false, true)
        hudExtensions.state.playerCriticalHealth = false
    end
end

--- HUD Blur
---@param enableBlur boolean
---@param immediate any
function hudExtensions.hudBlur(enableBlur, immediate)
    if enableBlur then
        execute_script([[(begin
                            (cinematic_screen_effect_start true)
                            (cinematic_screen_effect_set_convolution 2 1 1 1 5)
                            (cinematic_screen_effect_start false)
                        )]])
        return true
    end
    if not enableBlur and immediate then
        execute_script([[(begin
                        (cinematic_screen_effect_set_convolution 2 1 1 0 1)
                        (cinematic_screen_effect_start false)
                        (cinematic_stop)
                    )]])
        return false
    end
    execute_script([[(begin
                        (cinematic_screen_effect_set_convolution 2 1 1 0 1)
                        (cinematic_screen_effect_start false)
                        (sleep 45)
                        (cinematic_stop)
                    )]])
    return false
end

-- Shake screen effect when biped is melee, not working yet
-- function gameplay.meleeScreen()

--    local player = blam.player(get_player())
--   local playerObject = blam.biped(get_object(player.objectId))
--    if playerObject then
--        -- console_out(playerObject.zoomLevel)
--        if playerObject.meleeKey then
--            execute_script(
--                [[(begin (damage_object keymind\halo_infinite\halo_infinite\weapons\rifle\stalker_rifle\_fx__kinestecia\overheated.damage_effect") (unit (list_get) (players) 0) )]])
--            -- execute_script([[damage_object keymind\\halo_infinite\\halo_infinite\\weapons\\rifle\\stalker_rifle\\_fx\\_kinestecia\\overheated]])
--        end
--    end
-- end

return hudExtensions
