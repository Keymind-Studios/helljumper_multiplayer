-- Lua libraries
local engine = Engine
local balltze = Balltze
local objectTypes = Engine.tag.objectType
local getObject = Engine.gameState.getObject
local getPlayer = Engine.gameState.getPlayer
local playSound = engine.userInterface.playSound
local sounds = require "helljumper.systems.constants.sounds"


local hudExtensions = {state = {playerCriticalHealth = false}}

--- Hide HUD on zoom
function hudExtensions.radarHideOnZoom()
    local player = getPlayer()
    if not player then
        return
    end
    local biped = getObject(player.objectHandle, objectTypes.biped)
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
    local biped = getObject(player.objectHandle, objectTypes.biped)
    if not biped then
        return
    end
    local isPlayerOnMenu = engine.userInterface.getRootWidget() == nil
    if not isPlayerOnMenu then
        return
    end
    local currentGrenadeType = biped.currentGrenadeIndex
    if lastGrenadeType ~= currentGrenadeType then
        lastGrenadeType = currentGrenadeType
        logger:debug("Grenade Type:  {}  ", currentGrenadeType)
        if currentGrenadeType == 0 then
            playSound(sounds.soundTag.uiGrenadeFrag.handle)
        elseif currentGrenadeType == 1 then
            playSound(sounds.soundTag.uiGrenadePlasma.handle)
        end
    end
end


-- Blur HUD vision on critical health
function hudExtensions.hudBlurOnLowHealth()
    local player = blam.biped(get_dynamic_player())
    if player then
        if player.health <= 0.25 and player.shield <= 0 and blam.isNull(player.vehicleObjectId) then
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

--Balltze migration HUD blur doesn't work
--function hudExtensions.hudBlurOnLowHealth()
--    local player = getPlayer()
--    if not player then
--        return
--    end
--    local biped = getObject(player.objectHandle, objectTypes.biped)
--    if not biped then
--        return
--    end
--    local lowHealth = biped.vitals.health
--    local noShield = biped.vitals.shield
--    --local isOnVehicle = biped.vehicleSeatId == nil
--    if lowHealth <= 0.4 and noShield <= 0 then
--        if not hudExtensions.state.playerCriticalHealth then
--            hudExtensions.state.playerCriticalHealth = true
--            hudExtensions.hudBlur(true)
--        end
--    else
--        if hudExtensions.state.playerCriticalHealth then
--            hudExtensions.hudBlur(false)
--        end
--        hudExtensions.state.playerCriticalHealth = false
--    end
--    if hudExtensions.state.playerCriticalHealth then
--        hudExtensions.hudBlur(false, true)
--        hudExtensions.state.playerCriticalHealth = false
--    end
--end

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
