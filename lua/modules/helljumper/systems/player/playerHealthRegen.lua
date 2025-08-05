-- Lua libraries
local engine = Engine
local getObject = Engine.gameState.getObject
local getPlayer = Engine.gameState.getPlayer

local playerHealthRegen = {}

local isGameClient = function()
    return engine.netgame.getServerType() == "local" or engine.netgame.getServerType() == "none"
end

local maxHealth = 1
local healthRegenerationAmount = 0.0037

function playerHealthRegen.healthRegen()
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        if not player then
            return
        end
        local biped = getObject(player.objectHandle, engine.tag.objectType.biped)
        if not biped then
            return
        end
        if isGameClient() then
            if biped.vitals.health <= 0 then
                biped.vitals.health = 0.000000001
            end
        end
        if biped.vitals.health < maxHealth and biped.vitals.shield > 0.95 then
            local newPlayerHealth = biped.vitals.health + healthRegenerationAmount
            if newPlayerHealth > 1 then
                biped.vitals.health = 1
                -- if isGameClient() then
                --    healthRegen.playSound(const.sounds.humanRifleZoomIn, 5)
                -- end
            else
                biped.vitals.health = newPlayerHealth
            end
        end
    end
end


---- Lua Blam

--- Attempt to play a sound given tag path and optionally a gain number
-- function healthRegen.playSound(tagPath, gain)
--    local player = blam.player(get_player())
--    if player then
--        local playSoundCommand = const.hsc.playSound:format(tagPath, player.index, gain or 1.0)
--        execute_script(playSoundCommand)
--    end
-- end

----- Regenerate players health on low shield using game ticks
-----@param playerIndex? number
--function playerHealthRegen.regenerateHealth(playerIndex)
--    -- if blam.isGameSAPP() or isGameClient() then
--    local player
--    if playerIndex then
--        player = blam.biped(get_dynamic_player(playerIndex))
--    else
--        player = blam.biped(get_dynamic_player())
--    end
--    if player then
--        -- Fix muted audio shield sync
--        -- TODO Not sure this will work as expected
--        if isGameClient() then
--            if player.health <= 0 then
--                player.health = 0.000000001
--            end
--        end
--        if player.health < 1 and player.shield >= 0.98 and blam.isNull(player.vehicleObjectId) then
--            local newPlayerHealth = player.health + const.healthRegenAiAmount
--            if newPlayerHealth > 1 then
--                player.health = 1
--                -- if isGameClient() then
--                --    healthRegen.playSound(const.sounds.humanRifleZoomIn, 5)
--                -- end
--            else
--                player.health = newPlayerHealth
--            end
--        end
--    end
--    -- end
--end

return playerHealthRegen
