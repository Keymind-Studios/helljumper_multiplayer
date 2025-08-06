-- Lua libraries
local engine = Engine
local getObject = Engine.gameState.getObject
local getPlayer = Engine.gameState.getPlayer

local playerHealthRegen = {}

local isGameClient = function()
    return engine.netgame.getServerType() == "local" or engine.netgame.getServerType() == "none"
end

local maxHealth = 1
local healthRegenerationAmount = 0.02

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
        if biped.vitals.health < maxHealth and biped.vitals.shield > 0.98 and not (biped.vehicleSeatId == 0 and biped.vehicleSeatId <= 15) then
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

return playerHealthRegen
