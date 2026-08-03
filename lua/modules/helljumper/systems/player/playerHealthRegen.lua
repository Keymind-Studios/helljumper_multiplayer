-- Lua libraries
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer

local playerHealthRegen = {}

-- v1 asked netgame for the server type and accepted "local" or "none", meaning "anything
-- that is not a dedicated server". v2 folds both of those into the "local" connection type.
local isGameClient = function()
    return engine.game.getGameConnectionType() == "local"
end

local maxHealth = 1
local healthRegenerationAmount = 0.02

function playerHealthRegen.healthRegen()
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        if not player then
            return
        end
        local biped = getObject(player.unitHandle, "biped")
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
