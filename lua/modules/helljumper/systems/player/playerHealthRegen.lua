local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer

local playerHealthRegen = {}

local maxHealth = 1
local healthRegenerationAmount = 0.02

---@param player Player
---@param isGameClient boolean
local function healthRegeneration(player, isGameClient)
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    if isGameClient then
        if biped.vitals.health <= 0 then
            biped.vitals.health = 0.000000001
        end
    end
    local isOnFoot = biped.parentObject:isNull()
    if biped.vitals.health < maxHealth and biped.vitals.shield > 0.98 and isOnFoot then
        local newPlayerHealth = biped.vitals.health + healthRegenerationAmount
        if newPlayerHealth > 1 then
            biped.vitals.health = 1
        else
            biped.vitals.health = newPlayerHealth
        end
    end
end

function playerHealthRegen.healthRegen()
    -- Asked once for the whole list rather than once a player: what kind of game this is does not
    -- change between two players of it.
    local isGameClient = engine.game.getGameConnectionType() == "local"
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        if player then
            healthRegeneration(player, isGameClient)
        end
    end
end

return playerHealthRegen
