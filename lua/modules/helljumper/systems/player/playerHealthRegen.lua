local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer

local playerHealthRegen = {}

local maxHealth = 1
local healthRegenerationAmount = 0.02

---@param player Player
---@param isLocalGame boolean Whether this is a game of its own rather than one hosted for others
local function healthRegeneration(player, isLocalGame)
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    if isLocalGame then
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

--- Only meant to run where the game is hosted: multiplayer.gameplaySystems leaves it out on a
--- network client, whose writes to a biped the server would overwrite on its next update.
function playerHealthRegen.healthRegen()
    -- Asked once for the whole list rather than once a player: what kind of game this is does not
    -- change between two players of it.
    local isLocalGame = engine.game.getGameConnectionType() == "local"
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        if player then
            healthRegeneration(player, isLocalGame)
        end
    end
end

return playerHealthRegen
