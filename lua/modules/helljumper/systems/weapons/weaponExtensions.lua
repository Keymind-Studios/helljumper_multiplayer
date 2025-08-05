local engine = Engine
local balltze = Balltze
local objectTypes = Engine.tag.objectType
local tagClasses = Engine.tag.classes
local getTag = Engine.tag.getTag
local getObject = Engine.gameState.getObject
local getPlayer = Engine.gameState.getPlayer
local constants = require "helljumper.systems.constants.constants"

local weaponExtensions = {}

function weaponExtensions.casterFixHeat()
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        if player then
            local biped = getObject(player.objectHandle.value, engine.tag.objectType.biped)
            --local weaponObj = getObject(objectHandle.value, engine.tag.objectType.weapon)
            if biped then
                for weaponIndex = 1, 4 do
                    local weapon = getObject(biped.weapons[weaponIndex], objectTypes.weapon)
                    --local weaponTag = constants.weapons.plasmaCasterTag.handle
                    if weapon then
                        if weaponIndex and weapon.magazines[1].roundsLoaded == 0 and weapon.magazines[1].roundsUnloaded == 0 then
                            --local casterTag = 
                            --local casterDataTag = 
                            weapon.age = 1
                            --weapon.heat = 1
                        else
                            weapon.age = 0
                        end
                        --logger:info("Weapon: {} Ammo: {} Mag: {} Age: {}", weaponIndex, weapon.magazines[1].roundsLoaded, weapon.magazines[1].roundsUnloaded, weapon.age)
                    end
                end
            end
        end
    end
end

return weaponExtensions