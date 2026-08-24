-- Lua libraries
local balltze = Balltze
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local weapons = require "helljumper.systems.constants.tags"

local weaponExtensions = {}

-- The plasma caster spends ammunition, but its charged shot rides on the heat and battery
-- machinery of a plasma weapon. Nothing drains a battery on an ammunition weapon, so its
-- charge stays full forever and the charge animation keeps playing on an empty magazine.
-- Draining it along with the ammunition is what stops that.
--
-- Age is not a battery gauge, it runs the other way: 0 is a weapon that has not been used at
-- all, 1 is a spent one, and the battery the HUD and the animations read is what is left of
-- it. If it turns out to be gated the opposite way, swap these two values, nothing else.
local ageWhenEmpty = 1
local ageWhenLoaded = 0

-- Bipeds carry four weapons.
local weaponSlotCount = 4

--- Drain the age of every plasma caster a player carries that has run out of ammunition, and
--- give it back to any that has been picked up or resupplied
---@param player Player
local function syncPlayerAgeWithAmmo(player)
    local casterTag = weapons.weapon.plasmaCaster
    if not casterTag then
        return
    end
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    for weaponSlot = 1, weaponSlotCount do
        local weaponHandle = biped.weapons[weaponSlot]
        -- Passed over rather than returned on, the same way the player loop passes over an empty
        -- slot: a weapon that cannot be read would otherwise cut the loop short and leave the slots
        -- after it untouched.
        if weaponHandle and not weaponHandle:isNull() then
            local weapon = getObject(weaponHandle, "weapon")
            -- v1 walked every weapon of every player and aged them all; only the caster needs
            -- this, and ageing anything else would wear weapons the engine never wears.
            --
            -- Asked of the handle the weapon already carries. Reading it back off a tag entry, which
            -- is what this used to do, means looking the entry up by that same handle and taking the
            -- handle out of it again.
            if weapon and weapon.tagHandle.value == casterTag.value then
                local magazine = weapon.magazines[1]
                if magazine then
                    local isOutOfAmmo = magazine.roundsLoaded == 0 and magazine.roundsUnloaded == 0
                    local age = isOutOfAmmo and ageWhenEmpty or ageWhenLoaded
                    if weapon.age ~= age then
                        -- The value it is coming from is the interesting half: a full caster
                        -- resting at 0 confirms which end of the range means spent.
                        balltze.logger.debug("Plasma Caster | Age: {} | Age Used: {} | Ammo Loaded: {} | Ammo In Reserve: {}", weapon.age,
                            age, magazine.roundsLoaded, magazine.roundsUnloaded)
                        weapon.age = age
                    end
                end
            end
        end
    end
end

function weaponExtensions.syncWeaponAge()
    for playerIndex = 0, 15 do
        local player = getPlayer(playerIndex)
        -- Skipped, not returned on: an empty slot in the middle of the list would otherwise
        -- cut the loop short and leave the players after it untouched.
        if player then
            syncPlayerAgeWithAmmo(player)
        end
    end
end

return weaponExtensions
