-- Lua libraries
local engine = Engine
local balltze = Balltze
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local hsc = require "hsc"
local path = require "helljumper.systems.constants.paths"
local tags = require "helljumper.systems.constants.tags"
local core = require "helljumper.systems.core.core"

local hudExtensions = {state = {playerCriticalHealth = false}}

--- Step everything the HUD adds on top of the game's own, once per tick
---
--- Every one of the six only does work when something has changed. That is not incidental: the six
--- were measured one by one, and the radar hiding, the only one that then acted every tick
--- regardless, was on its own costing 1.290ms of the 1.323ms this function spent, which was most of
--- what lua cost in the whole project. Anything added here wants the same guard.
function hudExtensions.init()
    hudExtensions.hideElementsOnZoom()
    hudExtensions.changeGrenadeSound()
    hudExtensions.addGrenadeTypeText()
    hudExtensions.addWeaponText()
    hudExtensions.addSecondaryAmmoText()
    hudExtensions.hudBlurOnLowHealth()
end

----------------------------------------------------------------------------------------------
--- Zoom: what the HUD shows while the sights are up, and what it does not
----------------------------------------------------------------------------------------------

-- The HUDs whose meters belong to a scope rather than to the weapon, by the value of the tag handle
-- of the weapon they hang off. Filled by hudExtensions.load and emptied when the map goes, because
-- a handle is only good for as long as the map it was read out of.
---@type table<integer, TagHandle[]>
local zoomedMeterHuds = {}

--- Say that a weapon's scope takes some meters off the screen with it
---
--- Name a HUD here only if every meter in it belongs to the scope: what goes is the block, not a
--- meter of it. The sniper's are a child HUD of their own, which is what makes that true of them.
---@param weaponTag TagHandle|nil
---@param hudTags TagHandle[]
local function addZoomedMeterHuds(weaponTag, hudTags)
    -- A map carrying neither the weapon nor its HUDs is one there is nothing to do in rather than
    -- anything to warn about: not every map holds every weapon.
    if weaponTag and hudTags[1] then
        zoomedMeterHuds[weaponTag.value] = hudTags
    end
end

--- Work out which of the loaded map's HUDs answer to a scope
---
--- Once a map, off constants.tags, so that the tick has a handle in hand and never a path to look
--- one up by. Called after tags.get(), which is what puts the handles there in the first place.
function hudExtensions.load()
    zoomedMeterHuds = {}
    local weaponTags = tags.weapon
    local childHudTags = tags.weaponHudInterface.child
    -- One line per weapon: what is in hand, and the HUDs whose meters go away while its scope is
    -- down. Nothing else changes when a weapon is added.
    addZoomedMeterHuds(weaponTags.sniper, {childHudTags.sniperRifleExtMeters})
end

-- Shrunk away rather than moved: the offsets hudDynamicCrosshair animates are left alone, and
-- nothing has to be put back when the scope comes up, since nothing was moved.
-- Not zero: a scale of nothing is the sort of value a tag field is read as unset at.
local hiddenMeterScale = 0.001

--- Shrink every meter element of a HUD away
---@param hudTag TagHandle
local function hideMeterElements(hudTag)
    -- The data and not the handle is what dangles on a map change, so this is asked for on each
    -- write while the handle it is asked with was worked out once, at load.
    local hudTagData = engine.tag.getTagData(hudTag, "weapon_hud_interface")
    ---@cast hudTagData WeaponHudInterface
    if not hudTagData then
        return
    end
    -- An empty tag block comes back as nil rather than as an array of length zero, so a HUD with no
    -- meters at all is caught here rather than indexed into.
    local meterElements = hudTagData.meterElements
    if not meterElements then
        return
    end
    -- A meter keeps its two scales as loose fields, not as the Vector2d a static element has.
    for index = 1, #meterElements do
        local meterElement = meterElements[index]
        meterElement.widthScale = hiddenMeterScale
        meterElement.heightScale = hiddenMeterScale
    end
end

local isZoomedShown = nil
function hudExtensions.hideElementsOnZoom()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    if not biped then
        isZoomedShown = nil
        return
    end
    -- Read as the two levels a scope has rather than as "not unzoomed": what the engine leaves in
    -- here while nothing is zoomed is not worth depending on the sign of.
    local zoomLevel = biped.desiredZoomLevel
    local isZoomed = zoomLevel == 0 or zoomLevel == 1
    local isZoomChanged = isZoomed ~= isZoomedShown
    if isZoomChanged then
        isZoomedShown = isZoomed
        -- The one expensive thing here, and the reason for the guard: an hsc call every tick was
        -- what this module used to spend nearly all of its time on.
        hsc.hud_show_motion_sensor(not isZoomed)
    end
    if isZoomed then
        -- Nothing to write with the scope up: leaving the scales alone is what lets
        -- hudDynamicCrosshair go on animating them.
        return
    end
    -- The weapon in hand said as the value of its tag handle, which is what the table was keyed by
    -- at load. A weapon that is not in it is one with no scope meters to put away.
    local weaponObject = core.getHeldWeapon(biped)
    local hudTags = weaponObject and zoomedMeterHuds[weaponObject.tagHandle.value]
    if not hudTags then
        return
    end
    -- Written on every tick and not on the transition alone, because that module writes the same
    -- scales on every tick too and would have them back at full size by the next one. What lands is
    -- whatever wrote last, so this only works while multiplayer.gameplaySystems goes on calling
    -- hudExtensions after dynamicCrosshair. What it costs is a tag view and two writes a meter,
    -- which is not what the guard above is protecting against.
    for index = 1, #hudTags do
        hideMeterElements(hudTags[index])
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
            hsc.sound_impulse_start(path.sound.ui.hud.grenades.fragSelected, "none", 1)
        elseif currentGrenadeType == 1 then
            hsc.sound_impulse_start(path.sound.ui.hud.grenades.plasmaSelected, "none", 1)
        end
    end
end

----------------------------------------------------------------------------------------------
--- Gear Text
----------------------------------------------------------------------------------------------
local gearTextFont = path.vectorFont.ui.hud.gearText
local secondaryAmmoTextFont = path.vectorFont.ui.hud.adsSmall
local gearTextColor = {a = 1.0, r = 169 / 255, g = 198 / 255, b = 243 / 255}
local gearTextEmptyColor = {a = 120 / 255, r = 200 / 255, g = 208 / 255, b = 255 / 255}

----------------------------------------------------------------------------------------------
--- Gear Text: grenades
----------------------------------------------------------------------------------------------

local grenadeTexts = core.resolveTexts({
    [0] = {text = "FRAG GRENADE", position = {x = 26, y = 19}},
    [1] = {text = "PLASMA GRENADE", position = {x = 77, y = 19}}
},{fontPath = gearTextFont, justification = "left", anchor = "topLeft"})

local shownGrenadeText = {isUp = false}
function hudExtensions.addGrenadeTypeText()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    if not biped then
        core.removeText(shownGrenadeText)
        return
    end
    -- grenadeCounts is a two entry array over the game's grenade types and currentGrenadeIndex
    -- counts from zero, so the count of the type in hand sits one place further along.
    local count = biped.grenadeCounts[biped.currentGrenadeIndex + 1]
    local isEmpty = not count or count <= 0
    core.setText(shownGrenadeText, grenadeTexts[biped.currentGrenadeIndex],
                isEmpty and gearTextEmptyColor or gearTextColor)
end

----------------------------------------------------------------------------------------------
--- Gear Text: weapons
----------------------------------------------------------------------------------------------
local weaponTextDefaults = {
    position = {x = 25, y = 19},
    fontPath = gearTextFont,
    justification = "right",
    anchor = "topRight"
}

local weaponTexts = core.resolveTexts({
    [path.weapon.human.assaultRifleMa38] = {text = "MA38 ASSAULT RIFLE"},
    [path.weapon.human.vk78Commando] = {text = "VK78 COMMANDO"},
    [path.weapon.human.br65h] = {text = "BR65H BATTLE RIFLE"},
    [path.weapon.human.dmr392] = {text = "M392 DMR"},
    [path.weapon.human.saw] = {text = "M739 SAW"},
    [path.weapon.human.shotgunM90] = {text = "M90 SHOTGUN"},
    [path.weapon.human.smgM7] = {text = "M7 SMG"},
    [path.weapon.human.sniper] = {text = "SRS99C SNIPER RIFLE"},
    [path.weapon.covenant.plasmaCaster] = {text = "PLASMA CASTER"},
    [path.weapon.covenant.stormRifle] = {text = "STORM RIFLE"},
    [path.weapon.covenant.stalkerRifle] = {text = "STALKER RIFLE"},
    [path.weapon.human.magnumM6s] = {text = "M6S MAGNUM"},
    [path.weapon.covenant.plasmaPistol] = {text = "PLASMA PISTOL"},
    [path.weapon.covenant.needler] = {text = "T54C NEEDLER"},
    [path.weapon.covenant.disruptor] = {text = "DISRUPTOR"},
    [path.weapon.human.spnkr] = {text = "M41 SPNKR"},
    [path.weapon.covenant.skewer] = {text = "SKEWER"},
    [path.weapon.covenant.plasmaRifle] = {text = "PLASMA RIFLE"},
    [path.weapon.covenant.fusionCoil] = {text = "PLASMA COIL"}
}, weaponTextDefaults)

local shownWeaponText = {isUp = false}

--- Show the name of the weapon the player is holding, and only that one
function hudExtensions.addWeaponText()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    local weaponObject = biped and core.getHeldWeapon(biped)
    if not weaponObject then
        -- Nothing in hand to name, on a dead player, mid swap, or watching someone else. Leaving the
        -- name up would have it outlive the weapon it belongs to.
        core.removeText(shownWeaponText)
        return
    end
    -- Read off the tag entry rather than matched against the weapons constants: the path is what
    -- keys weaponTexts, and getTagEntry hands it over without walking every weapon tag in the map.
    local tagEntry = engine.tag.getTagEntry(weaponObject.tagHandle)
    local entry = tagEntry and weaponTexts[tagEntry.path]
    if not entry then
        -- A weapon this table says nothing about has no name to show for it.
        core.removeText(shownWeaponText)
        return
    end
    core.setText(shownWeaponText, entry,
                core.isWeaponEmpty(weaponObject) and gearTextEmptyColor or gearTextColor)
end

----------------------------------------------------------------------------------------------
--- Gear Text: secondary weapon ammunition
----------------------------------------------------------------------------------------------

-- What it reads as with rounds to count, and what it says instead on a weapon that has none.
local secondaryAmmoFormat = "%d"
local secondaryBatteryFormat = "%d%%"

--- The one text of its section, so its string is written into it rather than picked out of a table.
--- It is the same text throughout, saying a number that keeps changing, which is exactly what
--- core.setGearText rewrites in place instead of taking away and putting back.
local secondaryAmmoText = {
    text = "",
    position = {x = 195, y = 45},
    fontPath = secondaryAmmoTextFont,
    justification = "right",
    anchor = "topRight",
}

local shownSecondaryAmmoText = {isUp = false}

--- Show what the weapon the player would swap to has left
function hudExtensions.addSecondaryAmmoText()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    -- v2 exposes the held slot as currentWeaponId, and which weapon comes after it round the four is
    -- the swap order hudSecondaryWeapons walks for its icon.
    local weaponObject = biped and core.getNextWeapon(biped, biped.currentWeaponId)
    if not weaponObject then
        -- Nothing to swap to: a dead player, one carrying the single weapon, or watching someone
        -- else. Leaving the number up would have it outlive the weapon it was counted off.
        core.removeText(shownSecondaryAmmoText)
        return
    end
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    if not weaponTagData then
        core.removeText(shownSecondaryAmmoText)
        return
    end
    local totalAmmo = core.getWeaponTotalAmmo(weaponObject, weaponTagData)
    if totalAmmo then
        secondaryAmmoText.text = secondaryAmmoFormat:format(totalAmmo)
    else
        secondaryAmmoText.text = secondaryBatteryFormat:format(core.getBatteryPercent(weaponObject))
    end
    core.setText(shownSecondaryAmmoText, secondaryAmmoText, gearTextColor)
end

function hudExtensions.unload()
    core.removeText(shownGrenadeText)
    core.removeText(shownWeaponText)
    core.removeText(shownSecondaryAmmoText)
    lastGrenadeType = nil
    isZoomedShown = nil
    -- Let go of rather than carried into the next map: the handles in it belong to this one.
    zoomedMeterHuds = {}
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
