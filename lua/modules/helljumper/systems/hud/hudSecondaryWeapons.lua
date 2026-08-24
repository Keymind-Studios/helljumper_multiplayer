-- Lua libraries
local balltze = Balltze
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local path = require "helljumper.systems.constants.paths"
local colors = require "helljumper.systems.constants.colors"
local core = require "helljumper.systems.core.core"

local secondaryWeapons = {}

--------------------------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------------------------

---@class SecondaryWeaponIconSettings
---@field staticElementIndex integer|nil
---@field anchor HudInterfaceAnchor|nil
---@field position {x: integer, y: integer}|nil
---@field scale {width: number, height: number}|nil
---@field color {a: integer, r: integer, g: integer, b: integer}|nil
---@field stateAttachedTo WeaponHudInterfaceStaticElementStateAttachedTo|nil
---@field flags HudDrawFlags|nil

local defaultStaticElementIndex = 3

---@type SecondaryWeaponIconSettings
local defaultIcon = {
    staticElementIndex = defaultStaticElementIndex,
    anchor = "topRight",
    position = {x = 164, y = 25},
    scale = {width = 0.45, height = 0.45},
    flags = {flashing = false, disabled = false, inMultiplayer = false},
    -- Every icon, not the one weapon it was tried on. Written into the HUD tag rather than drawn, so
    -- it is the palette's own form and gets packed at the moment of the write.
    color = colors.palette.secondaryWeaponIcon
}

---@type table<string, SecondaryWeaponIconSettings>
local weaponIcons = {
    [path.weapon.covenant.fusionCoil] = {
        staticElementIndex = 1
    },
    [path.weapon.human.assaultRifleMa38] = {
        staticElementIndex = 4,
        scale = {width = 0.125, height = 0.125}
    }
}

-- The table above as the loaded map has it, by the value of each weapon's tag handle, so the tick
-- reaches it with the handle the weapon already carries instead of a path read out of the tag.
---@type table<integer, SecondaryWeaponIconSettings>
local weaponIconsByTag = {}

-- What the count beside the icon reads as with rounds to count, and what it says instead on a weapon
-- that feeds off a battery.
local ammoFormat = "%d"
local batteryFormat = "%d%%"

--- The one text this module draws, so its string is written into it rather than picked out of a
--- table. It is the same text throughout, saying a number that keeps changing, which is what
--- core.setText rewrites in place instead of taking away and putting back.
local ammoText = {
    text = "",
    position = {x = 195, y = 45},
    fontPath = path.vectorFont.ui.hud.adsSmall,
    justification = "right",
    anchor = "topRight"
}

local shownAmmoText = {isUp = false}

--------------------------------------------------------------------------------------------------
-- End of configuration
--------------------------------------------------------------------------------------------------

--- What this module wrote into one HUD tag, kept so the tag can be put back exactly as authored
---@class SecondaryIconOriginalValues
---@field hudTagPath string @looked the tag up again by path, never held as a view across ticks
---@field elementIndex integer @counted from zero, the way the settings name it
---@field anchorOffsetX integer
---@field anchorOffsetY integer
---@field scaleWidth number
---@field scaleHeight number
---@field color integer
---@field stateAttachedTo WeaponHudInterfaceStaticElementStateAttachedTo

--- Every HUD tag this module has written to and not yet handed back, by tag path and element.
---@type table<string, SecondaryIconOriginalValues>
local writtenElements = {}

--- The icon on screen right now, if there is one
---@class ShownSecondaryIcon
---@field element HudStaticElement|nil @nil on builds where addHudStaticElement hands nothing back
---@type ShownSecondaryIcon|nil
local shownIcon = nil

-- The weapon the icon on screen was worked out for, as the value of its tag handle, and nil for a
-- tick that has not worked one out yet.
--
-- Kept apart from shownIcon rather than inside it because it also stands for a weapon that turned
-- out to have no icon to show: that answer is worth keeping too, or a weapon with no HUD of its own
-- would be read for one on every tick it is carried.
local shownIconWeaponTagValue = nil

--- A HUD that cannot be read, or has no element where the settings say, is worth saying out loud
--- once rather than on every tick for as long as the weapon is carried.
local warnedHudElements = {}

--- Several tables' fields laid over one another, later ones winning, so what a weapon leaves out
--- comes from the defaults
---@vararg SecondaryWeaponIconSettings|nil
---@return SecondaryWeaponIconSettings
local function resolveSettings(...)
    local resolved = {}
    for index = 1, select("#", ...) do
        local settings = select(index, ...)
        if settings then
            for field, value in pairs(settings) do
                resolved[field] = value
            end
        end
    end
    return resolved
end

--- One static element out of a HUD tag, by the index the settings name it with
---@param hudTagData WeaponHudInterface
---@param elementIndex integer @counted from zero
---@return WeaponHudInterfaceStaticElement|nil
local function getStaticElement(hudTagData, elementIndex)
    local staticElements = hudTagData.staticElements
    if not staticElements then
        return nil
    end
    return staticElements[elementIndex + 1]
end

--- Which corner the icon is measured inward from
---@param settings SecondaryWeaponIconSettings
---@param hudTagData WeaponHudInterface
---@param hudElement WeaponHudInterfaceStaticElement
---@return HudInterfaceAnchor
local function getIconAnchor(settings, hudTagData, hudElement)
    if settings.anchor then
        return settings.anchor
    end
    local anchor = hudElement.anchor
    if not anchor or anchor == "fromParent" then
        return hudTagData.anchor
    end
    ---@cast anchor HudInterfaceAnchor
    return anchor
end

--- Write the icon's placement into a HUD tag, keeping what was there first
---@param claim string @tag path and element index in one string
---@param hudTagPath string
---@param elementIndex integer
---@param hudElement WeaponHudInterfaceStaticElement
---@param settings SecondaryWeaponIconSettings
local function writeElement(claim, hudTagPath, elementIndex, hudElement, settings)
    local definition = hudElement.staticElement
    local anchorOffset = definition.anchorOffset
    local scale = definition.scale
    local colorParameters = definition.color.parameters
    if not writtenElements[claim] then
        writtenElements[claim] = {
            hudTagPath = hudTagPath,
            elementIndex = elementIndex,
            anchorOffsetX = anchorOffset.x,
            anchorOffsetY = anchorOffset.y,
            scaleWidth = scale.i,
            scaleHeight = scale.j,
            color = colorParameters.defaultColor,
            stateAttachedTo = hudElement.stateAttachedTo
        }
    end
    local position = settings.position
    if position then
        anchorOffset.x = position.x
        anchorOffset.y = position.y
    end
    local iconScale = settings.scale
    if iconScale then
        if iconScale.width then
            scale.i = iconScale.width
        end
        if iconScale.height then
            scale.j = iconScale.height
        end
    end
    if settings.color then
        colorParameters.defaultColor = colors.pack(settings.color)
    end
    if settings.stateAttachedTo then
        hudElement.stateAttachedTo = settings.stateAttachedTo
    end
end

--- Put one HUD tag back the way it was authored
---@param originalValues SecondaryIconOriginalValues
local function restoreElement(originalValues)
    local hudTagData = core.getWeaponHudInterfaceTagData(originalValues.hudTagPath)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    local hudElement = getStaticElement(hudTagData, originalValues.elementIndex)
    if not hudElement then
        return
    end
    local definition = hudElement.staticElement
    definition.anchorOffset.x = originalValues.anchorOffsetX
    definition.anchorOffset.y = originalValues.anchorOffsetY
    definition.scale.i = originalValues.scaleWidth
    definition.scale.j = originalValues.scaleHeight
    definition.color.parameters.defaultColor = originalValues.color
    hudElement.stateAttachedTo = originalValues.stateAttachedTo
end

--- Put every HUD tag this module has written to back the way it was authored
local function restoreWrittenElements()
    for claim, originalValues in pairs(writtenElements) do
        local isRestored, failure = pcall(restoreElement, originalValues)
        if isRestored then
            writtenElements[claim] = nil
        else
            balltze.logger.debug("Could not restore weapon HUD element {}, keeping it to retry: {}",
                                 claim, failure)
        end
    end
end

--- Take the icon off the screen and hand back every HUD tag written for one
local function removeIcon()
    shownIconWeaponTagValue = nil
    if shownIcon then
        local element = shownIcon.element
        shownIcon = nil
        if element and element.remove then
            pcall(element.remove, element)
        end
    end
    restoreWrittenElements()
end

--- Say what the weapon the player would swap to has left, or take the number away
---@param weaponObject WeaponObject|nil @nil when there is nothing to swap to
local function setAmmoText(weaponObject)
    -- Outside the icon's guard on purpose, and so read on every tick: the icon is the same picture
    -- for as long as the same weapon is in the next slot, but the number beside it is what changes
    -- while that weapon sits there being reloaded and fired. core.setText is what keeps that cheap,
    -- rewriting the string in place when only the count moved.
    if not weaponObject then
        -- Nothing to swap to: a dead player, one carrying the single weapon, or watching someone
        -- else. Leaving the number up would have it outlive the weapon it was counted off.
        core.removeText(shownAmmoText)
        return
    end
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    if not weaponTagData then
        core.removeText(shownAmmoText)
        return
    end
    local totalAmmo = core.getWeaponTotalAmmo(weaponObject, weaponTagData)
    if totalAmmo then
        ammoText.text = ammoFormat:format(totalAmmo)
    else
        ammoText.text = batteryFormat:format(core.getBatteryPercent(weaponObject))
    end
    core.setText(shownAmmoText, ammoText, colors.interface.gearText)
end

--- Show the icon of the weapon the player would swap to next, and what it has left
---
--- The two are one function because they are one question asked once: which weapon is next. It used
--- to be asked twice a tick, here for the icon and again in hudExtensions for the number, which put
--- the two an accident apart from ever disagreeing about what the player would swap to.
function secondaryWeapons.showSecondaryWeapons()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    local weaponObject = biped and core.getNextWeapon(biped, biped.currentWeaponId)
    setAmmoText(weaponObject)
    if not weaponObject then
        removeIcon()
        return
    end
    -- Which icon this is, is a question about the weapon's tag and nothing else: the HUD it comes
    -- out of and the element of it are both read off that tag, and so are the settings. So the tag
    -- handle the weapon already carries is the whole of what says "still the same icon", and asking
    -- it costs one integer compare. Everything below this line used to run on every tick to build
    -- the two strings that were compared instead, the tag reads behind them included.
    local weaponTagValue = weaponObject.tagHandle.value
    if weaponTagValue == shownIconWeaponTagValue then
        return
    end
    removeIcon()
    -- Set whatever comes of it, so a weapon that turns out to have no icon is worked out once too
    shownIconWeaponTagValue = weaponTagValue
    local hudTagPath = core.getWeaponHudTagPath(weaponObject)
    if not hudTagPath then
        return
    end
    local settings = resolveSettings(defaultIcon, weaponIconsByTag[weaponTagValue])
    local elementIndex = settings.staticElementIndex or defaultStaticElementIndex
    local claim = hudTagPath .. "|" .. elementIndex
    local hudTagData = core.getWeaponHudInterfaceTagData(hudTagPath)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    local hudElement = getStaticElement(hudTagData, elementIndex)
    if not hudElement then
        if not warnedHudElements[claim] then
            warnedHudElements[claim] = true
            balltze.logger.warning("Weapon HUD has no static element {}: {}", elementIndex,
                                   hudTagPath)
        end
        return
    end
    local anchor = getIconAnchor(settings, hudTagData, hudElement)
    writeElement(claim, hudTagPath, elementIndex, hudElement, settings)
    shownIcon = {
        element = engine.interface.addHudStaticElement(anchor, hudElement.staticElement,
                                                       settings.flags)
    }
end

--- Work out which of the loaded map's weapons the settings above are about
---
--- Once a map, since a tag handle is only good for as long as the map it was read out of. Called
--- after tags.get(), the way every load in this project is.
function secondaryWeapons.load()
    weaponIconsByTag = core.resolveTagKeys(weaponIcons, "weapon")
end

function secondaryWeapons.unload()
    removeIcon()
    core.removeText(shownAmmoText)
    warnedHudElements = {}
    -- Let go of rather than carried into the next map: the keys in it are this map's handles.
    weaponIconsByTag = {}
end

return secondaryWeapons
