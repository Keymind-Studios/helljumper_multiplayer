-- Lua libraries
local balltze = Balltze
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local path = require "helljumper.systems.constants.paths"
local core = require "helljumper.systems.core.core"

local secondaryWeaponIcons = {}

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
    --color = {a = 120, r = 84, g = 99, b = 122}
}

---@type table<string, SecondaryWeaponIconSettings>
local weaponIcons = {
    [path.weapon.covenant.fusionCoil] = {
        staticElementIndex = 1
    },
    [path.weapon.human.assaultRifleMa38] = {
        staticElementIndex = 4,
        scale = {width = 0.125, height = 0.125},
        color = {a = 120, r = 84, g = 99, b = 122}
    }
}

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
---@field key string @weapon, HUD tag and element in one string; what tells "still the same icon"
---@field element HudStaticElement|nil @nil on builds where addHudStaticElement hands nothing back
---@type ShownSecondaryIcon|nil
local shownIcon = nil

--- A HUD that cannot be read, or has no element where the settings say, is worth saying out loud
--- once rather than on every tick for as long as the weapon is carried.
local warnedHudElements = {}

--- The four channels Guerilla shows, as the one number the tag actually keeps them in
---@param color {a: integer, r: integer, g: integer, b: integer}
---@return integer
local function packColor(color)
    -- Arithmetic rather than shifts, so this does not depend on which Lua it is running under: the
    -- server side of this project still goes through compat53.
    return color.a * 0x1000000 + color.r * 0x10000 + color.g * 0x100 + color.b
end

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
    local colors = definition.color.parameters
    if not writtenElements[claim] then
        writtenElements[claim] = {
            hudTagPath = hudTagPath,
            elementIndex = elementIndex,
            anchorOffsetX = anchorOffset.x,
            anchorOffsetY = anchorOffset.y,
            scaleWidth = scale.i,
            scaleHeight = scale.j,
            color = colors.defaultColor,
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
        colors.defaultColor = packColor(settings.color)
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
    if shownIcon then
        local element = shownIcon.element
        shownIcon = nil
        if element and element.remove then
            pcall(element.remove, element)
        end
    end
    restoreWrittenElements()
end

--- Show the icon of the weapon the player would swap to next
function secondaryWeaponIcons.showSecondaryWeaponIcons()
    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    local weaponObject = biped and core.getNextWeapon(biped, biped.currentWeaponId)
    if not weaponObject then
        removeIcon()
        return
    end
    local weaponTagEntry = engine.tag.getTagEntry(weaponObject.tagHandle)
    local hudTagPath = weaponTagEntry and core.getWeaponHudTagPath(weaponObject)
    if not weaponTagEntry or not hudTagPath then
        removeIcon()
        return
    end
    local settings = resolveSettings(defaultIcon, weaponIcons[weaponTagEntry.path])
    local elementIndex = settings.staticElementIndex or defaultStaticElementIndex
    local claim = hudTagPath .. "|" .. elementIndex
    local key = weaponTagEntry.path .. "|" .. claim
    if shownIcon and shownIcon.key == key then
        return
    end
    removeIcon()
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
        key = key,
        element = engine.interface.addHudStaticElement(anchor, hudElement.staticElement,
                                                       settings.flags)
    }
end

function secondaryWeaponIcons.unload()
    removeIcon()
    warnedHudElements = {}
end

return secondaryWeaponIcons
