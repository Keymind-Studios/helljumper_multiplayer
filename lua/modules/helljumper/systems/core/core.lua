local engine = Engine
local balltze = Balltze
local hsc = require "hsc"
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer

local core = {}

---@class TextEntry
---@field text string
---@field position {x: integer, y: integer}
---@field justification "left"|"right"|"center"
---@field anchor "topLeft"|"topRight"|"bottomLeft"|"bottomRight"|"center"
---@field fontPath string
---@generic K
---@param entries table<K, TextEntry>
---@param defaults table
---@return table<K, TextEntry>
function core.resolveTexts(entries, defaults)
    for _, entry in pairs(entries) do
        entry.position = entry.position or defaults.position
        entry.fontPath = entry.fontPath or defaults.fontPath
        entry.justification = entry.justification or defaults.justification
        entry.anchor = entry.anchor or defaults.anchor
    end
    return entries
end

---@class ShownText
---@field handle InterfaceText|nil
---@field isUp boolean
---@field entry TextEntry|nil
---@field text string|nil
---@field color table|nil
---@param shown ShownText
function core.removeText(shown)
    if shown.handle and shown.handle.remove then
        shown.handle:remove()
    end
    shown.handle = nil
    shown.isUp = false
    shown.entry = nil
    shown.text = nil
    shown.color = nil
end

--- A font that cannot be found is worth saying out loud once, not every time a text asks for it.
local warnedFonts = {}

--- Put one text on screen in the colour asked for, taking away whatever was up before it
---@param shown ShownText
---@param entry TextEntry|nil @nil leaves the HUD clear
---@param color table
function core.setText(shown, entry, color)
    if not entry then
        core.removeText(shown)
        return
    end
    if shown.isUp and shown.entry == entry and shown.color == color then
        if shown.text == entry.text then
            return
        end
        if shown.handle and shown.handle.setText then
            shown.handle:setText(entry.text)
            shown.text = entry.text
            return
        end
    end
    core.removeText(shown)
    local font = engine.tag.lookupTag(entry.fontPath, "vector_font")
    if not font and not warnedFonts[entry.fontPath] then
        warnedFonts[entry.fontPath] = true
        balltze.logger.warning("Text font does not exist: {}", entry.fontPath)
    end
    local options = {
        color = color,
        font = font,
        shadow = false,
        layer = "hud",
        anchor = entry.anchor,
        justification = entry.justification
    }
    shown.handle = engine.interface.addText(entry.text, entry.position.x, entry.position.y, options)
    shown.isUp = true
    shown.entry = entry
    shown.text = entry.text
    shown.color = color
end

--- The weapon a biped is holding, if it is holding one
---@param biped BipedObject
---@return WeaponObject|nil
function core.getHeldWeapon(biped)
    local weaponHandle = biped.weapons[biped.currentWeaponId + 1]
    if not weaponHandle or weaponHandle:isNull() then
        return nil
    end
    return getObject(weaponHandle, "weapon")
end

--- The weapon the player would swap to from the one in hand
---@param biped BipedObject
---@param heldSlot integer @counted from zero, the way currentWeaponId reports it
---@return WeaponObject|nil
function core.getNextWeapon(biped, heldSlot)
    local weaponSlotCount = 4
    for step = 1, weaponSlotCount - 1 do
        local slot = (heldSlot + step) % weaponSlotCount
        local weaponHandle = biped.weapons[slot + 1]
        if weaponHandle and not weaponHandle:isNull() then
            local weaponObject = getObject(weaponHandle, "weapon")
            if weaponObject then
                return weaponObject
            end
        end
    end
    return nil
end

--- The magazine a weapon feeds from, or nil when it runs off a battery instead
---@param weaponObject WeaponObject
---@param weaponTagData Weapon
---@return WeaponObjectMagazine|nil
function core.getWeaponMagazine(weaponObject, weaponTagData)
    local magazines = weaponTagData.magazines
    local tagMagazine = magazines and magazines[1]
    if not tagMagazine or tagMagazine.roundsLoadedMaximum <= 0 then
        return nil
    end
    return weaponObject.magazines[1]
end

--- Whether the weapon in hand has nothing left to it
---@param weaponObject WeaponObject
---@return boolean
function core.isWeaponEmpty(weaponObject)
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    if not weaponTagData then
        return false
    end
    local magazine = core.getWeaponMagazine(weaponObject, weaponTagData)
    if magazine then
        return magazine.roundsUnloaded <= 0
    end
    return weaponObject.age >= 1
end

--- Every round a weapon has, in its magazines and in the reserve waiting to be loaded into them
---@param weaponObject WeaponObject
---@param weaponTagData Weapon
---@return integer|nil @nil on a weapon that runs off a battery rather than magazines
function core.getWeaponTotalAmmo(weaponObject, weaponTagData)
    local tagMagazines = weaponTagData.magazines or {}
    local total = nil
    for index = 1, #tagMagazines do
        local magazine = tagMagazines[index].roundsLoadedMaximum > 0 and
                             weaponObject.magazines[index]
        if magazine then
            total = (total or 0) + magazine.roundsLoaded + magazine.roundsUnloaded
        end
    end
    return total
end

--- What is left of a battery, as the percentage the game's own HUD reads it at
---@param weaponObject WeaponObject
---@return integer
function core.getBatteryPercent(weaponObject)
    local percent = math.floor((1 - weaponObject.age) * 100)
    if percent > 100 then
        return 100
    elseif percent < 0 then
        return 0
    end
    return percent
end

--- A weapon's own HUD tag, by path
---@param weaponObject WeaponObject
---@return string|nil
function core.getWeaponHudTagPath(weaponObject)
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    if not weaponTagData then
        return nil
    end
    local hudInterface = weaponTagData.hudInterface
    -- A weapon with no HUD of its own has nothing to take an icon out of. Its reference still
    -- carries a path field, so the handle is what says whether anything is actually there.
    if not hudInterface or hudInterface.tagHandle:isNull() then
        return nil
    end
    return hudInterface.path
end

-- Get the data of a weapon hud interface
---@param hudTagPath string
---@return WeaponHudInterface|nil
function core.getWeaponHudInterfaceTagData(hudTagPath)
    local tagHandle = engine.tag.lookupTag(hudTagPath, "weapon_hud_interface")
    if not tagHandle then
        return nil
    end
    return engine.tag.getTagData(tagHandle, "weapon_hud_interface")
end

--- The cutscene flag a waypoint anchors to, found by the name the scenario gave it
---
--- By name and not by position in the block: which flag is the third one in a scenario is the map
--- author's business, and matching on the name is what lets this module borrow four of them out of a
--- list that has anything else in it too.
---@param flagName string
---@return ScenarioCutsceneFlags|nil
function core.findCutsceneFlag(flagName)
    -- Looked up on each ping instead of held as an upvalue: the scenario belongs to the loaded map,
    -- so a cached view would dangle the moment the map changes. A ping is a keypress and not a tick,
    -- so walking the block costs nothing worth saving.
    local scenarioEntries = engine.tag.filterTags("scenario", "")
    local scenarioEntry = scenarioEntries and scenarioEntries[1]
    if not scenarioEntry then
        return nil
    end
    local scenarioTagData = engine.tag.getTagData(scenarioEntry.handle, "scenario")
    ---@cast scenarioTagData Scenario
    -- An empty tag block comes back as nil rather than as an array of length zero, so a scenario
    -- with no cutscene flags at all is caught here rather than indexed into.
    local cutsceneFlags = scenarioTagData and scenarioTagData.cutsceneFlags
    if not cutsceneFlags then
        return nil
    end
    for index = 1, #cutsceneFlags do
        local cutsceneFlag = cutsceneFlags[index]
        if cutsceneFlag.name == flagName then
            return cutsceneFlag
        end
    end
    return nil
end

--- Where the player is looking from, which is not where their biped is
---@param biped BipedObject
---@return {x: number, y: number, z: number}|nil
function core.getCameraOrigin(biped)
    local bipedTagData = engine.tag.getTagData(biped.tagHandle, "biped")
    ---@cast bipedTagData Biped
    if not bipedTagData then
        return nil
    end
    -- An object's position is at the biped's feet, so the eye is a camera height above it. Read off
    -- the tag rather than assumed the way v1's flat 0.54 was, and walked between the standing and
    -- the crouching height by crouchScale, which runs 0 standing to 1 crouched: a ping made while
    -- crouching leaves from where the player is actually looking rather than from over their head.
    local standingHeight = bipedTagData.standingCameraHeight
    local crouchingHeight = bipedTagData.crouchingCameraHeight
    local crouchScale = biped.crouchScale
    if crouchScale < 0 then
        crouchScale = 0
    elseif crouchScale > 1 then
        crouchScale = 1
    end
    local cameraHeight = standingHeight + (crouchingHeight - standingHeight) * crouchScale
    local position = biped.position
    return {x = position.x, y = position.y, z = position.z + cameraHeight}
end

--- Whether the ray came back off an object rather than off the level itself
---
--- Asked of the type and not of the handle beside it. A hit against the level is meant to carry no
--- handle at all, but what it does carry cannot be relied on to be nothing: a handle left over in
--- the result points at whatever object happens to live at that slot, and taking it at face value is
--- what puts a biped's or a vehicle's arrow on a wall, a different one each time.
---
--- The type is read as text because the same word arrives in two shapes: a plain string on a build
--- that pushes the enum as one, and an engine enum value that prints as "object(3)" on a build that
--- does not. Anything else falls to the level's own arrow, which is the safe way round: a weapon
--- pinged as an objective still reads as a ping, an arbitrary arrow does not.
---@param collision CollisionResult
---@return boolean
function core.isObjectCollision(collision)
    local resultType = collision.type
    if resultType == nil then
        return false
    end
    return tostring(resultType):find("object", 1, true) == 1
end

---@param modelAnimations ModelAnimations
function core.getModelAnimations(modelAnimations)
    if not modelAnimations then
        return nil
    end
    return modelAnimations
end

return core