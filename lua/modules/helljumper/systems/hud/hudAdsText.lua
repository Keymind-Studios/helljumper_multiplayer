-- Lua libraries
local balltze = Balltze
local engine = Engine
local path = require "helljumper.systems.constants.objectPaths"

local adsHudText = {}

local floor = math.floor

---@class AdsHudReadoutSettings
---@field text string|nil @what it says, for a readout whose string is fixed rather than read off the weapon
---@field format string|nil @formats the number a readout reads off the weapon
---@field batteryFormat string|nil @what the ammunition readout says on a weapon that runs off a battery
---@field position {x: integer, y: integer}|nil @where it sits, measured from the middle of the screen
---@field justification "left"|"right"|"center"|nil @which part of the readout lands on that position
---@field fontPath string|nil @vector_font tag it is drawn with
---@field color {a: number, r: number, g: number, b: number}|nil @what colour it is drawn in, 0 to 1 per channel
---@field delayTicks integer|nil @ticks it waits after the aim is taken before coming up

---@class AdsWeaponTextSettings
---@field zoom AdsHudReadoutSettings|nil @the magnification, on the left; nothing to read it off, so it is written out
---@field ammo AdsHudReadoutSettings|nil @the rounds loaded, or the charge left on a battery weapon
---@field reserve AdsHudReadoutSettings|nil @the rounds left to reload from; a battery weapon has none

-- The readouts that come up beside the reticle while aiming, keyed by weapon tag path the same
-- way hudDynamicCrosshair keys its crosshair animations. A weapon that is not listed here aims
-- without any text of its own, and a weapon only spells out what it wants done differently:
-- everything else falls back to the defaults below.
--
-- Nothing here says which readout a weapon gets on the right: a weapon that feeds from magazines
-- shows what is loaded and what is left in reserve, one that runs off a battery shows what is
-- left of the charge and no reserve at all, and which of the two it is comes off the weapon tag
-- rather than out of this table.
---@type table<string, AdsWeaponTextSettings>
local adsWeaponTexts = {
    -- AssaultRifleMA38
    [path.weapons.ma38_weap] = {
        zoom = {text = "1.40x"}
    }
}

-- What a readout is drawn with unless it names a font of its own. A vector_font tag carries its
-- own size, so telling the readouts apart the way the reference shot does, big rounds against a
-- small reserve and zoom label, is a matter of pointing them at different font tags.
local defaultFontPath = "keymind\\helljumper\\ui\\fonts\\small_ui"
local ultraSmallFontPath = "keymind\\helljumper\\ui\\fonts\\ultra_small_ui"
-- And the colour they are drawn in, 0 to 1 per channel. Held apart from the readouts below so a
-- readout that wants its own, a reserve dimmed against the rounds loaded or a battery counter
-- that goes red, only has to say so.
local _defaultTextColor = {a = 1.0, r = 188/255, g = 243/255, b = 255/255}
local defaultTextColor = {a = 1.0, r = 135/255, g = 196/255, b = 255/255}
local whiteTextColor = {a = 1.0, r = 1.0, g = 1.0, b = 1.0}


-- What a weapon does not spell out for itself, per readout. The positions are measured from the
-- middle of the screen, where the reticle is, x running right and y running down, so the left
-- hand readout sits at a negative x; flip the signs here if a readout lands on the wrong side.
--
-- The reserve has to be nudged clear of the rounds loaded by hand: addText has no way to say how
-- wide a string came out, so the gap between the two is a number here and not a measurement.
--
-- The delay is what keeps the readouts from beating the reticle onto the screen. Ticks run at 30
-- a second, and hudAdsElements opens the reticle at 3 of them.
---@type table<string, AdsHudReadoutSettings>
local defaultReadouts = {
    zoom = {
        position = {x = -135, y = -7},
        justification = "center",
        fontPath = defaultFontPath,
        color = defaultTextColor,
        delayTicks = 7
    },
    ammo = {
        format = "%d",
        batteryFormat = "%d/%%",
        position = {x = -295, y = -7},
        justification = "right",
        fontPath = defaultFontPath,
        color = defaultTextColor,
        delayTicks = 7
    },
    reserve = {
        format = "/ %d",
        position = {x = 555, y = -6},
        justification = "left",
        fontPath = ultraSmallFontPath,
        color = defaultTextColor,
        delayTicks = 7
    }
}

-- The order they are walked in. Named here so adding a fourth readout is a line in the defaults
-- and a line here, rather than another branch everywhere a readout is touched.
local readoutKeys = {"zoom", "ammo", "reserve"}

--- Options for a readout. Built per text rather than kept around: they are fixed at creation
--- time anyway, and the font handle belongs to the loaded map, so it is looked up on the spot.
---@param readout AdsHudReadoutSettings
---@return HudTextOptions
local function getTextOptions(readout)
    local fontPath = readout.fontPath or defaultFontPath
    -- A font that cannot be found is not worth dropping the readout over: addText falls back to
    -- the globals terminal font on a nil handle, so a wrong path costs the look and nothing
    -- else. Worth saying out loud though, since that fallback is easy to mistake for the tag
    -- simply looking wrong.
    local font = engine.tag.lookupTag(fontPath, "vector_font")
    if not font then
        balltze.logger.warning("ADS HUD readout font does not exist: {}", fontPath)
    end
    return {
        color = readout.color or defaultTextColor,
        font = font,
        -- The HUD layer draws these once per local player's HUD pane and keeps them out of
        -- menus, which is where a readout that belongs beside the reticle wants to be.
        layer = "hud",
        anchor = "center",
        justification = readout.justification
    }
end

---@class AdsHudTextSlot
---@field handle HudText|nil @nil even while shown on builds where addText hands nothing back
---@field isShown boolean @tracked apart from the handle, for that same reason
---@field text string|nil @what it currently reads

-- One slot per readout, holding what is on screen for it right now.
---@type table<string, AdsHudTextSlot>
local slots = {}
for index = 1, #readoutKeys do
    slots[readoutKeys[index]] = {isShown = false}
end

-- The weapon whose readouts are up right now, its readouts with every default already filled in,
-- and when its aim was taken.
---@type string|nil
local shownWeaponPath = nil
---@type table<string, AdsHudReadoutSettings>|nil
local shownReadouts = nil
local shownWeaponStartTick = 0

--- Take a readout off the screen
---@param slot AdsHudTextSlot
local function removeSlotText(slot)
    if slot.handle and slot.handle.remove then
        slot.handle:remove()
    end
    slot.handle = nil
    slot.isShown = false
    slot.text = nil
end

--- Put a readout on screen, move it along to what it should say now, or take it away
---@param slot AdsHudTextSlot
---@param text string|nil @nil takes the readout away
---@param readout AdsHudReadoutSettings
local function setSlotText(slot, text, readout)
    if not text then
        removeSlotText(slot)
        return
    end
    if slot.isShown then
        if text == slot.text then
            return
        end
        if slot.handle and slot.handle.setText then
            slot.handle:setText(text)
            slot.text = text
            return
        end
        -- Nothing to rewrite the string with on this build, so the readout is replaced by one
        -- that reads right. Rare enough to be worth the two calls, and it keeps a counter from
        -- freezing on the round count it was added with.
        removeSlotText(slot)
    end
    -- Every readout reaching here has been through resolveReadouts, so it carries a position;
    -- the middle of the screen only stands in for one that somehow did not.
    local position = readout.position or {x = 0, y = 0}
    slot.handle = engine.hud.addText(text, position.x, position.y, getTextOptions(readout))
    slot.isShown = true
    slot.text = text
end

--- What the readouts on the right say for a weapon
---@param weaponObject WeaponObject
---@param readouts table<string, AdsHudReadoutSettings>
---@return string|nil ammoText @the rounds loaded, or the charge left
---@return string|nil reserveText @nil on a weapon with nothing to reload from
local function getAmmoTexts(weaponObject, readouts)
    -- Which of the two a weapon gets is not something the settings have to answer: a weapon that
    -- feeds from magazines carries a magazines block in its tag and a weapon that runs off a
    -- battery carries none, and an empty tag block comes back as nil rather than as an array of
    -- length zero.
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    if weaponTagData and weaponTagData.magazines then
        local magazine = weaponObject.magazines[1]
        if not magazine then
            return nil, nil
        end
        -- roundsUnloaded is what the weapon still has to reload from, which is the reserve shown
        -- next to what is in the magazine.
        return readouts.ammo.format:format(magazine.roundsLoaded),
               readouts.reserve.format:format(magazine.roundsUnloaded)
    end
    -- Age runs the other way round from a battery gauge: 0 is a weapon that has not been used
    -- at all and 1 is a spent one, so what is left of the charge is the other side of it.
    -- Rounded rather than cut off, so a full battery reads 100 and a spent one 0.
    local batteryPercent = floor((1 - weaponObject.age) * 100 + 0.5)
    return readouts.ammo.batteryFormat:format(batteryPercent), nil
end

--- Put every readout where the weapon in hand wants it this tick
---@param weaponObject WeaponObject
local function updateShownTexts(weaponObject)
    local readouts = shownReadouts
    if not readouts then
        return
    end
    local elapsedTicks = engine.game.getTickCount() - shownWeaponStartTick
    local ammoText, reserveText = getAmmoTexts(weaponObject, readouts)
    local texts = {zoom = readouts.zoom.text, ammo = ammoText, reserve = reserveText}
    for index = 1, #readoutKeys do
        local key = readoutKeys[index]
        local readout = readouts[key]
        -- A readout still inside its own delay is treated as one with nothing to say, which is
        -- what lets each of them come up on its own tick without a second piece of state.
        local text = texts[key]
        if elapsedTicks < (readout.delayTicks or 0) then
            text = nil
        end
        setSlotText(slots[key], text, readout)
    end
end

--- Take every readout off the screen
local function removeShownTexts()
    for index = 1, #readoutKeys do
        removeSlotText(slots[readoutKeys[index]])
    end
end

--- A weapon's readouts with everything it leaves out filled in from the defaults
---@param settings AdsWeaponTextSettings
---@return table<string, AdsHudReadoutSettings>
local function resolveReadouts(settings)
    -- Worked out once when the aim is taken rather than on every tick: none of it changes while
    -- the aim lasts, and what does change, the string itself, is not in here.
    local readouts = {}
    for index = 1, #readoutKeys do
        local key = readoutKeys[index]
        local resolved = {}
        for field, value in pairs(defaultReadouts[key]) do
            resolved[field] = value
        end
        for field, value in pairs(settings[key] or {}) do
            resolved[field] = value
        end
        readouts[key] = resolved
    end
    return readouts
end

--- Show the readouts of the weapon being aimed with, taking away those of any weapon that was
--- being aimed with before. A weapon this module has no settings for gets no readouts. Called
--- with nil, it only takes things away. Meant to be called every tick with whatever the aim is
--- doing right now: the ammunition counter is kept up to date from here, so a weapon that keeps
--- being asked for is a weapon whose readouts keep counting.
---@param weaponTagPath string|nil @tag path of the weapon being aimed with
---@param weaponObject WeaponObject|nil @that same weapon, as it stands right now
function adsHudText.setShownWeapon(weaponTagPath, weaponObject)
    local settings = weaponTagPath and adsWeaponTexts[weaponTagPath]
    local wantedWeaponPath = (settings and weaponObject) and weaponTagPath or nil
    if wantedWeaponPath ~= shownWeaponPath then
        removeShownTexts()
        shownWeaponPath = wantedWeaponPath
        shownReadouts = settings and wantedWeaponPath and resolveReadouts(settings) or nil
        shownWeaponStartTick = engine.game.getTickCount()
        --if wantedWeaponPath then
        --    balltze.logger.debug("ADS HUD readouts coming up for {}", wantedWeaponPath)
        --end
    end
    if shownWeaponPath and weaponObject then
        updateShownTexts(weaponObject)
    end
end

--- Take the readouts off the screen and forget them. Safe to call at unload: these texts belong
--- to the plugin rather than to the map, which is why they are removed here and the tag data
--- hudAdsElements writes is not.
function adsHudText.unload()
    removeShownTexts()
    shownWeaponPath = nil
    shownReadouts = nil
end

return adsHudText
