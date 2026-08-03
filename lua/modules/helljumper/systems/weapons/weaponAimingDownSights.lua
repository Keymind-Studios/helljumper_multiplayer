-- Lua libraries
local balltze = Balltze
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local adsHudElements = require "helljumper.systems.hud.hudAdsElements"
local adsHudText = require "helljumper.systems.hud.hudAdsText"
local path = require "helljumper.systems.constants.objectPaths"
local sounds = require "helljumper.systems.constants.sounds"
local weapons = require "helljumper.systems.constants.weapons"

--Keyboard keys reference:
--17="1", 18="2", 19="3", 20="4", 2="5", 22="6", 23="7", 24="8", 25="9", 26="10", 27="Minus", 28="Equal", 30="Tab", 31="Q", 32="W", 33="E", 34="R", 35="T", 36="Y", 37="U", 38="I", 39="O", 40="P", 43="Backslash",
--44="Caps Lock", 45="A", 46="S", 47="D", 48="F", 49="G", 50="H", 51="J", 52="K", 53="L", 56="Enter", 57="Shift", 58="Z", 59="X", 60="C", 61="V", 62="B", 63="N", 64="M", 69="Ctrl",71="Alt",72="Space",

local aimingDownSights = {}

-- What toggles the aim. Any of them can be set to nil to leave it out, and they can all be on at
-- once. The key code comes from the table above; the mouse button is 0 left, 1 middle, 2 right,
-- 3-7 the extra ones; the gamepad button is whatever index the pad reports for it, 10 being the
-- right stick pressed in.
--
-- That last one is worth checking rather than trusting: nothing settles whether the index the
-- event reports counts from zero or from one, so a gamepad button that goes unrecognised is
-- logged with the number it came in as. Watch the console with the pad in hand and set this to
-- whatever the stick actually reports.
local toggleKeyCode = 71
---@type integer|nil
local toggleMouseButton = 1
---@type integer|nil
local toggleGamepadButton = 9
-- Whether the game control this input is bound to should still see it. Mostly a mouse
-- concern: the right button usually already does something, and without this the aim and
-- that something both happen on the same click. It only suppresses the press, not the hold.
local cancelToggleInput = false

local keyboardInputAddress = 0x64C550

-- Camera field of view, in degrees, with the weapon at the hip. This is the value authored in
-- the biped tag, and what every weapon goes back to; change it here if the tag changes.
local hipFieldOfView = 76
-- Anything closer than this to the wanted value counts as already there, and is not rewritten.
local fieldOfViewTolerance = 0.5

---@class AdsWeaponSettings
---@field fieldOfView number @camera field of view while aiming, in degrees: lower pulls the weapon closer
---@field adsHuds string[]|nil @weapon_hud_interface tag paths whose hidden elements come out while aiming

-- Every weapon that aims down sights, keyed by tag path, the same way hudDynamicCrosshair keys
-- its crosshair animations. A weapon that is not listed here simply never aims, so adding one
-- is a line and removing one is deleting it. A weapon left without adsHuds still aims, it just
-- does not bring any HUD element out with it; a weapon whose reticle is spread over a HUD and
-- the child HUDs riding inside it names them all, since each one is its own tag and has to be
-- written on its own. What those elements do is hudAdsElements' business, this table only says
-- which HUDs belong to which weapon.
---@type table<string, AdsWeaponSettings>
local adsWeapons = {
    -- AssaultRifleMA38
    [path.weapons.ma38_weap] = {
        fieldOfView = 57,
        adsHuds = {
            path.weaponHudInterface.ma38ADS_hud,
            path.weaponHudInterface.ma38ADS_mask_hud
            --path.weaponHudInterface.ma38ADS_total_number_hud,
            --path.weaponHudInterface.ma38ADS_hud_loaded_number_hud
        }
    },
    -- Needler
    [path.weapons.needler_weap] = {fieldOfView = 60},
    -- Saw
    [path.weapons.saw_weap] = {fieldOfView = 60},
}

-- The engine reads the unit's camera field of view when it builds the camera and then renders
-- from its own copy, so writing the tag is not enough on its own: handing camera control over
-- and straight back is what makes it pick the new value up. Set to false to skip it.
local refreshCameraAfterWrite = true
local refreshCameraCommand = [[(begin (camera_control 1) (camera_control 0))]]

local function refreshCamera()
    if not refreshCameraAfterWrite then
        return
    end
    engine.script.execute(refreshCameraCommand)
end

-- v2 removed Engine.userInterface.playSound, so play these through HSC like the rest of the
-- project does. The project wide command attaches the sound to a player with
-- (list_get (players) n), but v2's Player carries no players list index, and "none" suits
-- a first person weapon sound better anyway: it plays it non positionally, for this client only.
local playSoundCommand = [[(begin (sound_impulse_start "%s" none %s))]]

local function playSound(tagPath, gain)
    if not tagPath then
        return
    end
    engine.script.execute(playSoundCommand:format(tagPath, gain or 1.0))
end

local isAimingDownSights = false
local isToggleRequested = false
local lastToggleInputTick = -1
-- Which weapon the aim was taken with. Swapping weapons drops the aim rather than carrying it
-- over, so this is compared against what is in hand and not merely used to pick a field of
-- view. It holds the weapon object, not its tag, so swapping between two of the same kind
-- counts as a swap too.
local aimingWeaponHandleValue = nil

--- Register that the toggle input went down. Every input source funnels through here, so one
--- press flips the aim exactly once no matter which of them saw it.
local function noteToggleInput()
    local tick = engine.game.getTickCount()
    -- Anything arriving on the same tick or the next one is the same press still being held:
    -- the event repeats while an input is down, and the poll sees it down on every tick.
    local isSamePress = lastToggleInputTick >= 0 and tick - lastToggleInputTick <= 1
    lastToggleInputTick = tick
    if not isSamePress then
        isToggleRequested = true
    end
end

-- "player_input" is raised with the input itself, covers keyboard, mouse and gamepad alike,
-- and cannot be missed. The keyboard state array is polled on top of it as a hedge, since the
-- event reports v2's own InputKey indices, which are not necessarily the indices of the table
-- at the top of this file; there is no such fallback for the mouse or the gamepad, so those
-- bindings rest entirely on the event.
local isToggleListenerRegistered = false
local toggleListener

-- The last gamepad button that went by without being the toggle, so holding a button down logs
-- the number once instead of once a frame.
local lastUnboundGamepadButton = nil
local lastUnboundGamepadButtonTick = -1

--- Say which gamepad button just came in, when it is not the one bound to the aim. This is the
--- only way to find out what the pad calls its buttons, so it stays in rather than being a thing
--- to add back whenever the binding needs changing.
---@param button integer
local function noteUnboundGamepadButton(button)
    local tick = engine.game.getTickCount()
    local isSamePress = button == lastUnboundGamepadButton and lastUnboundGamepadButtonTick >= 0 and
                            tick - lastUnboundGamepadButtonTick <= 1
    lastUnboundGamepadButton = button
    lastUnboundGamepadButtonTick = tick
    --if not isSamePress then
    --    balltze.logger.debug("Gamepad button {} is not bound to the aim", button)
    --end
end

---@param event PlayerInputEvent
local function onPlayerInput(event)
    local device = event:getDevice()
    local isToggleInput = false
    -- getKeyCode, getMouseButton and getGamepadButton each raise on the wrong device, so the
    -- device decides which one is safe to ask.
    if device == "keyboard" then
        isToggleInput = toggleKeyCode ~= nil and event:getKeyCode() == toggleKeyCode
    elseif device == "mouse" then
        isToggleInput = toggleMouseButton ~= nil and event:getMouseButton() == toggleMouseButton
    elseif device == "gamepad" then
        local button = event:getGamepadButton()
        isToggleInput = toggleGamepadButton ~= nil and button == toggleGamepadButton
        if not isToggleInput then
            noteUnboundGamepadButton(button)
        end
    end
    if not isToggleInput then
        return
    end
    if cancelToggleInput then
        event:cancel()
    end
    noteToggleInput()
end

local function ensureToggleListener()
    if isToggleListenerRegistered then
        return
    end
    -- Subscribed on first use rather than at load: require caches this module, so after an
    -- unload it would come back with its listener already removed and never subscribe again.
    toggleListener = balltze.addEventListener("player_input", onPlayerInput)
    isToggleListenerRegistered = true
end

--- The aiming settings of the weapon a biped is holding, if that weapon aims at all
---@param biped BipedObject
---@return AdsWeaponSettings|nil settings
---@return WeaponObject|nil weaponObject
---@return integer|nil weaponHandleValue @identifies the weapon in hand, not just its kind
---@return string|nil weaponTagPath @what kind of weapon it is, for the modules keyed by that
local function getHeldAdsWeapon(biped)
    -- v1 picked the held weapon by branching on blam's weaponSlot over the four
    -- firstWeaponObjectId..fourthWeaponObjectId fields; v2 exposes the slot as currentWeaponId
    -- and the four handles as one array, so the branch collapses into an index.
    local weaponHandle = biped.weapons[biped.currentWeaponId + 1]
    if not weaponHandle or weaponHandle:isNull() then
        return nil
    end
    local weaponObject = getObject(weaponHandle, "weapon")
    if not weaponObject then
        return nil
    end
    -- constants.lua still reaches for blam, so the tags come from the migrated constants. A
    -- weapon that is not in that list is simply one this module has nothing to say about.
    local weaponTag = table.find(weapons.weaponTag, function(tag)
        return tag.handle.value == weaponObject.tagHandle.value
    end)
    if not weaponTag then
        return nil
    end
    return adsWeapons[weaponTag.path], weaponObject, weaponHandle.value, weaponTag.path
end

--- Why the aim has to drop on its own this tick, or nil if it does not
---@param biped BipedObject
---@param weaponObject WeaponObject
---@return string|nil
local function getAimBreakingAction(biped, weaponObject)
    -- Both are read off the weapon and the unit's own state rather than off unitControlFlags:
    -- v2's flag list and blam's bit numbers run in opposite directions, so the name alone does
    -- not settle which bit `reload` or `grenade` really is. These say what is happening, not
    -- what key went down, which is what the aim should react to anyway.
    local magazine = weaponObject.magazines[1]
    if magazine and magazine.reloadTicksRemaining > 0 then
        return "reloading"
    end
    if biped.grenadeState ~= 0 then
        return "throwing a grenade"
    end
    return nil
end

-- ADS system for some weapons
function aimingDownSights.adsSystem()
    ensureToggleListener()
    if toggleKeyCode and balltze.memory.readInt8(keyboardInputAddress + toggleKeyCode) > 0 then
        noteToggleInput()
    end
    -- Consumed whether or not it can be acted on, so a press made with a weapon that does not
    -- aim does not fire the moment one that does comes back up.
    local isToggleWanted = isToggleRequested
    isToggleRequested = false

    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    local bipedTagData = biped and engine.tag.getTagData(biped.tagHandle, "biped")
    if not bipedTagData then
        -- Nothing to aim with, on a dead player or between maps. The tag data the aim writes is
        -- left where it is, the same way it is at unload, but the readouts are taken down: they
        -- are drawn by the plugin rather than by the HUD, so nothing else would clear them.
        adsHudText.setShownWeapon(nil, nil)
        return
    end
    ---@cast biped BipedObject

    local adsWeapon, weaponObject, weaponHandleValue, weaponTagPath = getHeldAdsWeapon(biped)
    local aimBreakingAction
    if not (adsWeapon and weaponObject) then
        aimBreakingAction = "weapon does not aim"
    elseif isAimingDownSights and aimingWeaponHandleValue ~= weaponHandleValue then
        -- Checked against the aim as it stands coming into this tick, before any toggle below
        -- is applied, so taking aim with a weapon just switched to is not mistaken for a swap.
        aimBreakingAction = "weapon changed"
    else
        aimBreakingAction = getAimBreakingAction(biped, weaponObject)
    end

    local wasAimingDownSights = isAimingDownSights
    if isToggleWanted and adsWeapon then
        isAimingDownSights = not isAimingDownSights
    end
    if aimBreakingAction then
        isAimingDownSights = false
    end

    -- Resolved from what is true right now rather than from the transition, so the field of
    -- view lands correctly whether the aim was just taken, just dropped, or dropped by a swap.
    local targetFieldOfView = hipFieldOfView
    if isAimingDownSights and adsWeapon then
        targetFieldOfView = adsWeapon.fieldOfView
    end

    -- Resolved the same way, and asked for on every tick rather than only on the transition
    -- below: the HUDs open over several ticks, and keeping the call out of the transition also
    -- means a HUD that could not be written when the aim was taken is picked up on the next tick
    -- instead of staying hidden for as long as the aim lasts.
    adsHudElements.setShownHuds(isAimingDownSights and adsWeapon and adsWeapon.adsHuds or nil)
    -- The readouts beside the reticle are keyed by the weapon rather than by its HUD, since what
    -- they say is read off the weapon in hand and not out of a tag.
    adsHudText.setShownWeapon(isAimingDownSights and weaponTagPath or nil, weaponObject)

    if isAimingDownSights ~= wasAimingDownSights then
        aimingWeaponHandleValue = isAimingDownSights and weaponHandleValue or nil
        if isAimingDownSights then
            playSound(sounds.soundTag.humanRifleZoomIn and sounds.soundTag.humanRifleZoomIn.path,
                      5)
            --balltze.logger.debug("ADS in at {} degrees", targetFieldOfView)
        else
            playSound(sounds.soundTag.humanRifleZoomOut and sounds.soundTag.humanRifleZoomOut.path,
                      5)
            --balltze.logger.debug("ADS out{}",
            --                     aimBreakingAction and (", " .. aimBreakingAction) or "")
        end
    end

    local currentFieldOfView = math.deg(bipedTagData.cameraFieldOfView)
    if math.abs(currentFieldOfView - targetFieldOfView) > fieldOfViewTolerance then
        bipedTagData.cameraFieldOfView = math.rad(targetFieldOfView)
        refreshCamera()
    end
end

--- Drop the input listener and the aim state. The tag's field of view is left alone: unload
--- runs during map teardown, where reaching back into game state is not safe, and the HUD
--- elements are dropped the same way for the same reason.
function aimingDownSights.unload()
    adsHudElements.unload()
    -- The readouts do come off the screen here: they belong to the plugin, not to the map.
    adsHudText.unload()
    if toggleListener and toggleListener.remove then
        toggleListener:remove()
    end
    toggleListener = nil
    isToggleListenerRegistered = false
    isAimingDownSights = false
    isToggleRequested = false
    lastToggleInputTick = -1
    lastUnboundGamepadButton = nil
    lastUnboundGamepadButtonTick = -1
    aimingWeaponHandleValue = nil
end

return aimingDownSights
