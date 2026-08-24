local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local getTagData = Engine.tag.getTagData
local path = require "helljumper.systems.constants.paths"
local core = require "helljumper.systems.core.core"

local dynamicCrosshair = {}

-- local rand = math.random
-- local sqrt = math.sqrt
-- local cos = math.cos
-- local sin = math.sin
-- local rad = math.rad
-- local pi = math.pi
local floor = math.floor
local ceil = math.ceil

-- The arithmetic every handler was writing out by hand, in one place. Nothing here is required: a
-- handler is handed the tag struct itself and can write any field of it directly. These only exist
-- so the two idioms this module repeats the most stop being copied around.
local hudWrite = {}

--- What a value driven by the heat comes to: where it sits cold, plus how far the heat carries it
---@param driver number @usually weaponObject.heat, but any 0..1 the animation wants to drive off
---@param initial number
---@param additional number
---@return number
function hudWrite.ramp(driver, initial, additional)
    return initial + driver * additional
end

--- Size of anything the HUD draws, whichever of the two ways its block keeps it
---
--- Guerilla shows every one of these as the same pair of boxes, width scale and height scale, but
--- the tag does not keep them the same way: a crosshair overlay, an overlay element's overlay, a
--- meter and a number each carry two loose fields, while a static element carries a single Vector2d
--- one level further in, under the .staticElement it is written through. Which one is in hand is
--- read off the struct rather than asked for, so a handler passes what it has and stops caring.
---@param element WeaponHudInterfaceCrosshairsCrosshairOverlays|WeaponHudInterfaceMeterElement|WeaponHudInterfaceNumberElement|WeaponHudInterfaceOverlayElementOverlays|HudInterfaceStaticElementDefinition
---@param width number
---@param height? number @same as width when left out, which is what a round reticle piece wants
function hudWrite.scale(element, width, height)
    local vector = element.scale
    if vector then
        vector.i = width
        vector.j = height or width
    else
        element.widthScale = width
        element.heightScale = height or width
    end
end

--- How far along x a piece sits, rounded away from the middle so it never lands short of it
---
--- The tag keeps these offsets as whole pixels. Rounding away from zero is what the animations
--- were already doing by hand, floor on the way left and ceil on the way right.
---@param element WeaponHudInterfaceCrosshairsCrosshairOverlays|WeaponHudInterfaceMeterElement|WeaponHudInterfaceNumberElement|WeaponHudInterfaceOverlayElementOverlays|HudInterfaceStaticElementDefinition
---@param value number
function hudWrite.offsetX(element, value)
    element.anchorOffset.x = value < 0 and floor(value) or ceil(value)
end

--- How far along y a piece sits, rounded the same way
---@param element WeaponHudInterfaceCrosshairsCrosshairOverlays|WeaponHudInterfaceMeterElement|WeaponHudInterfaceNumberElement|WeaponHudInterfaceOverlayElementOverlays|HudInterfaceStaticElementDefinition
---@param value number
function hudWrite.offsetY(element, value)
    element.anchorOffset.y = value < 0 and floor(value) or ceil(value)
end

--- The four channels Guerilla shows, as the one number every colour field in the tag keeps them in
---
--- Where that number goes depends on the block: an overlay or a number keeps its colours under
--- .defaultColor.parameters, a static element under .staticElement.color.parameters, and a meter
--- keeps bare ones of its own in colorAtMeterMinimum, colorAtMeterMaximum, flashColor and
--- emptyColor. This packs the value; the handler says where it lands.
---@param a integer
---@param r integer
---@param g integer
---@param b integer
---@return integer
function hudWrite.color(a, r, g, b)
    -- Arithmetic rather than shifts, so this does not depend on which Lua it is running under: the
    -- server side of this project still goes through compat53.
    return a * 0x1000000 + r * 0x10000 + g * 0x100 + b
end

-- The two HUD blocks that keep their writable pieces one level further in. A block named here is
-- walked twice, element and then piece, and its handler is called once per piece with both indices.
-- Every other block is walked once and its handler called with the element itself.
local nestedBlocks = {crosshairs = "crosshairOverlays", overlayElements = "overlays"}

-- One handler per weapon_hud_interface block, named exactly as the tag names them. Declaring the
-- signatures here once is what keeps them out of the entries below: a weapon's table is typed
-- against this, so every handler inside it takes its parameter types from these lines and carries
-- no annotation of its own. Adding a block to a weapon is the one line that names it, nothing more.
--
-- Every index is named after what it counts, and the two blocks that nest hand over the piece
-- rather than the box holding it. What arrives is one overlay either way, and the two indices say
-- which box it came out of and which overlay of that box it is: an overlay element's handler
-- reading overlayElementIndex 2 and overlayIndex 3 has the third overlay of the second overlay
-- element in hand. All indices count from one, in the order the tag lists them.
--
-- A handler is handed the tag struct itself, so what it can write is whatever the tag holds:
-- offsets, scales, colours, flags, sequence indices, any of it.
-- childHuds is the same shape all the way down, so an entry can be nested the way the tags are: the
-- HUD a weapon points at, the one that hands off to, and the one after that. A link that names the
-- one below it hands its own childHuds down; a link with no entry, or one that declares none,
-- leaves the lookup where it was, so a HUD three deep can also be named straight from the weapon
-- rather than through the two above it. Only blocks with a handler are walked either way.
---@class WeaponHudEntry
---@field crosshairs? fun(overlay: WeaponHudInterfaceCrosshairsCrosshairOverlays, weaponObject: WeaponObject, crosshairIndex: integer, overlayIndex: integer)
---@field overlayElements? fun(overlay: WeaponHudInterfaceOverlayElementOverlays, weaponObject: WeaponObject, overlayElementIndex: integer, overlayIndex: integer)
---@field staticElements? fun(element: WeaponHudInterfaceStaticElement, weaponObject: WeaponObject, staticElementIndex: integer)
---@field meterElements? fun(meter: WeaponHudInterfaceMeterElement, weaponObject: WeaponObject, meterIndex: integer)
---@field numberElements? fun(numberElement: WeaponHudInterfaceNumberElement, weaponObject: WeaponObject, numberIndex: integer)
---@field childHuds? table<string, WeaponHudEntry>

-- Every weapon whose HUD this module writes, keyed by weapon tag path
---@type table<string, WeaponHudEntry>
local weaponHuds = {

    -- AssaultRifleMA38
    [path.weapon.human.assaultRifleMa38] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex, overlayIndex)
            local reticleInitial = 4
            local reticleAdditional = 16
            local dotReticleInitial = 0.08
            local dotReticleAdditional = 0
            local animTimer = weaponObject.readyTicks +
                                  weaponObject.magazines[1].reloadTicksRemaining
            if animTimer > 20 then
                animTimer = 20
            end
            -- Left spelled out: ramp reads as "where it sits cold, plus how far the heat carries
            -- it", and the timer riding in the middle of this sum is not that
            local heat = weaponObject.heat * reticleAdditional + animTimer / 2
            local scaleDot = hudWrite.ramp(heat, dotReticleInitial, dotReticleAdditional)
            if crosshairIndex == 1 then
                if overlayIndex == 1 then
                    hudWrite.offsetX(overlay, -reticleInitial - heat) -- Left
                elseif overlayIndex == 2 then
                    hudWrite.offsetX(overlay, reticleInitial + heat) -- Right
                elseif overlayIndex == 3 then
                    hudWrite.offsetY(overlay, -reticleInitial - heat) -- Up
                elseif overlayIndex == 4 then
                    hudWrite.offsetY(overlay, reticleInitial + heat) -- Down
                end
            elseif crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleDot * weaponObject.heat)
            end
        end
    },

    -- LmgSaw
    [path.weapon.human.saw] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex, overlayIndex)
            local reticleInitial = 4
            local reticleAdditional = 20
            local dotReticleInitial = 0.08
            local dotReticleAdditional = 0
            local animTimer = weaponObject.readyTicks +
                                  weaponObject.magazines[1].reloadTicksRemaining
            if animTimer > 20 then
                animTimer = 20
            end
            local heat = weaponObject.heat * reticleAdditional + animTimer / 2
            local scaleDot = hudWrite.ramp(heat, dotReticleInitial, dotReticleAdditional)
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay, -reticleInitial - heat) -- Left
            elseif crosshairIndex == 2 then
                hudWrite.offsetX(overlay, reticleInitial + heat) -- Right
            elseif crosshairIndex == 3 then
                hudWrite.offsetY(overlay, -reticleInitial - heat) -- Up
            elseif crosshairIndex == 4 then
                hudWrite.offsetY(overlay, reticleInitial + heat) -- Down
            elseif crosshairIndex == 5 then
                hudWrite.scale(overlay, scaleDot * weaponObject.heat)
            end
        end
    },

    -- Needler
    [path.weapon.covenant.needler] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleInitial = 12
            local reticleAdditional = 15
            local dotReticleInitial = 0.08
            local dotReticleAdditional = 0
            local animTimer = weaponObject.readyTicks +
                                  weaponObject.magazines[1].reloadTicksRemaining
            if animTimer > 20 then
                animTimer = 20
            end
            local heat = weaponObject.heat * reticleAdditional + animTimer / 2
            local scaleDot = hudWrite.ramp(heat, dotReticleInitial, dotReticleAdditional)
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay, -reticleInitial - heat) -- Left
            elseif crosshairIndex == 2 then
                hudWrite.offsetX(overlay, reticleInitial + heat) -- Right
            elseif crosshairIndex == 3 then
                hudWrite.scale(overlay, scaleDot * weaponObject.heat)
            end
        end
    },

    -- Disruptor
    [path.weapon.covenant.disruptor] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleInitialPos = 0
            local reticleAdditionalPos = 1.5
            local reticleScaleInitial = 0.22
            local reticleScaleAdditional = 0.26
            local reticleScaleZero = 0
            local animTimerA = weaponObject.readyTicks * 2 +
                                   weaponObject.magazines[1].reloadTicksRemaining * 2
            if animTimerA > 20 then
                animTimerA = 20
            end
            local animTimerB = weaponObject.magazines[1].reloadTicksRemaining * 2 +
                                   reticleScaleInitial
            if animTimerB > 20 then
                animTimerB = 10
            end
            local heat = weaponObject.heat
            local reticleAddPos = hudWrite.ramp(heat, reticleInitialPos, reticleAdditionalPos)
            local reticleScale = hudWrite.ramp(heat, reticleScaleInitial, reticleScaleAdditional)
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay,
                                 -reticleInitialPos - reticleAddPos * heat - animTimerA / 4 * 0.9)
                -- The width alone, the way it was: hudWrite.scale would take the height with it and
                -- squash a piece that was never meant to move
                overlay.widthScale = reticleScale
            elseif crosshairIndex == 2 then
                hudWrite.offsetX(overlay,
                                 reticleInitialPos + reticleAddPos * heat + animTimerA / 4 * 0.9)
                overlay.widthScale = reticleScale
            elseif crosshairIndex == 3 then
                -- Two scales that do not match: the width loses a little more than the height does
                hudWrite.scale(overlay,
                               reticleScaleZero - reticleScaleInitial + animTimerA / 42 -
                                   animTimerB / 70,
                               reticleScaleZero - reticleScaleInitial + animTimerA / 42)
            end
        end
    },

    -- BattleRifle65H
    [path.weapon.human.br65h] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local zoomMaskInitial = 3
            local zoomMaskAdditional = 0.22
            local zoomInitial = 0.21
            local zoomAdditional = 0.018
            local heat = weaponObject.heat
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleZoom = hudWrite.ramp(heat, zoomInitial, zoomAdditional)
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleZoom)
            end
        end
    },

    -- DMR392
    [path.weapon.human.dmr392] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleAddPos = 3
            local zoomMaskInitial = 0.65
            local zoomMaskAdditional = 0.06
            local zoomInitial = 0.5
            local zoomAdditional = 0.06
            local heat = weaponObject.heat
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleZoom = hudWrite.ramp(heat, zoomInitial, zoomAdditional)
            local reticlePosition = heat * reticleAddPos * (4 / 3) * 0.98
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleZoom)
            elseif crosshairIndex == 3 then
                hudWrite.offsetX(overlay, -reticlePosition)
            elseif crosshairIndex == 4 then
                hudWrite.offsetX(overlay, reticlePosition)
            elseif crosshairIndex == 5 then
                hudWrite.offsetY(overlay, -reticlePosition)
            elseif crosshairIndex == 6 then
                hudWrite.offsetY(overlay, reticlePosition)
            end
        end
    },

    -- ShotgunM90
    [path.weapon.human.shotgunM90] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleScaleInitial = 0.23
            local reticleScaleAdditional = 0.07
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            -- readyTime twice and no reload, the way this one was tuned
            local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (readyTime / 6)
            local reticleScale = hudWrite.ramp(heat, reticleScaleInitial, reticleScaleAdditional)
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, reticleScale)
            end
        end
    },

    -- MagnumM6S
    [path.weapon.human.magnumM6s] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleInitial = 0.2
            local reticleAdditional = 0.6
            local zoomMaskInitial = 1.5
            local zoomMaskAdditional = 0.22
            local zoomInitial = 0.44
            local zoomAdditional = 0.057
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
            if reloadTime > 9 then
                reloadTime = 0
            end
            local heat = weaponObject.heat
            local scaleReticle = hudWrite.ramp(heat, reticleInitial, reticleAdditional)
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleZoom = hudWrite.ramp(heat, zoomInitial, zoomAdditional)
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleZoom)
            elseif crosshairIndex == 3 then
                hudWrite.scale(overlay, scaleReticle + readyTime / 8 * 0.5 + reloadTime / 8 * 0.4)
            elseif crosshairIndex == 4 then
                hudWrite.scale(overlay, 0.165)
            end
        end
    },

    -- SpnkrRocketLauncher
    [path.weapon.human.spnkr] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local zoomMaskInitial = 1.7
            local zoomMaskAdditional = 0.6
            local zoomInitial = 0.5
            local zoomAdditional = 0.15
            local heat = weaponObject.heat
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleZoom = hudWrite.ramp(heat, zoomInitial, zoomAdditional)
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleZoom)
            end
        end
    },

    -- VK78Commando
    [path.weapon.human.vk78Commando] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local zoomFullInitial = 0.4
            local zoomFullAdditional = 0.03
            local zoomMaskInitial = 2
            local zoomMaskAdditional = 0.03
            local zoomBlurInitial = 0.8
            local zoomBlurAdditional = 0.2
            local reticleInitPos = 3
            local reticleAddPos = 16
            local reticleInitScale = 0.2
            local reticleAddScale = 0.16
            local strokeInitial = 0.22
            local strokeAdditional = 0.0015
            local strokeLess = 0.08
            local dotReticleInitial = 0.09
            local dotReticleAdditional = 0
            local animTimer = weaponObject.readyTicks +
                                  weaponObject.magazines[1].reloadTicksRemaining
            if animTimer > 20 then
                animTimer = 10
            end
            local heat = weaponObject.heat + animTimer / 30
            local heatOrig = weaponObject.heat
            local scaleMask = hudWrite.ramp(heatOrig, zoomMaskInitial, zoomMaskAdditional)
            local scaleFull = hudWrite.ramp(heatOrig, zoomFullInitial, zoomFullAdditional)
            local scaleBlur = hudWrite.ramp(heatOrig, zoomBlurInitial, zoomBlurAdditional)
            local scaleReticle = hudWrite.ramp(heat, reticleInitScale, reticleAddScale)
            local scaleStroke = hudWrite.ramp(heat, strokeInitial, strokeAdditional)
            local scaleDot = hudWrite.ramp(heat, dotReticleInitial, dotReticleAdditional)
            local posReticleAdd = heat * 1.1 * reticleAddPos
            if crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 3 or crosshairIndex == 4 then
                hudWrite.scale(overlay, scaleFull)
            elseif crosshairIndex == 5 or crosshairIndex == 6 then
                hudWrite.scale(overlay, scaleBlur)
            elseif crosshairIndex == 7 then
                hudWrite.scale(overlay, scaleReticle)
            elseif crosshairIndex == 8 then
                hudWrite.offsetX(overlay, -reticleInitPos - posReticleAdd * heat) -- Left
                -- Thinner across than it is tall, which is what a vertical stroke wants
                hudWrite.scale(overlay, scaleStroke - strokeLess, scaleStroke)
            elseif crosshairIndex == 9 then
                hudWrite.offsetX(overlay, reticleInitPos + posReticleAdd * heat) -- Right
                hudWrite.scale(overlay, scaleStroke - strokeLess, scaleStroke)
            elseif crosshairIndex == 10 then
                hudWrite.offsetY(overlay, -reticleInitPos - posReticleAdd * heat) -- Up
                -- And the other way round for a horizontal one
                hudWrite.scale(overlay, scaleStroke, scaleStroke - strokeLess)
            elseif crosshairIndex == 11 then
                hudWrite.offsetY(overlay, reticleInitPos + posReticleAdd * heat) -- Down
                hudWrite.scale(overlay, scaleStroke, scaleStroke - strokeLess)
            elseif crosshairIndex == 12 then
                hudWrite.scale(overlay, scaleDot * heatOrig)
            end
        end
    },

    -- SniperRifle
    --
    -- Its HUD is a chain rather than a single tag: sniper_rifle_srs99c hands off to
    -- sniper_rifle_ticks, and that one to sniper_rifle_ext_meters. The crosshairs below belong to
    -- the first; anything further down is named under childHuds by its own tag path.
    [path.weapon.human.sniper] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleInitial = 0
            local reticleAdditional = 0.3
            local zoomMaskInitial = 0.9
            local zoomMaskAdditional = 0.03
            local zoomInitial = 0.45
            local zoomAdditional = 0.03
            local zoomLevelsInitial = 0.35
            local zoomLevelsAdditional = 0.12
            local zoomLevelPosInitial = -140
            local zoomLevelPosAdditional = 10
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
            if reloadTime > 9 then
                reloadTime = 0
            end
            local heat = weaponObject.heat
            local scaleReticle = hudWrite.ramp(heat, reticleInitial, reticleAdditional)
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleZoom = hudWrite.ramp(heat, zoomInitial, zoomAdditional)
            local scaleZoomLevels = hudWrite.ramp(heat, zoomLevelsInitial, zoomLevelsAdditional)
            local positionZoomLevels = heat * zoomLevelPosAdditional
            if crosshairIndex == 2 then
                hudWrite.scale(overlay, scaleReticle + readyTime / 25 * 0.5)
            elseif crosshairIndex == 3 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 4 then
                hudWrite.scale(overlay, scaleZoom)
                --overlay.defaultColor.parameters.defaultColor = hudWrite.color(200, 169, 198, 243)
                overlay.defaultColor.parameters.defaultColor = hudWrite.color(200, 0, 0, 0)
            elseif crosshairIndex == 5 or crosshairIndex == 6 then
                hudWrite.scale(overlay, scaleZoom)
            elseif crosshairIndex == 7 then
                hudWrite.scale(overlay, scaleZoomLevels)
                hudWrite.offsetY(overlay, zoomLevelPosInitial - positionZoomLevels * heat -
                                     reloadTime / 2 * 0.9)
            end
        end,
        childHuds = {
            [path.weaponHudInterface.child.sniperRifleTicks] = {
                staticElements = function (element, weaponObject, staticElementIndex)
                    local ticksInitialScale = 1
                    local ticksAdditionalScale = 0.3
                    local LTicksInitialPos = -280
                    local LTicksAdditionalPos = -15
                    local RTicksInitialPos = 280
                    local RTicksAdditionalPos = 15
                    local heat = weaponObject.heat
                    local scaleReticle = hudWrite.ramp(heat, ticksInitialScale, ticksAdditionalScale)
                    local LTickPos = hudWrite.ramp(heat, LTicksInitialPos, LTicksAdditionalPos)
                    local RTickPos = hudWrite.ramp(heat, RTicksInitialPos, RTicksAdditionalPos)
                    if staticElementIndex == 1 then
                        hudWrite.scale(element.staticElement, scaleReticle)
                        hudWrite.offsetX(element.staticElement, RTickPos)
                    elseif staticElementIndex == 2 then
                        hudWrite.scale(element.staticElement, scaleReticle)
                        hudWrite.offsetX(element.staticElement, LTickPos)
                    end
                end,
                childHuds = {
                    [path.weaponHudInterface.child.sniperRifleExtMeters] = {
                        meterElements = function(meter, weaponObject, meterIndex)
                            -- A meter keeps its offset and its two scales bare, the way an overlay does,
                            -- and its colours as four numbers of its own: colorAtMeterMinimum,
                            -- colorAtMeterMaximum, flashColor and emptyColor. hudWrite.color packs one.
                            if meterIndex == 1 then
                                local meterInitial = 100
                                local meterAdditional = 5
                                local heat = weaponObject.heat * meterAdditional
                                local scaleMeter = hudWrite.ramp(heat, 0.5, 0.01)
                                hudWrite.scale(meter, scaleMeter)
                                hudWrite.offsetY(meter, meterInitial + heat)
                            end
                        end
                    }
                }
            }
        }
    },

    -- Skewer
    [path.weapon.covenant.skewer] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local zoomFullInitial = 0.47
            local zoomFullAdditional = 0.1
            local zoomMaskInitial = 0.88
            local zoomMaskAdditional = 0.1
            local reticleAddPos = 10
            local reticleAddScale = 0.25
            local animTimer = weaponObject.readyTicks * 2 +
                                  weaponObject.magazines[1].reloadTicksRemaining * 2
            if animTimer > 20 then
                animTimer = 20
            end
            local heat = weaponObject.heat
            local scaleMask = hudWrite.ramp(heat, zoomMaskInitial, zoomMaskAdditional)
            local scaleFull = hudWrite.ramp(heat, zoomFullInitial, zoomFullAdditional)
            local posReticleAdd = heat * reticleAddPos
            if crosshairIndex == 1 then
                hudWrite.scale(overlay, scaleMask)
            elseif crosshairIndex == 2 or crosshairIndex == 3 then
                hudWrite.scale(overlay, scaleFull)
            elseif crosshairIndex == 4 then
                hudWrite.offsetX(overlay, -posReticleAdd * heat - animTimer / 2 * 0.9)
            elseif crosshairIndex == 5 then
                hudWrite.offsetX(overlay, posReticleAdd * heat + animTimer / 2 * 0.9)
            elseif crosshairIndex == 6 then
                hudWrite.scale(overlay, -reticleAddScale + animTimer / 30)
            end
        end
    },

    -- StormRifle
    [path.weapon.covenant.stormRifle] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleAddPos = 3
            local reticleScaleInitial = 0.3
            local reticleScaleAdditional = 0.07
            local dotReticleInitial = 0.09
            local dotReticleAdditional = 0
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
            if reloadTime > 9 then
                reloadTime = 0
            end
            local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (reloadTime / 6 * 0.4)
            local heatOrig = weaponObject.heat
            local reticleScale = hudWrite.ramp(heat, reticleScaleInitial, -reticleScaleAdditional)
            local scaleDot = hudWrite.ramp(heat, dotReticleInitial, dotReticleAdditional)
            local reticlePos = heat * reticleAddPos * 2
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay, -reticlePos) -- Up and to the left
                hudWrite.offsetY(overlay, -reticlePos)
            elseif crosshairIndex == 2 then
                hudWrite.offsetX(overlay, reticlePos) -- Down and to the right
                hudWrite.offsetY(overlay, reticlePos)
            elseif crosshairIndex == 3 then
                hudWrite.offsetX(overlay, reticlePos) -- Up and to the right
                hudWrite.offsetY(overlay, -reticlePos)
            elseif crosshairIndex == 4 then
                hudWrite.offsetX(overlay, -reticlePos) -- Down and to the left
                hudWrite.offsetY(overlay, reticlePos)
            elseif crosshairIndex == 5 then
                hudWrite.scale(overlay, scaleDot * heatOrig)
            elseif crosshairIndex == 6 then
                hudWrite.scale(overlay, -reticleScale)
            end
        end
    },

    -- PlasmaPistol
    [path.weapon.covenant.plasmaPistol] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleAddPos = 3.5
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
            if reloadTime > 9 then
                reloadTime = 0
            end
            local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (reloadTime / 6 * 0.4)
            local reticlePos = heat * reticleAddPos ^ 2
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay, -reticlePos) -- Left
                hudWrite.offsetY(overlay, reticlePos * 0.45)
            elseif crosshairIndex == 2 then
                hudWrite.offsetY(overlay, -reticlePos) -- Up
            elseif crosshairIndex == 3 then
                hudWrite.offsetX(overlay, reticlePos) -- Right
                hudWrite.offsetY(overlay, reticlePos * 0.45)
            end
        end
    },

    -- PlasmaCaster
    [path.weapon.covenant.plasmaCaster] = {
        crosshairs = function(overlay, weaponObject, crosshairIndex)
            local reticleInitPos = 2
            local reticleAddPos = 3
            local readyTime = weaponObject.readyTicks
            if readyTime > 10 then
                readyTime = 6
            end
            -- Off the reload alone and at twice its rate, which is this one's own way of counting
            local reloadTimeB = weaponObject.magazines[1].reloadTicksRemaining * 2
            if reloadTimeB > 20 then
                reloadTimeB = 20
            end
            local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (reloadTimeB / 45)
            local reticlePos = heat * reticleAddPos ^ 2
            if crosshairIndex == 1 then
                hudWrite.offsetX(overlay, -reticleInitPos - reticlePos) -- Left
                hudWrite.offsetY(overlay, reticleInitPos + reticlePos * 0.45)
            elseif crosshairIndex == 2 then
                hudWrite.offsetY(overlay, -reticleInitPos - reticlePos) -- Up
            elseif crosshairIndex == 3 then
                hudWrite.offsetX(overlay, reticleInitPos + reticlePos) -- Right
                hudWrite.offsetY(overlay, reticleInitPos + reticlePos * 0.45)
            end
        end
    }
}



-- Handed out so a weapon's handlers can reach the same arithmetic the module uses. Nothing is
-- forced through it: a handler writes the tag struct directly.
dynamicCrosshair.write = hudWrite

-- Every entry above as the loaded map has it: the weapons keyed by the value of their tag handles,
-- and the child HUDs inside them by theirs. The tick then reaches both with a handle it is already
-- holding, rather than with a path it has to read out of a tag first.
---@type table<integer, WeaponHudEntry>
local weaponHudsByTag = {}

--- One weapon's entry over again, with the tag paths inside it turned into the handles they name
---@param entry WeaponHudEntry
---@return WeaponHudEntry
local function resolveHudEntry(entry)
    local resolved = {}
    for blockName, handler in pairs(entry) do
        -- Every block's handler carries straight over; only the paths need doing anything to.
        if blockName ~= "childHuds" then
            resolved[blockName] = handler
        end
    end
    local childHuds = entry.childHuds
    if childHuds then
        -- The same field under the same name and the same shape all the way down, keyed by handle
        -- value instead of by path, which is what lets the walk carry on reading it the way it did.
        local resolvedChildHuds = {}
        for hudTagPath, childEntry in pairs(childHuds) do
            local hudTagHandle = engine.tag.lookupTag(hudTagPath, "weapon_hud_interface")
            if hudTagHandle then
                resolvedChildHuds[hudTagHandle.value] = resolveHudEntry(childEntry)
            end
        end
        resolved.childHuds = resolvedChildHuds
    end
    return resolved
end

--- Work out which of the loaded map's weapons and HUDs the entries above are about
---
--- Once a map, since a handle is only good for as long as the map it was looked up in. Called after
--- tags.get(), the way every load in this project is.
function dynamicCrosshair.load()
    local entriesByTag = core.resolveTagKeys(weaponHuds, "weapon")
    weaponHudsByTag = {}
    for weaponTagValue, entry in pairs(entriesByTag) do
        weaponHudsByTag[weaponTagValue] = resolveHudEntry(entry)
    end
end

--- Let go of what belonged to the map that is going
function dynamicCrosshair.unload()
    weaponHudsByTag = {}
end

--- Depth the child HUD walk gives up at
---
--- A tag whose child hud leads back to one already walked is caught by the handles seen along the
--- way, so this is only a floor under a chain that is merely absurd rather than circular.
local maxHudDepth = 8

function dynamicCrosshair.dynamicReticles()
    local player = getPlayer()
    if not player then
        return
    end
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return
    end
    local weaponObjectHandle = biped.weapons[biped.currentWeaponId + 1]
    if not weaponObjectHandle or (weaponObjectHandle and weaponObjectHandle:isNull()) then
        return
    end
    local weaponObject = getObject(weaponObjectHandle, "weapon")
    if not weaponObject then
        return
    end
    -- Reached with the handle the weapon carries. This used to go through the weapon's tag entry for
    -- its path, which meant reading a string out of the tag on every tick to hash it with; the table
    -- was keyed by that path when the map came up instead.
    local weaponHud = weaponHudsByTag[weaponObject.tagHandle.value]
    if not weaponHud then
        -- A weapon this table says nothing about has no HUD of its own to write.
        return
    end
    -- v2 has no .data on a tag entry; the weapon tag data is what carries the HUD it was authored
    -- with. Resolved once here rather than inside the loop, which would rebuild the whole tag view
    -- per crosshair.
    local weaponTagData = getTagData(weaponObject.tagHandle, "weapon")
    if not weaponTagData then
        return
    end
    ---@cast weaponTagData Weapon
    -- Taken straight off the reference the weapon tag already carries. This used to look the handle
    -- up in a list of HUD tags gathered by path substring, which meant a weapon whose HUD had a
    -- sibling sharing its name, an ADS one for instance, could end up with the sibling on the list
    -- and its own HUD nowhere on it.
    local hudReference = weaponTagData.hudInterface
    -- The weapon's own HUD is the first link of a chain: each one can hand off to another through
    -- its child hud reference, and the sniper's reticle is spread over three of them. The first
    -- link is written by the handlers sitting at the top of the weapon's entry, and every link past
    -- it by whatever childHuds says about that link's own tag path.
    ---@type WeaponHudEntry|nil
    local handlers = weaponHud
    -- Where the link below is looked up, and it travels down with the chain: a link whose entry
    -- declares childHuds of its own hands them to the link under it, which is what lets an entry be
    -- nested the way the tags are. A link with no entry, or one that declares none, leaves this
    -- where it was, so a HUD three deep can also be named straight from the weapon.
    local childHudLookup = weaponHud.childHuds
    local walkedHuds = {}
    for _ = 1, maxHudDepth do
        -- A HUD with no child leaves a reference behind that still carries a path, so the handle is
        -- what says whether there is another link or not.
        if not hudReference or hudReference.tagHandle:isNull() then
            return
        end
        local hudHandleValue = hudReference.tagHandle.value
        if walkedHuds[hudHandleValue] then
            -- A chain that leads back to a link already written would go round forever
            return
        end
        walkedHuds[hudHandleValue] = true
        local hudTagData = getTagData(hudReference.tagHandle, "weapon_hud_interface")
        if not hudTagData then
            return
        end
        ---@cast hudTagData WeaponHudInterface
        if handlers then
            if handlers.childHuds then
                childHudLookup = handlers.childHuds
            end
            for blockName, handler in pairs(handlers) do
                -- Tag blocks are plain arrays in v2: no .elements wrapper and no .count field. An
                -- empty one comes back as nil rather than as an array of length zero, so a HUD
                -- without the block a handler asks for is stepped over rather than indexed into.
                local block
                if blockName ~= "childHuds" then
                    block = hudTagData[blockName]
                end
                if block then
                    -- Named here, the block keeps its writable pieces one level further in and the
                    -- handler wants both indices; otherwise the element itself is what is written.
                    local nestedName = nestedBlocks[blockName]
                    for elementIndex = 1, #block do
                        local element = block[elementIndex]
                        local pieces = nestedName and element[nestedName]
                        if pieces then
                            for pieceIndex = 1, #pieces do
                                handler(pieces[pieceIndex], weaponObject, elementIndex, pieceIndex)
                            end
                        elseif not nestedName then
                            handler(element, weaponObject, elementIndex)
                        end
                    end
                end
            end
        end
        hudReference = hudTagData.childHud
        -- By the handle the reference carries, the same one the top of the loop reads to decide
        -- whether there is a link at all, rather than by the path beside it. A null handle finds
        -- nothing here and is turned back at the top of the next pass.
        handlers = childHudLookup and hudReference and
                       childHudLookup[hudReference.tagHandle.value]
    end
end

return dynamicCrosshair
