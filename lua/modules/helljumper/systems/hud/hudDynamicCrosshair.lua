local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local getTagData = Engine.tag.getTagData
local getTagEntry = Engine.tag.getTagEntry
local path = require "helljumper.systems.constants.paths"

local dynamicCrosshair = {}

-- local rand = math.random
-- local sqrt = math.sqrt
-- local cos = math.cos
-- local sin = math.sin
-- local rad = math.rad
-- local pi = math.pi
local floor = math.floor
local ceil = math.ceil

local crossHairAnimations = {

    -- AssaultRifleMA38
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    ---@param overlayIndex integer
    [path.weapon.human.assaultRifleMa38] = function(overlay, weaponObject, crosshairIndex,
                                                    overlayIndex)
        local reticleInitial = 4
        local reticleAdditional = 16
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            if overlayIndex == 1 then
                overlay.anchorOffset.x = floor(-reticleInitial - heat) -- Left
            elseif overlayIndex == 2 then
                overlay.anchorOffset.x = ceil(reticleInitial + heat) -- Right
            elseif overlayIndex == 3 then
                overlay.anchorOffset.y = floor(-reticleInitial - heat) -- Up
            elseif overlayIndex == 4 then
                overlay.anchorOffset.y = ceil(reticleInitial + heat) -- Down
            end
        elseif crosshairIndex == 2 then
            overlay.heightScale = scaleDot * heatOrig
            overlay.widthScale = scaleDot * heatOrig
        end
    end,

    -- LmgSaw
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    ---@param overlayIndex integer
    [path.weapon.human.saw] = function(overlay, weaponObject, crosshairIndex, overlayIndex)
        local reticleInitial = 4
        local reticleAdditional = 20
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(-reticleInitial - heat) -- Left
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.x = ceil(reticleInitial + heat) -- Right
        elseif crosshairIndex == 3 then
            overlay.anchorOffset.y = floor(-reticleInitial - heat) -- Up
        elseif crosshairIndex == 4 then
            overlay.anchorOffset.y = ceil(reticleInitial + heat) -- Down
        elseif crosshairIndex == 5 then
            overlay.heightScale = scaleDot * heatOrig
            overlay.widthScale = scaleDot * heatOrig
        end
    end,

    -- Needler
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.needler] = function(overlay, weaponObject, crosshairIndex)
        local reticleInitial = 12
        local reticleAdditional = 15
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(-reticleInitial - heat) -- Left
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.x = ceil(reticleInitial + heat) -- Right
        elseif crosshairIndex == 3 then
            overlay.heightScale = scaleDot * heatOrig
            overlay.widthScale = scaleDot * heatOrig
        end
    end,

    -- Disruptor
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.disruptor] = function(overlay, weaponObject, crosshairIndex)
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
        local animTimerB = weaponObject.magazines[1].reloadTicksRemaining * 2 + reticleScaleInitial
        if animTimerB > 20 then
            animTimerB = 10
        end
        local heat = weaponObject.heat
        local reticleAddPos = reticleInitialPos + heat * reticleAdditionalPos
        local reticleScale = reticleScaleInitial + heat * reticleScaleAdditional
        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(
                                        -reticleInitialPos - reticleAddPos * heat - animTimerA / 4 *
                                            0.9) -- Left
            overlay.widthScale = reticleScale
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.x = ceil(reticleInitialPos + reticleAddPos * heat + animTimerA / 4 *
                                              0.9) -- Right
            overlay.widthScale = reticleScale
        elseif crosshairIndex == 3 then
            overlay.widthScale = reticleScaleZero - reticleScaleInitial + animTimerA / 42 -
                                     animTimerB / 70
            overlay.heightScale = reticleScaleZero - reticleScaleInitial + animTimerA / 42
        end
    end,

    -- BattleRifle65H
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.br65h] = function(overlay, weaponObject, crosshairIndex)
        local zoomMaskInitial = 3
        local zoomMaskAdditional = 0.22
        local zoomInitial = 0.21
        local zoomAdditional = 0.018
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        if crosshairIndex == 1 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            overlay.widthScale = scaleZoom
            overlay.heightScale = scaleZoom
        end
    end,

    -- DMR392
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.dmr392] = function(overlay, weaponObject, crosshairIndex)
        local reticleAddPos = 3
        local reticleScaleInitial = 0.15
        local reticleScaleAdditional = 0.1
        local zoomMaskInitial = 0.65
        local zoomMaskAdditional = 0.06
        local zoomInitial = 0.5
        local zoomAdditional = 0.06
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        local reticlePosition = heat * reticleAddPos
        if crosshairIndex == 1 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            overlay.widthScale = scaleZoom
            overlay.heightScale = scaleZoom
        elseif crosshairIndex == 3 then
            overlay.anchorOffset.x = floor(-reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 4 then
            overlay.anchorOffset.x = ceil(reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 5 then
            overlay.anchorOffset.y = floor(-reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 6 then
            overlay.anchorOffset.y = ceil(reticlePosition * (4 / 3) * 0.98)
        end
    end,

    -- ShotgunM90
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.shotgunM90] = function(overlay, weaponObject, crosshairIndex)
        local reticleScaleInitial = 0.23
        local reticleScaleAdditional = 0.07
        local readyTime = weaponObject.readyTicks
        if readyTime > 10 then
            readyTime = 6
        end
        local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
        if reloadTime > 9 then
            reloadTime = 0
        end
        local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (readyTime / 6)
        local reticleScale = reticleScaleInitial + heat * reticleScaleAdditional
        if crosshairIndex == 1 then
            overlay.widthScale = reticleScale
            overlay.heightScale = reticleScale
        end
    end,

    -- MagnumM6S
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.magnumM6s] = function(overlay, weaponObject, crosshairIndex)
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
        -- FIXME This also leaks memory due to magazines being a table with unknown elements
        local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
        if reloadTime > 9 then
            reloadTime = 0
        end
        local heat = weaponObject.heat
        local scaleReticle = reticleInitial + heat * reticleAdditional
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        if crosshairIndex == 1 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            overlay.widthScale = scaleZoom
            overlay.heightScale = scaleZoom
        elseif crosshairIndex == 3 then
            overlay.widthScale = scaleReticle + readyTime / 8 * 0.5 + reloadTime / 8 * 0.4
            overlay.heightScale = scaleReticle + readyTime / 8 * 0.5 + reloadTime / 8 * 0.4
        elseif crosshairIndex == 4 then
            overlay.widthScale = 0.165
            overlay.heightScale = 0.165
        end
    end,

    -- SpnkrRocketLauncher
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.spnkr] = function(overlay, weaponObject, crosshairIndex)
        local zoomMaskInitial = 1.7
        local zoomMaskAdditional = 0.6
        local zoomInitial = 0.5
        local zoomAdditional = 0.15
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        if crosshairIndex == 1 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            overlay.widthScale = scaleZoom
            overlay.heightScale = scaleZoom
        end
    end,

    -- VK78Commando
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.vk78Commando] = function(overlay, weaponObject, crosshairIndex)
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
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 10
        end
        local heat = weaponObject.heat + animTimer / 30
        local heatOrig = weaponObject.heat
        local scaleMask = zoomMaskInitial + heatOrig * zoomMaskAdditional
        local scaleFull = zoomFullInitial + heatOrig * zoomFullAdditional
        local scaleBlur = zoomBlurInitial + heatOrig * zoomBlurAdditional
        local scaleReticle = reticleInitScale + heat * reticleAddScale
        local posReticleAdd = heat * 1.1 * reticleAddPos
        local scaleStroke = strokeInitial + heat * strokeAdditional
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 2 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 3 then
            overlay.widthScale = scaleFull
            overlay.heightScale = scaleFull
        elseif crosshairIndex == 4 then
            overlay.widthScale = scaleFull
            overlay.heightScale = scaleFull
        elseif crosshairIndex == 5 then
            overlay.widthScale = scaleBlur
            overlay.heightScale = scaleBlur
        elseif crosshairIndex == 6 then
            overlay.widthScale = scaleBlur
            overlay.heightScale = scaleBlur
        elseif crosshairIndex == 7 then
            overlay.widthScale = scaleReticle
            overlay.heightScale = scaleReticle
        elseif crosshairIndex == 8 then
            overlay.anchorOffset.x = floor(-reticleInitPos - posReticleAdd * heat) -- Left
            overlay.widthScale = scaleStroke - strokeLess
            overlay.heightScale = scaleStroke
        elseif crosshairIndex == 9 then
            overlay.anchorOffset.x = ceil(reticleInitPos + posReticleAdd * heat) -- Right
            overlay.widthScale = scaleStroke - strokeLess
            overlay.heightScale = scaleStroke
        elseif crosshairIndex == 10 then
            overlay.anchorOffset.y = floor(-reticleInitPos - posReticleAdd * heat) -- Left
            overlay.widthScale = scaleStroke
            overlay.heightScale = scaleStroke - strokeLess
        elseif crosshairIndex == 11 then
            overlay.anchorOffset.y = ceil(reticleInitPos + posReticleAdd * heat) -- Left
            overlay.widthScale = scaleStroke
            overlay.heightScale = scaleStroke - strokeLess
        elseif crosshairIndex == 12 then
            overlay.heightScale = scaleDot * heatOrig
            overlay.widthScale = scaleDot * heatOrig
        end
    end,

    -- SniperRifle
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.human.sniper] = function(overlay, weaponObject, crosshairIndex)
        local reticleInitial = 0
        local reticleAdditional = 0.3
        local zoomMaskInitial = 2.209
        local zoomMaskAdditional = 0.03
        local zoomInitial = 0.47
        local zoomAdditional = 0.03
        local zoomLevelsInitial = 0.45
        local zoomLevelsAdditional = 0.12
        local zoomLevelPosInitial = -200
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
        local scaleReticle = reticleInitial + heat * reticleAdditional
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        local scaleZoomLevels = zoomLevelsInitial + heat * zoomLevelsAdditional
        local positionZoomLevels = heat * zoomLevelPosAdditional
        if crosshairIndex == 2 then
            overlay.widthScale = scaleReticle + readyTime / 25 * 0.5
            overlay.heightScale = scaleReticle + readyTime / 25 * 0.5
        elseif crosshairIndex == 3 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 4 then
            overlay.widthScale = scaleZoom
            overlay.heightScale = scaleZoom
        elseif crosshairIndex == 5 then
            overlay.widthScale = scaleZoomLevels
            overlay.heightScale = scaleZoomLevels
            overlay.anchorOffset.x = floor(zoomLevelPosInitial - positionZoomLevels * heat -
                                               reloadTime / 2 * 0.9)
        end
    end,

    -- Skewer
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.skewer] = function(overlay, weaponObject, crosshairIndex)
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
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleFull = zoomFullInitial + heat * zoomFullAdditional
        local posReticleAdd = heat * reticleAddPos
        if crosshairIndex == 1 then
            overlay.widthScale = scaleMask
            overlay.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            overlay.widthScale = scaleFull
            overlay.heightScale = scaleFull
        elseif crosshairIndex == 3 then
            overlay.widthScale = scaleFull
            overlay.heightScale = scaleFull
        elseif crosshairIndex == 4 then
            overlay.anchorOffset.x = floor(-posReticleAdd * heat - animTimer / 2 * 0.9)
        elseif crosshairIndex == 5 then
            overlay.anchorOffset.x = ceil(posReticleAdd * heat + animTimer / 2 * 0.9)
        elseif crosshairIndex == 6 then
            overlay.widthScale = -reticleAddScale + animTimer / 30
            overlay.heightScale = -reticleAddScale + animTimer / 30
        end
    end,

    -- StormRifle
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.stormRifle] = function(overlay, weaponObject, crosshairIndex)
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
        local reticleScale = reticleScaleInitial + heat * -reticleScaleAdditional
        local reticlePos = heat * reticleAddPos * 2
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional

        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(-reticlePos)
            overlay.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.x = ceil(reticlePos)
            overlay.anchorOffset.y = ceil(reticlePos)
        elseif crosshairIndex == 3 then
            overlay.anchorOffset.x = ceil(reticlePos)
            overlay.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 4 then
            overlay.anchorOffset.x = floor(-reticlePos)
            overlay.anchorOffset.y = ceil(reticlePos)
        elseif crosshairIndex == 5 then
            overlay.widthScale = scaleDot * heatOrig
            overlay.heightScale = scaleDot * heatOrig
        elseif crosshairIndex == 6 then
            overlay.widthScale = -reticleScale
            overlay.heightScale = -reticleScale
        end
    end,

    -- PlasmaPistol
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.plasmaPistol] = function(overlay, weaponObject, crosshairIndex)
        local reticleAddPos = 3.5
        local reticleScaleInitial = 0.19
        local reticleScaleAdditional = 0.08
        local readyTime = weaponObject.readyTicks
        if readyTime > 10 then
            readyTime = 6
        end
        local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
        if reloadTime > 9 then
            reloadTime = 0
        end
        local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (reloadTime / 6 * 0.4)
        local reticleScale = reticleScaleInitial + heat * reticleScaleAdditional
        local reticlePos = heat * reticleAddPos ^ 2
        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(-reticlePos)
            overlay.anchorOffset.y = ceil(reticlePos * 0.45)
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 3 then
            overlay.anchorOffset.x = ceil(reticlePos)
            overlay.anchorOffset.y = ceil(reticlePos * 0.45)
        end
    end,

    -- PlasmaCaster
    ---@param overlay WeaponHudInterfaceCrosshairsCrosshairOverlays
    ---@param weaponObject WeaponObject
    ---@param crosshairIndex integer
    [path.weapon.covenant.plasmaCaster] = function(overlay, weaponObject, crosshairIndex)
        local reticleInitPos = 2
        local reticleAddPos = 3
        local reticleScaleInitial = 0.18
        local reticleScaleAdditional = 0.025
        local readyTime = weaponObject.readyTicks
        if readyTime > 10 then
            readyTime = 6
        end
        local reloadTime = weaponObject.magazines[1].reloadTicksRemaining * 0.4
        if reloadTime > 9 then
            reloadTime = 0
        end
        local reloadTimeB = weaponObject.magazines[1].reloadTicksRemaining * 2
        if reloadTimeB > 20 then
            reloadTimeB = 20
        end
        local heat = weaponObject.heat + (readyTime / 6 * 0.5) + (reloadTimeB / 45)
        local reticleScale = reticleScaleInitial + heat * reticleScaleAdditional
        local reticlePos = heat * reticleAddPos ^ 2
        if crosshairIndex == 1 then
            overlay.anchorOffset.x = floor(-reticleInitPos - reticlePos)
            overlay.anchorOffset.y = ceil(reticleInitPos + reticlePos * 0.45)
        elseif crosshairIndex == 2 then
            overlay.anchorOffset.y = floor(-reticleInitPos - reticlePos)
        elseif crosshairIndex == 3 then
            overlay.anchorOffset.x = ceil(reticleInitPos + reticlePos)
            overlay.anchorOffset.y = ceil(reticleInitPos + reticlePos * 0.45)
        end
    end
}

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
    -- Read off the tag entry rather than matched against the tags constants: the path is what keys
    -- crossHairAnimations, and getTagEntry hands it over without walking every weapon tag in the
    -- map. constants.tags holds bare TagHandles now, which carry neither a path nor a handle field,
    -- and it only holds them once tags.get() has run.
    local weaponTagEntry = getTagEntry(weaponObject.tagHandle)
    local crossHairAnimation = weaponTagEntry and crossHairAnimations[weaponTagEntry.path]
    if not crossHairAnimation then
        -- A weapon this table says nothing about has no reticle of its own to animate.
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
    local hudInterface = weaponTagData.hudInterface
    -- A weapon with no HUD of its own has no reticle to move. Its reference still carries a path
    -- field, so the handle is what says whether anything is actually there.
    if not hudInterface or hudInterface.tagHandle:isNull() then
        return
    end
    -- Taken straight off the handle the weapon tag already carries. This used to look the handle up
    -- in a list of HUD tags gathered by path substring, which meant a weapon whose HUD had a sibling
    -- sharing its name, an ADS one for instance, could end up with the sibling on the list and its
    -- own HUD nowhere on it.
    local hudInterfaceTagData = getTagData(hudInterface.tagHandle, "weapon_hud_interface")
    if not hudInterfaceTagData then
        return
    end
    -- Tag blocks are plain arrays in v2: no .elements wrapper and no .count field.
    ---@cast hudInterfaceTagData WeaponHudInterface
    -- An empty tag block comes back as nil rather than as an array of length zero, so a HUD with no
    -- crosshairs at all is caught here rather than indexed into.
    local crosshairs = hudInterfaceTagData.crosshairs
    if not crosshairs then
        return
    end
    -- Walked nested rather than flattened into a running count, so what the animation reads as
    -- crosshair 1 overlay 2 is what the tag holds as the second overlay of the first crosshair.
    -- Every overlay of every crosshair is handed over, which is what lets a weapon spread its
    -- reticle over one crosshair per piece or keep those pieces as overlays of a single one.
    for crosshairIndex = 1, #crosshairs do
        local crosshairOverlays = crosshairs[crosshairIndex].crosshairOverlays
        if crosshairOverlays then
            for overlayIndex = 1, #crosshairOverlays do
                crossHairAnimation(crosshairOverlays[overlayIndex], weaponObject, crosshairIndex,
                                   overlayIndex)
            end
        end
    end
end

return dynamicCrosshair
