local engine = Engine
local balltze = Balltze
local objectTypes = Engine.tag.objectType
local getPlayer = Engine.gameState.getPlayer
local path = require "helljumper.systems.constants.objectPaths"
local weapons = require "helljumper.systems.constants.weapons"
--local blam = require "blam"

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
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.ma38_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitial = 4
        local reticleAdditional = 16
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            crosshair.anchorOffset.x = floor(-reticleInitial - heat) -- Left
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.x = ceil(reticleInitial + heat) -- Right
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.y = floor(-reticleInitial - heat) -- Up
        elseif crosshairIndex == 4 then
            crosshair.anchorOffset.y = ceil(reticleInitial + heat) -- Down
        elseif crosshairIndex == 5 then
            crosshair.heightScale = scaleDot * heatOrig
            crosshair.widthScale = scaleDot * heatOrig
        end
    end,

    -- LmgSaw
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.saw_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitial = 4
        local reticleAdditional = 16
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            crosshair.anchorOffset.x = floor(-reticleInitial - heat) -- Left
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.x = ceil(reticleInitial + heat) -- Right
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.y = floor(-reticleInitial - heat) -- Up
        elseif crosshairIndex == 4 then
            crosshair.anchorOffset.y = ceil(reticleInitial + heat) -- Down
        elseif crosshairIndex == 5 then
            crosshair.heightScale = scaleDot * heatOrig
            crosshair.widthScale = scaleDot * heatOrig
        end
    end,

    -- Needler
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.needler_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitial = 12
        local reticleAdditional = 15
        local dotReticleInitial = 0.08
        local dotReticleAdditional = 0
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local animTimer = weaponObject.readyTicks + weaponObject.magazines[1].reloadTicksRemaining
        if animTimer > 20 then
            animTimer = 20
        end
        local heat = weaponObject.heat * reticleAdditional + animTimer / 2
        local heatOrig = weaponObject.heat
        local scaleDot = dotReticleInitial + heat * dotReticleAdditional
        if crosshairIndex == 1 then
            crosshair.anchorOffset.x = floor(-reticleInitial - heat) -- Left
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.x = ceil(reticleInitial + heat) -- Right
        elseif crosshairIndex == 3 then
            crosshair.heightScale = scaleDot * heatOrig
            crosshair.widthScale = scaleDot * heatOrig
        end
    end,

    -- Disruptor
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.zapper_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitialPos = 0
        local reticleAdditionalPos = 1.5
        local reticleScaleInitial = 0.22
        local reticleScaleAdditional = 0.26
        local reticleScaleZero = 0
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.anchorOffset.x = floor(
                                           -reticleInitialPos - reticleAddPos * heat - animTimerA /
                                               4 * 0.9) -- Left
            crosshair.widthScale = reticleScale
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.x = ceil(reticleInitialPos + reticleAddPos * heat + animTimerA /
                                                4 * 0.9) -- Right
            crosshair.widthScale = reticleScale
        elseif crosshairIndex == 3 then
            crosshair.widthScale = reticleScaleZero - reticleScaleInitial + animTimerA / 42 -
                                       animTimerB / 70
            crosshair.heightScale = reticleScaleZero - reticleScaleInitial + animTimerA / 42
        end
    end,

    -- BattleRifle65H
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.br65h_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local zoomMaskInitial = 3
        local zoomMaskAdditional = 0.22
        local zoomInitial = 0.21
        local zoomAdditional = 0.018
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        if crosshairIndex == 1 then
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            crosshair.widthScale = scaleZoom
            crosshair.heightScale = scaleZoom
        end
    end,

    -- DMR392
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.dmr_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleAddPos = 3
        local reticleScaleInitial = 0.15
        local reticleScaleAdditional = 0.1
        local zoomMaskInitial = 0.65
        local zoomMaskAdditional = 0.06
        local zoomInitial = 0.5
        local zoomAdditional = 0.06
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        local reticlePosition = heat * reticleAddPos
        if crosshairIndex == 1 then
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            crosshair.widthScale = scaleZoom
            crosshair.heightScale = scaleZoom
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.x = floor(-reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 4 then
            crosshair.anchorOffset.x = ceil(reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 5 then
            crosshair.anchorOffset.y = floor(-reticlePosition * (4 / 3) * 0.98)
        elseif crosshairIndex == 6 then
            crosshair.anchorOffset.y = ceil(reticlePosition * (4 / 3) * 0.98)
        end
    end,

    -- ShotgunM90
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.m90_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleScaleInitial = 0.23
        local reticleScaleAdditional = 0.07
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.widthScale = reticleScale
            crosshair.heightScale = reticleScale
        end
    end,

    ---@class MagnumM6S
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.m6s_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitial = 0.2
        local reticleAdditional = 0.6
        local zoomMaskInitial = 1.5
        local zoomMaskAdditional = 0.22
        local zoomInitial = 0.44
        local zoomAdditional = 0.057
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            crosshair.widthScale = scaleZoom
            crosshair.heightScale = scaleZoom
        elseif crosshairIndex == 3 then
            crosshair.widthScale = scaleReticle + readyTime / 8 * 0.5 + reloadTime / 8 * 0.4
            crosshair.heightScale = scaleReticle + readyTime / 8 * 0.5 + reloadTime / 8 * 0.4
        elseif crosshairIndex == 4 then
            crosshair.widthScale = 0.165
            crosshair.heightScale = 0.165
        end
    end,

    -- SpnkrRocketLauncher
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.spnkr_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local zoomMaskInitial = 1.7
        local zoomMaskAdditional = 0.6
        local zoomInitial = 0.5
        local zoomAdditional = 0.15
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
        local heat = weaponObject.heat
        local scaleMask = zoomMaskInitial + heat * zoomMaskAdditional
        local scaleZoom = zoomInitial + heat * zoomAdditional
        if crosshairIndex == 1 then
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            crosshair.widthScale = scaleZoom
            crosshair.heightScale = scaleZoom
        end
    end,

    -- VK78Commando
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.vk78_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
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
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 3 then
            crosshair.widthScale = scaleFull
            crosshair.heightScale = scaleFull
        elseif crosshairIndex == 4 then
            crosshair.widthScale = scaleFull
            crosshair.heightScale = scaleFull
        elseif crosshairIndex == 5 then
            crosshair.widthScale = scaleBlur
            crosshair.heightScale = scaleBlur
        elseif crosshairIndex == 6 then
            crosshair.widthScale = scaleBlur
            crosshair.heightScale = scaleBlur
        elseif crosshairIndex == 7 then
            crosshair.widthScale = scaleReticle
            crosshair.heightScale = scaleReticle
        elseif crosshairIndex == 8 then
            crosshair.anchorOffset.x = floor(-reticleInitPos - posReticleAdd * heat) -- Left
            crosshair.widthScale = scaleStroke - strokeLess
            crosshair.heightScale = scaleStroke
        elseif crosshairIndex == 9 then
            crosshair.anchorOffset.x = ceil(reticleInitPos + posReticleAdd * heat) -- Right
            crosshair.widthScale = scaleStroke - strokeLess
            crosshair.heightScale = scaleStroke
        elseif crosshairIndex == 10 then
            crosshair.anchorOffset.y = floor(-reticleInitPos - posReticleAdd * heat) -- Left
            crosshair.widthScale = scaleStroke
            crosshair.heightScale = scaleStroke - strokeLess
        elseif crosshairIndex == 11 then
            crosshair.anchorOffset.y = ceil(reticleInitPos + posReticleAdd * heat) -- Left
            crosshair.widthScale = scaleStroke
            crosshair.heightScale = scaleStroke - strokeLess
        elseif crosshairIndex == 12 then
            crosshair.heightScale = scaleDot * heatOrig
            crosshair.widthScale = scaleDot * heatOrig
        end
    end,

    -- SniperRifle
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.sniper_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
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
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.widthScale = scaleReticle + readyTime / 25 * 0.5
            crosshair.heightScale = scaleReticle + readyTime / 25 * 0.5
        elseif crosshairIndex == 3 then
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 4 then
            crosshair.widthScale = scaleZoom
            crosshair.heightScale = scaleZoom
        elseif crosshairIndex == 5 then
            crosshair.widthScale = scaleZoomLevels
            crosshair.heightScale = scaleZoomLevels
            crosshair.anchorOffset.x = floor(zoomLevelPosInitial -positionZoomLevels * heat - reloadTime / 2 * 0.9)
        end
    end,

    -- Skewer
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.skewer_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local zoomFullInitial = 0.47
        local zoomFullAdditional = 0.1
        local zoomMaskInitial = 0.88
        local zoomMaskAdditional = 0.1
        local reticleAddPos = 10
        local reticleAddScale = 0.25
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.widthScale = scaleMask
            crosshair.heightScale = scaleMask
        elseif crosshairIndex == 2 then
            crosshair.widthScale = scaleFull
            crosshair.heightScale = scaleFull
        elseif crosshairIndex == 3 then
            crosshair.widthScale = scaleFull
            crosshair.heightScale = scaleFull
        elseif crosshairIndex == 4 then
            crosshair.anchorOffset.x = floor(-posReticleAdd * heat - animTimer / 2 * 0.9)
        elseif crosshairIndex == 5 then
            crosshair.anchorOffset.x = ceil(posReticleAdd * heat + animTimer / 2 * 0.9)
        elseif crosshairIndex == 6 then
            crosshair.widthScale = -reticleAddScale + animTimer / 30
            crosshair.heightScale = -reticleAddScale + animTimer / 30
        end
    end,

    -- StormRifle
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.storm_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleAddPos = 3
        local reticleScaleInitial = 0.3
        local reticleScaleAdditional = 0.07
        local dotReticleInitial = 0.09
        local dotReticleAdditional = 0
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.anchorOffset.x = floor(-reticlePos)
            crosshair.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.x = ceil(reticlePos)
            crosshair.anchorOffset.y = ceil(reticlePos)
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.x = ceil(reticlePos)
            crosshair.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 4 then
            crosshair.anchorOffset.x = floor(-reticlePos)
            crosshair.anchorOffset.y = ceil(reticlePos)
        elseif crosshairIndex == 5 then
            crosshair.widthScale = scaleDot * heatOrig
            crosshair.heightScale = scaleDot * heatOrig
        elseif crosshairIndex == 6 then
            crosshair.widthScale = -reticleScale
            crosshair.heightScale = -reticleScale
        end
    end,

    -- PlasmaPistol
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.pp_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleAddPos = 3.5
        local reticleScaleInitial = 0.19
        local reticleScaleAdditional = 0.08
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.anchorOffset.x = floor(-reticlePos)
            crosshair.anchorOffset.y = ceil(reticlePos * 0.45)
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.y = floor(-reticlePos)
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.x = ceil(reticlePos)
            crosshair.anchorOffset.y = ceil(reticlePos * 0.45)
        end
    end,

    -- PlasmaCaster
    ---@param weaponHudTag MetaEngineTag|MetaEngineWeaponHudInterfaceTag
    ---@param weaponObject MetaEngineBaseObject|MetaEngineWeaponObject
    ---@param crosshairIndex integer
    [path.weapons.caster_weap] = function(weaponHudTag, weaponObject, crosshairIndex)
        local reticleInitPos = 2
        local reticleAddPos = 3
        local reticleScaleInitial = 0.18
        local reticleScaleAdditional = 0.025
        local crosshair =
            weaponHudTag.data.crosshairs.elements[1].crosshairOverlays.elements[crosshairIndex]
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
            crosshair.anchorOffset.x = floor(-reticleInitPos - reticlePos)
            crosshair.anchorOffset.y = ceil(reticleInitPos + reticlePos * 0.45)
        elseif crosshairIndex == 2 then
            crosshair.anchorOffset.y = floor(-reticleInitPos - reticlePos)
        elseif crosshairIndex == 3 then
            crosshair.anchorOffset.x = ceil(reticleInitPos + reticlePos)
            crosshair.anchorOffset.y = ceil(reticleInitPos + reticlePos * 0.45)
        end
    end
}

--local lastWeaponTagHandle

function dynamicCrosshair.dynamicReticles()
    local player = getPlayer()
    if not player then
        return
    end
    local biped = getObject(player.objectHandle, objectTypes.biped)
    if not biped then
        return
    end
    --local blamBiped = blam.biped(get_object(player.objectHandle.value))
    --assert(blamBiped, "Biped tag must exist")
    -- TODO Ask Mango if "weaponSlot" prop exists in Balltze, to remove blam dependency
    -- The function is biped.currentWeaponId
    local weaponObjectHandle = biped.weapons[biped.currentWeaponId + 1]
    if not weaponObjectHandle or (weaponObjectHandle and weaponObjectHandle:isNull()) then
        return
    end
    local weaponObject = getObject(weaponObjectHandle, objectTypes.weapon)
    if not weaponObject then
        return
    end
    local weaponTag = table.find(weapons.weaponTag, function(tag)
        return tag.handle.value == weaponObject.tagHandle.value
    end)
    if not weaponTag then
        logger:error("Weapon tag constant must exist on weapon list")
        return
    end
    local crossHairAnimation = crossHairAnimations[weaponTag.path]
    if crossHairAnimation then
        local hudInterfaceTag = table.find(weapons.weaponHudInterfaceTag, function(tag)
            return weaponTag.data.hudInterface.tagHandle.value == tag.handle.value
        end)
        if not hudInterfaceTag then
            logger:error("HUD weapon interface must exist on hud list")
            return
        end
        for crosshairIndex = 1, hudInterfaceTag.data.crosshairs.count do
            crossHairAnimation(hudInterfaceTag, weaponObject, crosshairIndex)
        end
    end
end

return dynamicCrosshair
