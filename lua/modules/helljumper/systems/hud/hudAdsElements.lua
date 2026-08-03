-- Lua libraries
local balltze = Balltze
local engine = Engine
local path = require "helljumper.systems.constants.objectPaths"

local adsHudElements = {}

---@class AdsHudElementSettings
---@field crosshairScale number|nil @width and height every crosshair overlay takes once fully open, nil leaves the crosshairs alone
---@field numberAnchorOffsetY integer|nil @vertical offset every number element takes while aiming, nil leaves the numbers alone
---@field openingDelayTicks number|nil @ticks the HUD stays hidden after the aim is taken
---@field openingStartScale number|nil @fraction of the final scale the crosshairs come in at
---@field openingTicks number|nil @ticks the crosshairs take to grow into the final scale
---@field openingEasePower number|nil @how hard the opening eases out: 1 runs straight, higher lands softer

-- The HUDs whose hidden elements come out while aiming, keyed by tag path the same way
-- weaponAimingDownSights keys its weapons and hudDynamicCrosshair its animations. These tags
-- author their ADS pieces out of sight already: the crosshair overlays at a scale too small to
-- be noticed and the numbers at an offset that puts them off screen. Showing them is a matter
-- of writing the visible values in and putting the authored ones back on the way out, so
-- nothing here has to know what "hidden" looks like, only what "shown" does. One value covers
-- a whole block; if some HUD ever needs its pieces scaled apart, this is where a per element
-- table would go.
--
-- A HUD only fills in the blocks it actually carries: leaving crosshairScale or
-- numberAnchorOffsetY out means that block is never written, and never restored either, which
-- is what lets a child HUD that only holds numbers keep its hands off reticle pieces it does
-- not own.
--
-- The opening fields are what makes the reticle come apart rather than pop in: after the aim is
-- taken the HUD waits openingDelayTicks still hidden, then comes in at openingStartScale of the
-- final scale and grows into it over openingTicks. Ticks run at 30 a second, so these are small
-- numbers, and they take fractions: the opening is stepped once a frame, not once a tick, so
-- half a tick is a real duration and not a rounding. Leaving openingTicks out brings the HUD
-- straight in at its final scale.
--
-- openingEasePower is how the growth is spread over that time. 1 runs straight from the start
-- scale to the final one, 2 and up spend the first half of the time covering most of the
-- distance and ease into the last of it, which reads as quicker and lands softer at once.
---@type table<string, AdsHudElementSettings>
local adsHuds = {
    -- AssaultRifleMA38
    [path.weaponHudInterface.ma38ADS_hud] = {
        crosshairScale = 0.375,
        openingDelayTicks = 3,
        openingStartScale = 0.20,
        openingTicks = 4
    },
    -- AssaultRifleMA38 Mask
    [path.weaponHudInterface.ma38ADS_mask_hud] = {
        crosshairScale = 4,
        openingDelayTicks = 1,
        openingStartScale = 3,
        openingTicks = 4
    },
    -- AssaultRifleMA38, the total number of rounds riding inside the HUD above
    --[path.weaponHudInterface.ma38ADS_total_number_hud] = {
    --    numberAnchorOffsetY = -15,
    --    openingDelayTicks = 7,
    --    openingTicks = 4
    --},
    -- AssaultRifleMA38, the loaded number of rounds riding inside the HUD above
    --[path.weaponHudInterface.ma38ADS_hud_loaded_number_hud] = {
    --    numberAnchorOffsetY = -10,
    --    openingDelayTicks = 7,
    --    openingTicks = 4
    --}

}

---@class ShownAdsHud
---@field path string
---@field settings AdsHudElementSettings
---@field originalValues {crosshairScales: {width: number, height: number}[]|nil, numberAnchorOffsetsY: integer[]|nil}|nil
---@field crosshairScale number|nil @the scale last written, nil while nothing has been written

-- The HUDs showing their ADS elements right now, each carrying the values it held before they
-- were written: a snapshot only means anything beside the HUD it was taken from, since the
-- restore walks the tag in the same order the write did. A HUD's snapshot stays nil until its
-- first write actually lands, which is what lets the opening delay pass without the tag being
-- touched at all.
---@type ShownAdsHud[]
local shownHuds = {}
-- One clock for all of them: the HUDs of a weapon are taken and dropped together, so they open
-- off the same moment even when each one times itself differently. It is a timestamp rather than
-- a tick count because the opening is stepped once a frame: counting ticks would quantise the
-- growth into the 30 steps a second the game thinks in, which is what makes a four tick opening
-- read as four jumps instead of a movement, no matter how many frames were drawn in between.
---@type BalltzeTimestamp|nil
local shownHudsTimestamp = nil
-- Ticks are what the settings are written in, milliseconds are what the clock hands back.
local tickMilliseconds = 1000 / 30
-- How hard the opening eases out when a HUD does not say. Straight line growth is what looks
-- mechanical, so the default spends its speed early and settles into the final scale.
local defaultOpeningEasePower = 3

---@param tagPath string
---@return WeaponHudInterface|nil
local function getHudTagData(tagPath)
    -- Looked up on each write instead of held as an upvalue: tag data lives in the loaded map,
    -- so a cached view would dangle the moment the map changes.
    local tagHandle = engine.tag.lookupTag(tagPath, "weapon_hud_interface")
    if not tagHandle then
        balltze.logger.error("ADS HUD tag does not exist: {}", tagPath)
        return nil
    end
    return engine.tag.getTagData(tagHandle, "weapon_hud_interface")
end

--- Every crosshair overlay of a HUD, flattened out of the groups holding them
---@param hudTagData WeaponHudInterface
---@return WeaponHudInterfaceCrosshairsCrosshairOverlays[]
local function getCrosshairOverlays(hudTagData)
    -- Same reason hudDynamicCrosshair flattens: a HUD is free to keep one overlay per crosshair
    -- group or several inside a single group, and this module does not care which, it touches
    -- all of them.
    local overlays = {}
    -- A tag block with nothing in it comes back as nil rather than as an array of length zero,
    -- and a HUD that carries only numbers is exactly that, so the block is checked before it is
    -- walked instead of being taken for granted.
    local crosshairs = hudTagData.crosshairs
    if not crosshairs then
        return overlays
    end
    for groupIndex = 1, #crosshairs do
        local groupOverlays = crosshairs[groupIndex].crosshairOverlays
        if groupOverlays then
            for overlayIndex = 1, #groupOverlays do
                overlays[#overlays + 1] = groupOverlays[overlayIndex]
            end
        end
    end
    return overlays
end

--- The number elements of a HUD, as an array even when the tag carries none
---@param hudTagData WeaponHudInterface
---@return WeaponHudInterfaceNumberElement[]
local function getNumberElements(hudTagData)
    return hudTagData.numberElements or {}
end

--- Put a HUD's elements where aiming wants them, keeping what was there the first time
---@param shownHud ShownAdsHud
---@param crosshairScale number|nil @nil when this HUD has no crosshairs of its own to open
local function writeHudElements(shownHud, crosshairScale)
    local settings = shownHud.settings
    local hudTagData = getHudTagData(shownHud.path)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    local crosshairOverlays = getCrosshairOverlays(hudTagData)
    local numberElements = getNumberElements(hudTagData)
    -- Taken on the first write of an aim and not on every one of them: past that point what the
    -- tag holds is this module's own doing, and snapshotting it again would save the opening
    -- scale as if it were the authored one. Only the blocks this HUD writes are saved, so the
    -- restore has nothing to say about the ones it leaves alone.
    if not shownHud.originalValues then
        local originalValues = {}
        if crosshairScale then
            local crosshairScales = {}
            for index = 1, #crosshairOverlays do
                local overlay = crosshairOverlays[index]
                crosshairScales[index] = {
                    width = overlay.widthScale,
                    height = overlay.heightScale
                }
            end
            originalValues.crosshairScales = crosshairScales
        end
        if settings.numberAnchorOffsetY then
            local numberAnchorOffsetsY = {}
            for index = 1, #numberElements do
                numberAnchorOffsetsY[index] = numberElements[index].anchorOffset.y
            end
            originalValues.numberAnchorOffsetsY = numberAnchorOffsetsY
        end
        shownHud.originalValues = originalValues
    end
    if crosshairScale then
        for index = 1, #crosshairOverlays do
            crosshairOverlays[index].widthScale = crosshairScale
            crosshairOverlays[index].heightScale = crosshairScale
        end
    end
    -- The numbers have nothing to open, they just come along on the first frame the reticle is
    -- drawn, and rewriting the same offset while the crosshairs grow costs nothing.
    if settings.numberAnchorOffsetY then
        for index = 1, #numberElements do
            numberElements[index].anchorOffset.y = settings.numberAnchorOffsetY
        end
    end
    shownHud.crosshairScale = crosshairScale
end

--- Step a HUD's opening, if it has anything left to do this frame
---@param shownHud ShownAdsHud
local function updateShownHud(shownHud)
    local settings = shownHud.settings
    local delayTicks = settings.openingDelayTicks or 0
    -- Fractional, unlike the tick count this used to be worked out from: what is wanted is how
    -- far into the opening this frame lands, and frames do not fall on tick boundaries.
    local elapsedTicks = 0
    if shownHudsTimestamp then
        elapsedTicks = shownHudsTimestamp:getElapsedMilliseconds() / tickMilliseconds
    end
    if elapsedTicks < delayTicks then
        return
    end
    local crosshairScale
    if settings.crosshairScale then
        local openingTicks = settings.openingTicks or 0
        local startScale = settings.openingStartScale or 1
        local progress = 1
        if openingTicks > 0 then
            progress = (elapsedTicks - delayTicks) / openingTicks
            if progress > 1 then
                progress = 1
            end
        end
        -- Eased out rather than run straight, so the reticle flies open and settles into its
        -- scale instead of stopping dead on the last frame of the opening.
        local easePower = settings.openingEasePower or defaultOpeningEasePower
        local easedProgress = 1 - (1 - progress) ^ easePower
        crosshairScale = settings.crosshairScale * (startScale + (1 - startScale) * easedProgress)
    end
    -- The snapshot standing in for "the first write already happened" is what makes a HUD with
    -- nothing to animate, or one already fully open, cost nothing for the rest of the aim.
    if shownHud.originalValues and crosshairScale == shownHud.crosshairScale then
        return
    end
    writeHudElements(shownHud, crosshairScale)
end

--- Put back what a HUD held before its ADS elements were written
---@param shownHud ShownAdsHud
local function hideHudElements(shownHud)
    -- There is nothing to put back when the aim is dropped before the opening delay runs out:
    -- the tag was never written, so it still holds the values it was authored with.
    local originalValues = shownHud.originalValues
    if not originalValues then
        return
    end
    local hudTagData = getHudTagData(shownHud.path)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    -- Each element is only put back if the snapshot has something for it, so a tag that came
    -- back with a different element count restores what it can instead of erroring out.
    local crosshairScales = originalValues.crosshairScales
    if crosshairScales then
        local crosshairOverlays = getCrosshairOverlays(hudTagData)
        for index = 1, #crosshairOverlays do
            local originalScale = crosshairScales[index]
            if originalScale then
                crosshairOverlays[index].widthScale = originalScale.width
                crosshairOverlays[index].heightScale = originalScale.height
            end
        end
    end
    local numberAnchorOffsetsY = originalValues.numberAnchorOffsetsY
    if numberAnchorOffsetsY then
        local numberElements = getNumberElements(hudTagData)
        for index = 1, #numberElements do
            local originalOffsetY = numberAnchorOffsetsY[index]
            if originalOffsetY then
                numberElements[index].anchorOffset.y = originalOffsetY
            end
        end
    end
    --balltze.logger.debug("ADS HUD elements hidden on {}", shownHud.path)
end

--- Whether the HUDs on show are already the ones wanted, in the same order
---@param wantedHudPaths string[]
---@return boolean
local function isShowingHuds(wantedHudPaths)
    if #shownHuds ~= #wantedHudPaths then
        return false
    end
    for index = 1, #shownHuds do
        if shownHuds[index].path ~= wantedHudPaths[index] then
            return false
        end
    end
    return true
end

--- Put back every HUD on show and forget them
local function hideShownHuds()
    local huds = shownHuds
    -- Cleared before the writes rather than after: a tag that cannot be reached now will not be
    -- reachable on the next tick either, and holding on to the snapshots would only mean writing
    -- stale values into whatever map loads next.
    shownHuds = {}
    for index = 1, #huds do
        hideHudElements(huds[index])
    end
end

--- Step every HUD on show
local function updateShownHuds()
    for index = 1, #shownHuds do
        updateShownHud(shownHuds[index])
    end
end

-- The aim itself is worked out once a tick, but an opening stepped that coarsely is a staircase:
-- a four tick opening would only ever be four writes, however smoothly the game is running. The
-- frame event is what turns those four writes into one per drawn frame.
local isFrameListenerRegistered = false
local frameListener

local function ensureFrameListener()
    if isFrameListenerRegistered then
        return
    end
    -- Subscribed on first use rather than at load, the same way weaponAimingDownSights does it:
    -- require caches this module, so after an unload it would come back believing it still had a
    -- listener and never subscribe again.
    frameListener = balltze.addEventListener("frame", updateShownHuds)
    isFrameListenerRegistered = true
end

--- Show the ADS elements of a set of HUDs, putting back those of any HUD already showing them.
--- Paths this module has no settings for are ignored, so a weapon is free to name a HUD that has
--- nothing to reveal. Called with nil or an empty list, it only puts things back. Meant to be
--- called every tick with whatever the aim wants right now; the opening itself is stepped on
--- frames, so what this decides is which HUDs are opening, not how far along they are.
---@param tagPaths string[]|nil @weapon_hud_interface tag paths
function adsHudElements.setShownHuds(tagPaths)
    ensureFrameListener()
    local wantedHudPaths = {}
    if tagPaths then
        for index = 1, #tagPaths do
            local tagPath = tagPaths[index]
            if adsHuds[tagPath] then
                wantedHudPaths[#wantedHudPaths + 1] = tagPath
            end
        end
    end
    if not isShowingHuds(wantedHudPaths) then
        hideShownHuds()
        for index = 1, #wantedHudPaths do
            local tagPath = wantedHudPaths[index]
            -- The opening is timed from here and nothing is written yet, which is the delay
            -- itself: the tags keep their authored values until the first frame with something to
            -- show, so an aim taken and dropped inside the delay never reaches the screen.
            shownHuds[index] = {path = tagPath, settings = adsHuds[tagPath]}
            --balltze.logger.debug("ADS HUD elements opening on {}", tagPath)
        end
        -- Created on the first aim rather than at load, since a clock started at load would have
        -- every HUD believing its opening finished long ago.
        if shownHudsTimestamp then
            shownHudsTimestamp:reset()
        else
            shownHudsTimestamp = balltze.createTimestamp()
        end
    end
    -- Stepped here too, not only on the frame event, so the first write lands on the tick the
    -- delay runs out on even if the event is not reaching this module.
    updateShownHuds()
end

--- Drop the tracked state without writing anything back. Unload runs during map teardown, where
--- reaching into tag data is not safe; the values a restore would write are the ones authored in
--- the tag, which a map load brings back on its own anyway. The frame listener does come off,
--- since it belongs to the plugin rather than to the map.
function adsHudElements.unload()
    shownHuds = {}
    if frameListener and frameListener.remove then
        frameListener:remove()
    end
    frameListener = nil
    isFrameListenerRegistered = false
end

return adsHudElements
