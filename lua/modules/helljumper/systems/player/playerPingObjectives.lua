-- Lua libraries
local balltze = Balltze
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local castRay = Engine.physics.castRay
local utils = require "helljumper.utils"
local core = require "helljumper.systems.core.core"
local hsc = require "hsc"
local input = require "helljumper.systems.core.input"
local path = require "helljumper.systems.constants.paths"

local playerPingObjectives = {}

--------------------------------------------------------------------------------------------------
-- Configuration
--
-- Everything meant to be tuned lives between here and the end of this section, so rebinding the key
-- or moving a waypoint off the ground never means reading the code underneath.
--
-- WHAT THIS DOES. On a keypress, a ray is cast from the player's eye along the direction they are
-- looking, and a navpoint is put wherever it lands: on the level itself, or on whatever weapon,
-- vehicle or player was in the way. The waypoint takes itself down after a few seconds.
--
-- HOW IT FINDS THE POINT. Engine.physics.castRay asks the engine's own collision code the question
-- outright and hands back the hit point and what was hit. The v1 module could not ask: it spawned an
-- invisible projectile down the player's line of sight and read where it had got to on the next
-- tick, which cost an object, a tick of delay, and the whole attachment dance to work out what the
-- projectile had stuck to. None of that is here anymore.
--
-- WHAT ANCHORS THE WAYPOINT. The engine draws navpoints on cutscene flags, so the scenario has to
-- carry one flag per waypoint that can be up at once, named the way waypointFlagNameFormat says.
-- Pinging writes the hit point into a free flag and activates a navpoint on it; the flags are only
-- ever borrowed, not created, so a map without them shows nothing and says so in the log.
--------------------------------------------------------------------------------------------------

-- What asks for a ping. Any of them can be set to nil to leave it out, and they can all be on at
-- once. The names come from input.key, input.mouse and input.gamepad, which is where the codes the
-- engine actually reports are written down.
---@type integer|nil
local pingKeyCode = input.key.c
---@type integer|nil
local pingMouseButton = nil
---@type integer|nil
local pingGamepadButton = nil

-- Whether the game control this input is bound to should still see it. It only suppresses the
-- press, not the hold.
local cancelPingInput = false

-- How far the ray reaches, in world units. Nothing beyond this is pinged at all: a player aiming at
-- the sky or across a map bigger than this gets no waypoint rather than one at arm's length.
local pingRange = 100

-- What the ray is allowed to hit, as one of the collision presets Balltze names. A ping wants both
-- halves of the world at once: the level, so a wall or a floor can be pointed at, and the objects on
-- it, so a weapon or a teammate can be. "projectiles" is the only preset that carries both, and it
-- is the one the v1 module was really using anyway, since what it cast down the player's line of
-- sight was an actual projectile. The other presets each drop one half: "environment" and
-- "lineOfSight" cannot point at a dropped weapon, "objects" goes straight through the level.
---@type CollisionTestPreset
local pingCollisionPreset = "projectiles"

--- Which navpoint is drawn for a kind of thing hit, and how far above the hit point it floats
---@class PingTarget
---@field navpoint string @a waypoint arrow name out of the map's hud_globals tag
---@field zOffset number @world units above the hit point, passed to the navpoint rather than baked
--- into the flag, so the flag keeps the point that was actually hit

-- The kinds are what the ray came back with: "structure" for the level itself, and for an object the
-- type it turned out to be. Anything not named here falls to "object", which is scenery, machines
-- and controls. The offsets lift a waypoint clear of what it sits on, the way the v1 module's flat
-- 0.45 did for anything its projectile stuck to; the level gets none, since a wall pinged head-on
-- should not float above where the player pointed.
---@type table<string, PingTarget>
local pingTargets = {
    biped = {navpoint = "biped", zOffset = 0.45},
    vehicle = {navpoint = "vehicle", zOffset = 0.45},
    weapon = {navpoint = "weapon", zOffset = 0.45},
    equipment = {navpoint = "weapon", zOffset = 0.45},
    object = {navpoint = "objective", zOffset = 0.45},
    structure = {navpoint = "objective", zOffset = 0}
}

-- The cutscene flags waypoints are anchored to, and so how many can be up at once. Four of them
-- because a waypoint asked for by another player lands in one of these too, once there is something
-- to carry the request across; on this machine alone the cooldown means only one is ever up.
local waypointFlagNameFormat = "waypoint_%d"
local waypointSlotCount = 4

-- How long a waypoint stays up, and how long before another can be asked for. Both in ticks, thirty
-- to the second, which is what the v1 module's 4000 and 4300 milliseconds come to. The cooldown is
-- the longer of the two on purpose: a player holding the key down should not have their own waypoint
-- replace itself the instant it expires.
local waypointDurationTicks = utils.secondsToTicks(4)
local pingCooldownTicks = utils.secondsToTicks(4.5)

-- What a ping sounds like. Played for this client only, non positionally, since it is feedback for
-- the press rather than something happening out in the world.
local pingSoundPath = path.sound.ui.hud.grenades.fragSelected
local pingSoundGain = 1

--------------------------------------------------------------------------------------------------
-- End of configuration
--------------------------------------------------------------------------------------------------

--- A waypoint that is up right now, kept by the slot whose flag it borrowed
---@class ShownWaypoint
---@field flagName string
---@field expiryTick integer
---@type table<integer, ShownWaypoint>
local shownWaypoints = {}

-- A scenario missing the flags to anchor to is the map's doing rather than this module's, and worth
-- saying once rather than on every press.
local warnedFlags = {}

--- Take a waypoint down and give its slot back
---@param slot integer
local function deactivateWaypoint(slot)
    local shownWaypoint = shownWaypoints[slot]
    if not shownWaypoint then
        return
    end
    -- Freed before the engine is told rather than after, so a deactivation that errors still leaves
    -- the slot usable instead of holding it for the rest of the map.
    shownWaypoints[slot] = nil
    hsc.deactivate_team_nav_point_flag("player", shownWaypoint.flagName)
end

--- Put a waypoint at a point in the world, for as long as waypointDurationTicks lasts
---
--- Public because pinging is not the only thing that puts a waypoint up: a ping made by a teammate
--- arrives as a position rather than as a keypress, and comes in through here. v1 carried those over
--- rcon, which v2 has no event for on the client yet, so nothing calls this from outside the module
--- today; when a transport lands, this is the one line it needs.
---@param x number
---@param y number
---@param z number
---@param navpoint? string @a waypoint arrow name; the objective one when left out
---@param zOffset? number @world units above the point
---@return boolean @false when every slot is taken, or the flag to anchor to is missing
function playerPingObjectives.createWaypoint(x, y, z, navpoint, zOffset)
    for slot = 1, waypointSlotCount do
        if not shownWaypoints[slot] then
            local flagName = waypointFlagNameFormat:format(slot)
            local cutsceneFlag = core.findCutsceneFlag(flagName)
            if not cutsceneFlag then
                if not warnedFlags[flagName] then
                    warnedFlags[flagName] = true
                    balltze.logger.error(
                        "Scenario has no cutscene flag named {}, waypoints cannot be anchored",
                        flagName)
                end
                return false
            end
            -- Written before the navpoint is activated, so it is never drawn for a frame at
            -- wherever the flag was last left.
            local position = cutsceneFlag.position
            position.x = x
            position.y = y
            position.z = z
            -- Through the team form and not the player one because naming the local player's unit in
            -- HSC needs its index in the (players) list, which v2's Player does not carry. hsc.lua
            -- already answers that for a network game by walking every player unit, and the engine
            -- quietly refuses the ones that are not local to this machine, which leaves exactly the
            -- local player's waypoint up.
            hsc.activate_team_nav_point_flag(navpoint or pingTargets.structure.navpoint, "player",
                                             flagName, zOffset or 0)
            shownWaypoints[slot] = {
                flagName = flagName,
                expiryTick = engine.game.getTickCount() + waypointDurationTicks
            }
            return true
        end
    end
    return false
end

--- Take down every waypoint whose time is up
---@param tick integer
local function expireWaypoints(tick)
    for slot = 1, waypointSlotCount do
        local shownWaypoint = shownWaypoints[slot]
        if shownWaypoint and tick >= shownWaypoint.expiryTick then
            deactivateWaypoint(slot)
        end
    end
end



-- The object types a hit is told apart into, in the order they are asked for. Everything else is an
-- object without a navpoint of its own, so there is nothing to gain by asking about it.
local objectKinds = {"biped", "vehicle", "weapon", "equipment"}

--- What kind of thing the ray hit
---@param objectHandle ObjectHandle
---@return string @a key into pingTargets
local function getObjectKind(objectHandle)
    -- getObject hands back nothing when the object is not of the type asked for, so asking for each
    -- in turn is what tells them apart. The object does carry its type as a number, but reading that
    -- would mean keeping a copy of the engine's own enum in here to compare it against.
    for index = 1, #objectKinds do
        local objectKind = objectKinds[index]
        if getObject(objectHandle, objectKind) then
            return objectKind
        end
    end
    return "object"
end

--- Cast the ray and put a waypoint wherever it lands
---@return boolean @false when there was nothing to ping, or nowhere to put it
local function ping()
    local player = getPlayer()
    if not player then
        return false
    end
    -- Asked for apart from the biped rather than as one `player and getObject(...)` the way the rest
    -- of the project reads it: the handle the ray excludes is the player's, so the player has to be
    -- known to be there in its own right and not only through what it led to.
    local biped = getObject(player.unitHandle, "biped")
    if not biped then
        return false
    end
    -- On foot only, the way v1 asked it of vehicleObjectId: a biped riding something is parented to
    -- it, and the camera it looks through is the vehicle's rather than the one worked out below.
    if not biped.parentObject:isNull() then
        return false
    end
    local origin = core.getCameraOrigin(biped)
    if not origin then
        return false
    end
    -- castRay takes the whole displacement rather than a direction, so the range is multiplied in
    -- here: the ray spans the eye to as far as a ping reaches, and stops at the first thing on the
    -- way. aimingVector is where the unit is looking, which for a first person player is the camera.
    local aimingVector = biped.aimingVector
    local delta = {
        i = aimingVector.i * pingRange,
        j = aimingVector.j * pingRange,
        k = aimingVector.k * pingRange
    }
    -- The player's own biped is excluded, since the ray leaves from inside it and would otherwise
    -- have every ping land on the person who made it.
    local collision = castRay(origin, delta, pingCollisionPreset, player.unitHandle)
    -- What says the ray landed is the point, not the result: a miss comes back either as nothing at
    -- all or as a result typed "none" with no point on it, and asking for the point is the one
    -- question that answers both shapes. Nothing within reach is the sky, or a gap wider than
    -- pingRange; better no waypoint than one hanging in the air at the end of the ray. Logged
    -- because a ping that never lands looks, from the outside, exactly like one never asked for.
    if not collision or not collision.point then
        balltze.logger.debug("Objective ping landed nowhere within {} world units", pingRange)
        return false
    end
    -- The level itself unless the result says an object was hit, and only then is the handle beside
    -- it worth reading: on a hit against the level that handle is leftovers, and asking what kind of
    -- object it is hands back whatever object sits at that slot, which is the arrow that kept coming
    -- out different on every wall.
    local kind = "structure"
    local objectHandle = collision.objectHandle
    if core.isObjectCollision(collision) and objectHandle and not objectHandle:isNull() then
        kind = getObjectKind(objectHandle)
    end
    local target = pingTargets[kind] or pingTargets.structure
    local point = collision.point
    -- Said out loud because what the ray hit and which arrow that asks for are decided here, while
    -- what shows up on screen is the map's own hud_globals answering to the name: an arrow that
    -- looks wrong is one or the other, and this line is what tells them apart.
    balltze.logger.debug("Objective ping hit {} (result type {}), asking for the {} navpoint", kind,
                         tostring(collision.type), target.navpoint)
    return playerPingObjectives.createWaypoint(point.x, point.y, point.z, target.navpoint,
                                               target.zOffset)
end

-- What asks for a ping. The press itself is watched by the shared input module rather than by a
-- listener of this module's own: Balltze keeps one listener per event name, so a second
-- "player_input" subscription anywhere in the project would take this one down, or be taken down by
-- it, depending on which of the two ran first.
local pingAction = input.newAction {
    keyCode = pingKeyCode,
    mouseButton = pingMouseButton,
    gamepadButton = pingGamepadButton,
    cancelInput = cancelPingInput
}

-- When the next ping may be made. Kept as a tick to compare against rather than as a countdown, so
-- nothing has to be stepped on the ticks in between.
local nextPingTick = 0

--- Act on a ping asked for since the last tick, and take down whatever has outstayed its welcome
function playerPingObjectives.pingObjectives()
    local tick = engine.game.getTickCount()
    expireWaypoints(tick)
    -- Consumed whether or not it can be acted on, so a press made in a menu or inside the cooldown
    -- does not fire the moment the player is back in the game or the cooldown runs out.
    local isPingWanted = pingAction:consume()
    if not isPingWanted or tick < nextPingTick then
        return
    end
    -- A press typed into a menu belongs to the menu. hudExtensions asks this the same way.
    if engine.uiWidget.getActiveWidget() then
        return
    end
    if ping() then
        -- Only a ping that actually put something on screen starts the cooldown: aiming at the sky
        -- should not cost the player the next few seconds of pinging.
        nextPingTick = tick + pingCooldownTicks
        hsc.sound_impulse_start(pingSoundPath, "none", pingSoundGain)
    end
end

--- Take down every waypoint this module put up. Called from multiplayer's unload: require caches
--- this module, so a reload would otherwise come back believing slots are free that still have a
--- navpoint on them, with no handle left anywhere to take those down by.
function playerPingObjectives.unload()
    for slot = 1, waypointSlotCount do
        -- An unload can come in the middle of a map change, with the engine already tearing down
        -- what the script would be talking to, so a deactivation that fails is not worth taking the
        -- rest of the unload down over. The waypoints go with the map either way.
        pcall(deactivateWaypoint, slot)
    end
    -- The press waiting to be read is the shared input module's to forget, and it is told to on the
    -- same unload this is called from.
    nextPingTick = 0
    warnedFlags = {}
end

return playerPingObjectives
