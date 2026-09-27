local balltze = Balltze
local engine = Engine

--- Camera zoom eased over time with Balltze's bezier curves; frame() advances it.
local zoom = {}

zoom.defaultDurationMs = 150
zoom.defaultCurve = "inout"

-- One word each, since the console splits arguments on spaces.
local presets = {
    linear = "linear",
    ["in"] = "ease in",
    out = "ease out",
    inout = "ease in out",
}

---@type table<string, BalltzeBezierCurve>
local curves = {}

---@class ZoomTransition
---@field from number
---@field to number
---@field durationMs number
---@field curve BalltzeBezierCurve
---@field timestamp BalltzeTimestamp

---@type ZoomTransition|nil
local transition

--- Engine.camera only exists in the Balltze builds that added setZoom.
---@return boolean
function zoom.isAvailable()
    return engine.camera ~= nil and engine.camera.setZoom ~= nil
end

---@param name string
---@return boolean
function zoom.isCurve(name)
    return presets[name] ~= nil
end

---@return number
function zoom.current()
    return engine.camera.getZoom()
end

--- Ease from the current zoom to `target`; a new call takes over from wherever the last one had got to.
---@param target number
---@param durationMs? number
---@param curveName? string
function zoom.to(target, durationMs, curveName)
    durationMs = durationMs or zoom.defaultDurationMs
    local from = engine.camera.getZoom()
    if durationMs <= 0 or from == target then
        transition = nil
        engine.camera.setZoom(target)
        return
    end

    local preset = presets[curveName or zoom.defaultCurve]
    curves[preset] = curves[preset] or balltze.createBezierCurve(preset)
    transition = {
        from = from,
        to = target,
        durationMs = durationMs,
        curve = curves[preset],
        timestamp = balltze.createTimestamp(),
    }
end

--- Called from the plugin's only frame listener, so the zoom moves as smoothly as the game draws.
function zoom.frame()
    if transition == nil then
        return
    end

    local t = transition.timestamp:getElapsedMilliseconds() / transition.durationMs
    if t >= 1 then
        engine.camera.setZoom(transition.to)
        transition = nil
        return
    end
    engine.camera.setZoom(transition.curve:getPoint(transition.from, transition.to, t))
end

return zoom
