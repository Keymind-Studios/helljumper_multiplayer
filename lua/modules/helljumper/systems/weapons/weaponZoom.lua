local balltze = Balltze
local engine = Engine
local core = require "helljumper.systems.core.core"

--- Camera zoom eased over time with Balltze's bezier curves; frame() advances it.
local zoom = {}

zoom.defaultDurationMs = 150
zoom.defaultCurve = "inout"

---@class ZoomTransition
---@field from number
---@field to number
---@field durationMs number
---@field curve BalltzeBezierCurve|nil @nil runs straight
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
    return core.isCurvePreset(name)
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

    transition = {
        from = from,
        to = target,
        durationMs = durationMs,
        curve = core.getCurve(curveName or zoom.defaultCurve),
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
    local from, to, curve = transition.from, transition.to, transition.curve
    if curve then
        engine.camera.setZoom(curve:getPoint(from, to, t))
    else
        engine.camera.setZoom(from + (to - from) * t)
    end
end

return zoom
