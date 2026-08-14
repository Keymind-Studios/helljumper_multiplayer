local utils = {}

--- Converts seconds to ticks (30 ticks per second)
--- @param seconds number
function utils.secondsToTicks(seconds)
    return 30 * seconds
end

--- Converts ticks to seconds (30 ticks per second)
---@param ticks number
function utils.ticksToSeconds(ticks)
    return math.round(ticks / 30)
end

-- Converts ticks to milliseconds (30 ticks per second)
-- every second has 1000 milliseconds, so 1 tick = 1000 / 30 milliseconds
---@param ticks number
function utils.ticksToMillisecs(ticks)
    return ticks * (1000 / 30)
end


--- Converts minutes to ticks (30 ticks per second)
---@param minutes number
---@return number
function utils.minutesToTicks(minutes)
    return utils.secondsToTicks(minutes * 60)
end

--- Converts seconds to milliseconds
---@param seconds number
function utils.secondsToMillisecs(seconds)
    return seconds * 1000
end

return utils