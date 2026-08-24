local balltze = Balltze
local engine = Engine
local script = require "script"

local hudTextChannels = {}

local function getHudChannel(channel)
    local existing = hudTextChannels[channel]
    if existing then
        return existing
    end
    local created = {}
    hudTextChannels[channel] = created
    return created
end

local function parseHudColor(...)
    local a, r, g, b = ...
    if type(a) == "number" and type(r) == "number" and type(g) == "number" and
        type(b) == "number" then
        local colorKey = table.concat({a, r, g, b}, ":")
        return {
            a = a,
            r = r,
            g = g,
            b = b
        }, colorKey
    end
    return nil, nil
end

local function beginHudChannelFrame(channel)
    local channelTexts = getHudChannel(channel)
    for _, entry in pairs(channelTexts) do
        entry.seen = false
    end
end

local function endHudChannelFrame(channel)
    local channelTexts = getHudChannel(channel)
    for key, entry in pairs(channelTexts) do
        if not entry.seen then
            entry.text:remove()
            channelTexts[key] = nil
        end
    end
end

local function clearHudChannel(channel)
    local channelTexts = getHudChannel(channel)
    for key, entry in pairs(channelTexts) do
        entry.text:remove()
        channelTexts[key] = nil
    end
end

-- v2 has no bounding box to lay text out in the way chimera's draw_text did: a text is placed at one
-- point and justified about it. The box the callers still pass is collapsed to that point here, so
-- "center" lands in the middle of the box it was measured against rather than at its left edge.
---@param left integer
---@param right integer|nil
---@param align string|nil
---@return integer x
---@return string justification
local function collapseBounds(left, right, align)
    if align == "center" and right then
        return math.floor((left + right) / 2), "center"
    elseif align == "right" and right then
        return right, "right"
    end
    return left, "left"
end

local function drawTextCompat(channel, text, left, top, right, bottom, font, align, ...)
    -- font is chimera's name for one of the stock fonts; v2 wants a vector_font tag handle instead,
    -- and the nil one it gets falls back to the globals terminal font, which is what this drew with.
    local _ = {bottom, font}
    -- Guarded rather than assumed because this module also loads on the server, where the frame
    -- channel is a shim and nothing is ever drawn.
    if not (engine.interface and engine.interface.addText and left ~= nil and top ~= nil) then
        engine.terminal.print("{}", tostring(text))
        return
    end

    local x, justification = collapseBounds(left, right, align)
    local channelTexts = getHudChannel(channel)
    local key = tostring(x) .. ":" .. tostring(top)
    local hudColor, colorKey = parseHudColor(...)
    local entry = channelTexts[key]

    if entry and entry.colorKey ~= colorKey then
        if entry.text then
            entry.text:remove()
        end
        entry = nil
        channelTexts[key] = nil
    end

    if not entry then
        -- v2 takes an options table rather than a bare colour. Passing the colour straight through
        -- drew the text in the default white and said nothing about it.
        local hudText = engine.interface.addText(tostring(text), x, top,
                                                 {color = hudColor, justification = justification})
        channelTexts[key] = {text = hudText, colorKey = colorKey, seen = true}
        return
    end

    -- setText is a method, so it takes the colon: called with a dot it went in without a self.
    if entry.text then
        entry.text:setText(tostring(text))
    end
    entry.seen = true
end

DebugTimes = {}

local bounds = {left = 15, top = 45, right = 640, bottom = 480}
local profileWindowTicks = 30 * 5
local profilerState = {byFunc = {}, snapshot = {threads = {}, ticks = 0, period = 0}}

-- ARGB color values
local textColor = {
    default = {1.0, 0.45, 0.72, 1.0},
    white = {1.0, 1.0, 1.0, 1.0},
    info = {1.0, 0.8, 0.8, 0.8},
    error = {1.0, 0.8, 0.45, 1.0},
    warning = {1.0, 0.8, 0.45, 1.0}
}

local function getScriptThreadName(scriptThread)
    -- return scriptThread.referenceFile or scriptThread.referenceName or ("thread #" .. tostring(scriptThread.index))
    local referenceFile = tostring(scriptThread.referenceFile or "")
    if referenceFile ~= "" then
        local filePath, fileDetails = referenceFile:match("^(.-)(:%d+.*)$")
        if filePath and fileDetails then
            local fileName = filePath:match("([^/\\]+)$") or filePath
            return fileName .. fileDetails
        end
        return referenceFile:match("([^/\\]+)$") or referenceFile
    end
    return scriptThread.referenceName or ("thread #" .. tostring(scriptThread.index))
end

local function isVisibleScriptThread(scriptThread)
    return not (scriptThread.referenceFile and scriptThread.referenceFile:find("script.lua"))
end

local function getCurrentScriptThreads()
    local threads = {}
    for _, scriptThread in ipairs(script.getStatus()) do
        if isVisibleScriptThread(scriptThread) then
            table.insert(threads, scriptThread)
        end
    end
    return threads
end

local function getMaxSample(samples)
    local maxSample = 0
    for _, value in ipairs(samples) do
        if value > maxSample then
            maxSample = value
        end
    end
    return maxSample
end

local function createWindowState(totalRunTime)
    return {lastTotalRunTime = totalRunTime or 0, samples = {}, sum = 0, max = 0}
end

local function pushWindowSample(windowState, sample)
    table.insert(windowState.samples, sample)
    windowState.sum = windowState.sum + sample

    if #windowState.samples > profileWindowTicks then
        local removed = table.remove(windowState.samples, 1) or 0
        windowState.sum = windowState.sum - removed
    end

    windowState.max = getMaxSample(windowState.samples)
end

local function updateThreadWindows(currentThreads)
    local alive = {}

    for _, scriptThread in ipairs(currentThreads) do
        local key = scriptThread.func
        alive[key] = true

        local totalRunTime = scriptThread.totalRunTime or 0
        local windowState = profilerState.byFunc[key]
        if not windowState then
            windowState = createWindowState(totalRunTime)
            profilerState.byFunc[key] = windowState
        end

        local sample = totalRunTime - (windowState.lastTotalRunTime or 0)
        if sample < 0 then
            sample = totalRunTime
        end
        windowState.lastTotalRunTime = totalRunTime
        pushWindowSample(windowState, sample)
    end

    for key, _ in pairs(profilerState.byFunc) do
        if not alive[key] then
            profilerState.byFunc[key] = nil
        end
    end
end

local function buildSnapshotThreads(currentThreads)
    local snapshotThreads = {}

    for _, scriptThread in ipairs(currentThreads) do
        local windowState = profilerState.byFunc[scriptThread.func]
        local sampleCount = windowState and #windowState.samples or 0
        local windowRunTime = windowState and windowState.sum or 0
        local windowAverageRunTime = sampleCount > 0 and (windowRunTime / sampleCount) or 0
        local windowMaxRunTime = windowState and windowState.max or 0

        table.insert(snapshotThreads, {
            index = scriptThread.index,
            type = scriptThread.type,
            referenceName = scriptThread.referenceName,
            referenceFile = scriptThread.referenceFile,
            lastRunTime = scriptThread.lastRunTime,
            totalRunTime = scriptThread.totalRunTime,
            runCount = scriptThread.runCount,
            windowRunTime = windowRunTime,
            windowAverageRunTime = windowAverageRunTime,
            windowMaxRunTime = windowMaxRunTime,
            windowSamples = sampleCount,
            windowSize = profileWindowTicks
        })
    end

    table.sort(snapshotThreads, function(left, right)
        local leftWindowAverageRunTime = left.windowAverageRunTime or 0
        local rightWindowAverageRunTime = right.windowAverageRunTime or 0
        if leftWindowAverageRunTime == rightWindowAverageRunTime then
            local leftWindowMaxRunTime = left.windowMaxRunTime or 0
            local rightWindowMaxRunTime = right.windowMaxRunTime or 0
            if leftWindowMaxRunTime == rightWindowMaxRunTime then
                return (left.totalRunTime or 0) > (right.totalRunTime or 0)
            end
            return leftWindowMaxRunTime > rightWindowMaxRunTime
        end
        return leftWindowAverageRunTime > rightWindowAverageRunTime
    end)

    return snapshotThreads
end

-- ---------------------------------------------------------------------------
-- Public module
-- ---------------------------------------------------------------------------
local performance = {}

performance.colors = textColor

---Optional callback invoked every time a new snapshot is built.
---Signature: function(snapshotThreads: table)
---On the server set this to `performance.printSnapshot` to log each snapshot.
performance.onSnapshotRefresh = nil

local function refreshScriptSnapshot(currentThreads)
    profilerState.snapshot.threads = buildSnapshotThreads(currentThreads)
    profilerState.snapshot.period = profilerState.snapshot.period + 1
    profilerState.snapshot.ticks = 0
    if performance.onSnapshotRefresh then
        performance.onSnapshotRefresh(profilerState.snapshot.threads)
    end
end

local function tickSnapshotWindow(currentThreads)
    profilerState.snapshot.ticks = profilerState.snapshot.ticks + 1
    if profilerState.snapshot.period == 0 or profilerState.snapshot.ticks >= profileWindowTicks then
        refreshScriptSnapshot(currentThreads)
    end
end

---Update rolling windows and advance the snapshot window for one game tick.
---Pass `elapsedTime` (seconds) to record the tick wall-clock time in DebugTimes.
---Called by the client's Balltze tick subscriber and by the server's OnTick.
---@param elapsedTime? number
function performance.tick(elapsedTime)
    if elapsedTime then
        DebugTimes.tickTime = elapsedTime
    end
    local currentThreads = getCurrentScriptThreads()
    updateThreadWindows(currentThreads)
    tickSnapshotWindow(currentThreads)
end

---Return the current frozen snapshot table.
---@return {threads: table[], ticks: number, period: number}
function performance.getSnapshot()
    return profilerState.snapshot
end

---Return the display name for a script thread.
---@param scriptThread table
---@return string
function performance.getThreadName(scriptThread)
    return getScriptThreadName(scriptThread)
end

---Print the current (or supplied) snapshot using draw_text when available,
---falling back to terminal output on runtimes without Chimera compatibility.
---@param snapshotThreads? table[]
function performance.printSnapshot(snapshotThreads)
    snapshotThreads = snapshotThreads or profilerState.snapshot.threads
    local drawText = function(text)
        engine.terminal.print("{}", text)
    end
    drawText(string.format("[Profiler] scriptThreads: %d | window: %d ticks | snapshot #%d",
                           #snapshotThreads, profileWindowTicks, profilerState.snapshot.period))
    local slowest = snapshotThreads[1]
    if slowest then
        drawText(string.format("  slowest(avg): %s | %.3f ms", getScriptThreadName(slowest),
                               (slowest.windowAverageRunTime or 0) * 1000))
    end
    for _, t in ipairs(snapshotThreads) do
        drawText(string.format(
                     "  %s | type %s | wAvg %.3f ms | wMax %.3f ms | last %.3f ms | total %.3f ms | runs %d",
                     getScriptThreadName(t), tostring(t.type or "unknown"),
                     (t.windowAverageRunTime or 0) * 1000, (t.windowMaxRunTime or 0) * 1000,
                     (t.lastRunTime or 0) * 1000, (t.totalRunTime or 0) * 1000, t.runCount or 0))
    end
end

-- ---------------------------------------------------------------------------
-- Client-only rendering: frame listeners draw the HUD overlay.
-- On the server the frame channel is a no-op shim (subscribers are never called),
-- so the client-specific API calls inside are unreachable and safe.

local function drawLuaMemory()
    if DebugLuaMemory then
        beginHudChannelFrame("lua-memory")
        local font = "smaller"
        local align = "center"
        local drawText = function(...)
            drawTextCompat("lua-memory", ...)
        end
        local bounds = {left = 0, top = 400, right = 640, bottom = 480}
        local memory = collectgarbage("count")
        local sizeInMb = memory / 1024
        local text = string.format("Helljumper Lua Memory %.4f MB", sizeInMb)
        drawText(text, bounds.left, bounds.top, bounds.right, bounds.bottom, font, align,
                 table.unpack(performance.colors.default))
        endHudChannelFrame("lua-memory")
        local connectionType = engine.game.getGameConnectionType()
        if connectionType == "networkServer" then
            -- Return one line above to prevent the text from being overwritten by the default SAPP memory usage line
            io.write("\27[1A")
        end
    else
        clearHudChannel("lua-memory")
    end
end

local function drawProfiler()
    if DebugPerformance then
        beginHudChannelFrame("performance")
        local align = "left"
        local drawText = function(...)
            drawTextCompat("performance", ...)
        end
        local scriptStatus = profilerState.snapshot.threads
        local startTime

        -- Measure performance from here
        if DebugPerformance then
            startTime = os.clock()
        end

        -- Prevent drawing info in menus or when console is open
        if console_is_open() then
            return
        end

        local rootWidget = engine.uiWidget.getActiveWidget()
        local isPlayerOnMenu = rootWidget ~= nil
        if isPlayerOnMenu then
            return
        end
        -- local player = get_dynamic_player()
        -- if not player then
        --    return
        -- end

        local endTime = os.clock()
        local elapsedTime = endTime - startTime
        DebugTimes.frameTime = elapsedTime

        if #scriptStatus == 0 and profilerState.snapshot.period == 0 then
            local currentThreads = getCurrentScriptThreads()
            updateThreadWindows(currentThreads)
            refreshScriptSnapshot(currentThreads)
            scriptStatus = profilerState.snapshot.threads
        end

        local yOffset = 0
        for _, key in ipairs({"tickTime", "frameTime"}) do
            local time = DebugTimes[key]
            if time then
                drawText(string.format("%s: %.6f s", key, time), bounds.left,
                         bounds.top + yOffset, bounds.right, bounds.bottom, "smaller", align,
                         table.unpack(performance.colors.default))
            end
            yOffset = yOffset + 20
        end

        if scriptStatus then
            drawText(string.format(
                         "scriptThreads: %d | window: %d ticks | snapshot #%d | next in %d",
                         #scriptStatus, profileWindowTicks, profilerState.snapshot.period,
                         math.max(profileWindowTicks - profilerState.snapshot.ticks, 0)), bounds.left,
                     bounds.top + yOffset, bounds.right, bounds.bottom, "smaller", align,
                     table.unpack(performance.colors.info))
            yOffset = yOffset + 20

            local slowestThread = scriptStatus[1]
            if slowestThread then
                drawText(string.format("slowest(avg): %s | %.3f ms",
                                       getScriptThreadName(slowestThread),
                                       (slowestThread.windowAverageRunTime or 0) * 1000), bounds.left,
                         bounds.top + yOffset, bounds.right, bounds.bottom, "smaller", align,
                         table.unpack(performance.colors.warning))
                yOffset = yOffset + 20
            end

            for index = 1, #scriptStatus do
                local scriptThread = scriptStatus[index]
                if not (scriptThread.referenceFile and scriptThread.referenceFile:find("script.lua")) then
                    drawText(string.format(
                                 "%s | type %s | wAvg %.3f ms | wMax %.3f ms | last %.3f ms | total %.3f ms | runs %d",
                                 getScriptThreadName(scriptThread),
                                 tostring(scriptThread.type or "unknown"),
                                 (scriptThread.windowAverageRunTime or 0) * 1000,
                                 (scriptThread.windowMaxRunTime or 0) * 1000,
                                 (scriptThread.lastRunTime or 0) * 1000,
                                 (scriptThread.totalRunTime or 0) * 1000, scriptThread.runCount or 0),
                             bounds.left, bounds.top + yOffset, bounds.right, bounds.bottom,
                             "smaller", align, table.unpack(performance.colors.default))
                    yOffset = yOffset + 20
                end
            end
        end
        endHudChannelFrame("performance")
    else
        clearHudChannel("performance")
    end
end

--- Draw both overlays for this frame
---
--- Called from the one frame listener the plugin keeps, rather than each overlay subscribing for
--- itself. Balltze holds a single listener per event name, so the two subscriptions this module used
--- to make left only the second one running, and whatever else in the project subscribed to "frame"
--- after them took that one down too.
---
--- The two are called separately and not merged into one body because the profiler bails early when
--- the console or a menu is open, and merging would have that bail take the memory readout with it.
function performance.frame()
    drawLuaMemory()
    drawProfiler()
end

return performance
