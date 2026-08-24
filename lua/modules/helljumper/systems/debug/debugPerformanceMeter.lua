-- Lua libraries
local balltze = Balltze
local engine = Engine

--------------------------------------------------------------------------------------------------
-- What the tick and the frame are actually costing, said out loud once a second.
--
-- Built to answer one question: a map that runs badly for its first seconds and then comes right on
-- its own, with nothing done to it. That shape is the whole point of the reading, so every line
-- carries the second it was taken at. A cost that is there at four seconds and gone at twelve is
-- something warming up, and the only thing that tells the two apart is watching the number fall.
--
-- Everything is measured over a window rather than per call. os.clock counts in whole milliseconds
-- on this platform, so a system that takes three tenths of one reads as nothing most of the times it
-- is measured and as a whole millisecond the rest; summed over the hundreds of calls a window holds,
-- that quantising averages back out to the real figure. A single call's reading would be noise.
--------------------------------------------------------------------------------------------------

local performanceMeter = {}

local clock = os.clock
local format = string.format

--------------------------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------------------------

-- Whether the reading is drawn, logged, or both. The log is what a comparison between two maps is
-- actually made of: it keeps every window, so the fall can be read back afterwards instead of being
-- caught on screen as it happens. Sent as warnings rather than as debug on purpose, since the
-- plugin's debug output does not reach ringworld's debug.txt.
local isShownOnScreen = true
local isLogged = false

-- How long a reading covers. Shorter says more about the first moments of a map, which is where the
-- problem being chased lives, and is noisier for it.
local windowSeconds = 1

-- Whether the memory figure is the raw count or the floor left standing after a full collection.
--
-- The raw count is what this line has always said. It climbs the whole time on any healthy program,
-- because it is everything allocated since the last sweep rather than everything still held, so read
-- as a leak it lies: a number that only goes up between collections is the ordinary sawtooth.
--
-- The floor is what actually answers a leak. It is the count taken immediately after
-- collectgarbage("collect"), which garbage cannot survive, so whatever is left is genuinely still
-- referenced by something. A floor that holds steady while the count saws up and down means nothing
-- is leaking; a floor that climbs reading after reading means something is keeping references, and
-- the rate beside it says how fast.
--
-- Off by default, because it costs a full collection every window and this is a performance meter:
-- that collection lands inside the very frame and tick figures printed beside it. Switch it on to
-- chase a leak, switch it off before believing any timing.
local isMemoryFloorMeasured = false

-- Where the reading is drawn, measured inward from the top left in the 640x480 space the HUD is laid
-- out in, and how far apart its lines sit. The reading is as tall as it has systems to report on, so
-- what is set here is where its first line goes and the rest follow downward from it.
local screenPosition = {x = 24, y = 90}
local lineHeight = 14
local textColor = {a = 1.0, r = 1.0, g = 0.85, b = 0.4}

--------------------------------------------------------------------------------------------------
-- End of configuration
--------------------------------------------------------------------------------------------------

-- Whether a measurement is taken at all. Neither switch on means every window's reading is built and
-- thrown away, so the sections are not timed either: the one place this module reaches into code
-- that ships to players is run(), and with nothing being read it gets out of the way.
local isMeasuring = isShownOnScreen or isLogged

-- Whether there is a screen to draw on at all. The same plugin loads on the dedicated server, where
-- there is no interface to add a text to; the counting and the log still mean something there, so
-- the reading falls back to the log rather than the module refusing to load.
local canDrawText = engine.interface ~= nil and engine.interface.addText ~= nil

-- What has been measured since the last reading went out. The sections are held twice over, by name
-- for the lookup a measurement does and in order for the reading to walk, so the lines come out in
-- the order the systems run in rather than in whatever order a hash hands them back.
local sections = {}
---@type {name: string, seconds: number, calls: integer}[]
local sectionOrder = {}
local frameCount = 0
local tickCount = 0
local worstFrameMilliseconds = 0

-- The floor the first reading found, which every later one is measured against. Held for the whole
-- run rather than per window, since what it exists to show is a slope.
local firstFloorKilobytes = nil

-- When the module first ran, which is near enough when the map came up. Every reading is stamped
-- with how long ago that was, since a cost that goes away on its own only means anything read
-- against the clock.
local startedAtSeconds = clock()

--- The running total for a section, started if this is the first time it is named
---@param name string
---@return {name: string, seconds: number, calls: integer}
local function getSection(name)
    local section = sections[name]
    if not section then
        section = {name = name, seconds = 0, calls = 0}
        sections[name] = section
        sectionOrder[#sectionOrder + 1] = section
    end
    return section
end

--- Run a piece of work and put what it cost on the running total under a name
---
--- Whatever it does, it does untouched: nothing here catches what it raises or looks at what it
--- hands back, so wrapping a call changes what that call costs and nothing else about it.
---
--- With nothing to publish a reading to, the measuring is dropped and the work is called straight
--- through. This is what keeps the wrapping in the gameplay path honest: a call that ships to
--- players with both switches off costs one boolean test on top of the call it was already making,
--- rather than the pair of clock reads and the table lookup a real measurement takes.
---@param name string
---@param work fun()
function performanceMeter.run(name, work)
    if not isMeasuring then
        work()
        return
    end
    local section = getSection(name)
    local startedAt = clock()
    work()
    section.seconds = section.seconds + (clock() - startedAt)
    section.calls = section.calls + 1
end

-- The clock a frame is measured against, reset on each one, so what it hands back is how long the
-- frame before it took. The worst of them across a window says more than the average does: a map
-- that mostly runs fine and hitches is the same average as one that is evenly slow, and they are
-- not the same problem.
---@type BalltzeTimestamp|nil
local frameTimestamp = nil
local lastFrameMilliseconds = 0

--- Count a frame
---
--- Called from main's own frame callback rather than from a listener of this module's own. That is
--- not a tidiness matter: put up from inside the tick, the way the other modules here do it, this
--- module's frame listener never fired once across a hundred windows of readings, and a counter that
--- quietly counts nothing reads exactly like a game running at no frames a second. Hung off a
--- callback already known to run, there is nothing left to fail without saying so.
function performanceMeter.frame()
    frameCount = frameCount + 1
    if not frameTimestamp then
        frameTimestamp = balltze.createTimestamp()
        return
    end
    -- Read as a running total and never reset, for the same reason the ADS movement is: the clock
    -- hands back whole milliseconds, and one reset every frame drops the fraction it had not counted
    -- yet, which above a thousand frames a second is the whole frame. What is left is a resolution
    -- of one millisecond on a single frame, which costs nothing here: the figure being kept is the
    -- worst frame of the window, and a frame worth reporting is never a fraction of a millisecond.
    local totalMilliseconds = frameTimestamp:getElapsedMilliseconds()
    local frameMilliseconds = totalMilliseconds - lastFrameMilliseconds
    lastFrameMilliseconds = totalMilliseconds
    if frameMilliseconds > worstFrameMilliseconds then
        worstFrameMilliseconds = frameMilliseconds
    end
end

-- The lines on screen, one per line of the reading, kept so a new reading rewrites them instead of
-- adding a second set over the top of the first.
---@type InterfaceText[]
local shownLines = {}

-- Whether a failure to draw has already been reported, so it is reported once and not every window.
local hasWarnedAboutDrawing = false

--- Take a line off the screen, if it is on it
---@param index integer
local function removeShownLine(index)
    local shownLine = shownLines[index]
    if shownLine and shownLine.remove then
        shownLine:remove()
    end
    shownLines[index] = nil
end

--- Put a reading on screen, over whatever the last one left there
---@param lines string[]
local function showLines(lines)
    for index = 1, #lines do
        local shownLine = shownLines[index]
        if shownLine and shownLine.setText then
            shownLine:setText(lines[index])
        else
            -- Either nothing is on this line yet or this build cannot rewrite a string, so the line
            -- is added afresh. Options are fixed at creation, which is why they are built here
            -- rather than kept: it is once a second, and a font handle belongs to the loaded map.
            removeShownLine(index)
            shownLines[index] = engine.interface.addText(lines[index], screenPosition.x,
                                                        screenPosition.y + (index - 1) * lineHeight, {
                color = textColor,
                -- No font named on purpose. addText falls back to the game's own terminal font when
                -- handed nothing, and this has to read the same on a map that carries the project's
                -- fonts and on one that does not, which is exactly the comparison being made.
                layer = "ui",
                anchor = "topLeft",
                justification = "left"
            })
        end
    end
    for index = #shownLines, #lines + 1, -1 do
        removeShownLine(index)
    end
end

--- One named figure of the reading, padded so the figures line up down a column of their own
---
--- The padding is only ever near enough: the font this is drawn in is proportional, so a space is
--- not the width of a digit and the column comes out ragged rather than square. Close enough that
--- the eye follows it, which is all the padding is for.
---@param label string
---@param value string
---@return string
local function readingLine(label, value)
    return format("%-14s %s", label .. ":", value)
end

--- Say what the window that just closed cost, and start the next one from nothing
---@param windowMilliseconds integer @what the window actually covered, which is never quite the
--- window asked for: it closes on the first tick past it
local function publishReading(windowMilliseconds)
    local elapsedSeconds = windowMilliseconds / 1000
    -- One figure to a line, each named. The four of them used to share a line, which fits a log
    -- better than a screen: read down a column the eye finds the figure it is after by its place,
    -- and a figure that moves is easier to catch when nothing beside it moves with it.
    --
    -- Worst frame is the longest single frame of the window, not an average of them, and it is the
    -- one here that says whether the game runs evenly. A map that mostly runs fine and hitches has
    -- the same fps as one that is evenly slow.
    local lines = {
        readingLine("Time Elapsed", format("%.1fs", clock() - startedAtSeconds)),
        readingLine("FPS", format("%.1f", frameCount / elapsedSeconds)),
        readingLine("Worst Frame", format("%dms", worstFrameMilliseconds)),
        readingLine("TPS", format("%.1f", tickCount / elapsedSeconds))
    }
    -- Held apart from the per system lines and printed before them, since it is the one figure that
    -- answers whether any of this is worth reading further: lua taking a fraction of a percent of
    -- the wall clock while the frames crawl means the frames are crawling for some other reason.
    --
    -- It only counts what was handed to run(), so it is a floor on what lua costs rather than the
    -- whole of it: a system nobody wrapped is a system this line does not know about.
    local totalSeconds = 0
    for index = 1, #sectionOrder do
        totalSeconds = totalSeconds + sectionOrder[index].seconds
    end
    lines[#lines + 1] = readingLine("Lua Total", format("%.2f%% of wall time",
                                                       totalSeconds / elapsedSeconds * 100))
    -- What lua is holding, in the same column as everything else rather than as a line of its own
    -- somewhere else on the screen.
    --
    -- It was read every frame when it was drawn separately; here it is read once a window, with the
    -- rest of the reading. Nothing is lost by that, since what a memory figure is watched for is the
    -- drift across seconds, not what it did between two frames.
    if DebugMode and DebugLuaMemory then
        -- collectgarbage counts in kilobytes.
        lines[#lines + 1] = readingLine("Lua Memory",
                                        format("%.4f MB", collectgarbage("count") / 1024))
        if isMemoryFloorMeasured then
            collectgarbage("collect")
            local floorKilobytes = collectgarbage("count")
            firstFloorKilobytes = firstFloorKilobytes or floorKilobytes
            -- Measured against the whole run rather than against the window before it, because a
            -- leak is a slope and one window is too short to see a slope in. The rate here is what a
            -- bug report is made of; the figure beside it is what the slope has cost so far.
            local sinceStartSeconds = clock() - startedAtSeconds
            local driftRate = 0
            if sinceStartSeconds > 0 then
                driftRate = (floorKilobytes - firstFloorKilobytes) / sinceStartSeconds
            end
            lines[#lines + 1] = readingLine("Lua Floor", format("%.4f MB   %+.2f KB/s",
                                                                floorKilobytes / 1024, driftRate))
        end
    end
    -- The breakdown of the total above, indented under it so the two are told apart at a glance:
    -- the named figures are about the game, the indented ones are about one system each.
    for index = 1, #sectionOrder do
        local section = sectionOrder[index]
        if section.calls > 0 then
            lines[#lines + 1] = format("  %-18s %7.3fms  x%d", section.name,
                                       section.seconds * 1000 / section.calls, section.calls)
        end
    end

    if isShownOnScreen and canDrawText then
        -- Guarded, and the failure said out loud once. A drawing call that raises inside the tick
        -- takes the listener down with it and nothing says so, which reads from the outside exactly
        -- like a game that suddenly stopped costing anything. Said once rather than every window,
        -- because a fault that repeats thirty times a second buries the line that explains it.
        local wasShown, failure = pcall(showLines, lines)
        if not wasShown and not hasWarnedAboutDrawing then
            hasWarnedAboutDrawing = true
            balltze.logger.error("performance: the reading could not be drawn: {}", failure)
        end
    else
        for index = #shownLines, 1, -1 do
            removeShownLine(index)
        end
    end
    if isLogged then
        for index = 1, #lines do
            balltze.logger.warning("performance: {}", lines[index])
        end
    end

    frameCount = 0
    tickCount = 0
    worstFrameMilliseconds = 0
    for index = 1, #sectionOrder do
        local section = sectionOrder[index]
        section.seconds = 0
        section.calls = 0
    end
end

-- The clock the window is measured against. Reset when a window closes rather than read against
-- some start, so a reading always covers exactly the stretch it reports on.
---@type BalltzeTimestamp|nil
local windowTimestamp = nil

--- Count a tick, and close the window if it has run long enough. Meant to be called first thing on
--- the tick, before the systems being measured run: what they cost on this tick then lands in the
--- window that is opening rather than in the one just reported.
function performanceMeter.tick()
    tickCount = tickCount + 1
    if not windowTimestamp then
        windowTimestamp = balltze.createTimestamp()
        return
    end
    local windowMilliseconds = windowTimestamp:getElapsedMilliseconds()
    if windowMilliseconds < windowSeconds * 1000 then
        return
    end
    windowTimestamp:reset()
    publishReading(windowMilliseconds)
end

--- Time something that happens once, and say so. For the work a map does on its way up, which never
--- reaches a window because it is over before the first one opens.
---
--- Only the first thing the work hands back comes back out of here, which is all any of the callers
--- want; work returning more than one wants calling directly.
---@generic T
---@param name string
---@param work fun(): T
---@return T
function performanceMeter.time(name, work)
    local startedAt = clock()
    local result = work()
    balltze.logger.warning("performance: {} took {} ms", name,
                           format("%.1f", (clock() - startedAt) * 1000))
    return result
end

--- Take everything this module put on screen back off
---
--- A text outlives the map it was added on, so anything still up when the plugin unloads is left
--- drawing over whatever comes next.
function performanceMeter.unload()
    for index = #shownLines, 1, -1 do
        removeShownLine(index)
    end
end

-- Said once, at load, so that silence afterwards means something specific. Without it a console with
-- no readings in it is equally consistent with the module never having been required and with the
-- tick never reaching it, and those two want opposite things done about them. This module has been
-- on the wrong side of that before: its frame listener once went a hundred windows without firing,
-- and a counter that quietly counts nothing reads exactly like a game running at no frames a second.
if isMeasuring then
    balltze.logger.warning("performance: meter is up, first reading due in {}s", windowSeconds)
end

return performanceMeter
